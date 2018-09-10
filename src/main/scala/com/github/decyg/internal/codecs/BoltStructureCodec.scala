package com.github.decyg.internal.codecs

import com.github.decyg.internal
import com.github.decyg.internal._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.{uint16, uint4, uint8}
import scodec.bits._
import shapeless.Typeable

object BoltStructureCodec extends Codec[BoltStructure] {

  // datatype markers
  val S = hex"B".bits.takeRight(4)
  val S8 = hex"DC".bits
  val S16 = hex"DD".bits

  // signature markers
  val STRUCTURE = hex"01".bits
  val NODE = hex"4E".bits
  val RELATIONSHIP = hex"52".bits
  val PATH = hex"50".bits
  val UNBOUNDRELATIONSHIP = hex"72".bits

  override def sizeBound: SizeBound = SizeBound.atLeast(8 * 2) // unbound but has to at least be two bytes (low order byte size and structure sig)

  override def encode(value: BoltStructure): Attempt[BitVector] = {

    val cbt = Codec[BoltType]
    value match {
      case BoltNode(nodeIdentity, labels, properties) => // three fields, 4E
        for{
          ni <- cbt.encode(BoltInteger(nodeIdentity))
          lb <- cbt.encode(BoltList(labels.map(BoltString)))
          prop <- cbt.encode(BoltMap(properties.map(e => BoltString(e._1) -> e._2)))
        } yield {
          hex"B3".bits ++ NODE ++ ni ++ lb ++ prop
        }
      case BoltRelationship(relIdentity, startNodeIdentity, endNodeIdentity, t, properties) =>
        for{
          ri <- cbt.encode(BoltInteger(relIdentity))
          sni <- cbt.encode(BoltInteger(startNodeIdentity))
          eni <- cbt.encode(BoltInteger(endNodeIdentity))
          tp <- cbt.encode(BoltString(t))
          prop <- cbt.encode(BoltMap(properties.map(e => BoltString(e._1) -> e._2)))
        } yield {
          hex"B5".bits ++ RELATIONSHIP ++ ri ++ sni ++ eni ++ tp ++ prop
        }
      case BoltPath(nodes, relationships, sequence) =>
        for{
          no <- cbt.encode(BoltList(nodes))
          rel <- cbt.encode(BoltList(relationships))
          seq <- cbt.encode(BoltList(sequence.map(BoltInteger)))
        } yield {
          hex"B3".bits ++ PATH ++ no ++ rel ++ seq
        }
      case BoltUnboundRelationship(relIdentity, t, properties) =>
        for{
          rel <- cbt.encode(BoltInteger(relIdentity))
          tp <- cbt.encode(BoltString(t))
          prop <- cbt.encode(BoltMap(properties.map(e => BoltString(e._1) -> e._2)))
        } yield {
          hex"B3".bits ++ UNBOUNDRELATIONSHIP ++ rel ++ tp ++ prop
        }
      case BoltStructureContainer(l) =>
        val listLen = l.size

        val asValueList = l.foldLeft(Attempt.successful(BitVector(hex""))){
          (cur, bt) =>
            cur.flatMap{
              bvL =>
                Codec[BoltType].encode(bt).map(bv => bvL ++ bv)
            }
        }

        if(listLen <= 15L){
          uint4.encode(listLen).flatMap{
            listLenBV =>
              asValueList.map{
                bv =>
                  (hex"B0".bits | listLenBV.padLeft(8)) ++ STRUCTURE ++ bv
              }
          }
        } else if(listLen <= 255L){
          uint8.encode(listLen).flatMap{
            listLenBV =>
              asValueList.map{
                bv => S8 ++ listLenBV.padLeft(8) ++ STRUCTURE ++ bv
              }
          }
        } else if(listLen <= 65535L){
          uint16.encode(listLen).flatMap{
            listLenBV =>
              asValueList.map{
                bv => S16 ++ listLenBV.padLeft(16) ++ STRUCTURE ++ bv
              }
          }
        } else {
          Attempt.failure(Err("Can't encode a BoltStructure longer than 65535 individual fields"))
        }
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltStructure]] = {
    val (marker, res) = bits.splitAt(8)
    val (markerHigh, markerLow) = marker.splitAt(4)

    import BoltType._

    val resDecode = (lstDecode: BitVector, lstLen: Long) => {
      // the first 8 bits of the rest of it are still signature bits that actually determine the type of the structure
      val (signatureBV, body) = lstDecode.splitAt(8)

      // yes, i am aware runtime validation is B A D, but i couldn't figure out how to do it with parameterised types so
      signatureBV match {
        case NODE =>

          (Codec[BoltInteger] ~ typedListCodec(Typeable[BoltString]) ~ typedMapCodec(Typeable[BoltString], Typeable[BoltType]))
            .xmap[BoltNode](
              { case ((a, b), c) => BoltNode(a.i, b.l.map(_.asInstanceOf[BoltString].s), c.m.map(e => e._1.asInstanceOf[BoltString].s -> e._2)) },
              { bn => ((BoltInteger(bn.nodeIdentity), BoltList(bn.labels.map(BoltString))), BoltMap(bn.properties.map(e => BoltString(e._1) -> e._2))) }
            ).decode(body)

        case RELATIONSHIP =>

          (Codec[BoltInteger] ~ Codec[BoltInteger] ~ Codec[BoltInteger] ~ Codec[BoltString] ~ typedMapCodec(Typeable[BoltString], Typeable[BoltType]))
            .xmap[BoltRelationship](
            { case ((((a, b), c), d), e) => BoltRelationship(a.i, b.i, c.i, d.s, e.m.map(e => e._1.asInstanceOf[BoltString].s -> e._2)) },
            { br => ((((
              BoltInteger(br.relIdentity),
              BoltInteger(br.startNodeIdentity)),
              BoltInteger(br.endNodeIdentity)),
              BoltString(br.`type`)),
              BoltMap(br.properties.map(e => BoltString(e._1) -> e._2)))
            }
          ).decode(body)

        case PATH =>

          (typedListCodec(Typeable[BoltNode]) ~ typedListCodec(Typeable[BoltUnboundRelationship]) ~ typedListCodec(Typeable[BoltInteger]))
            .xmap[BoltPath](
            { case ((a, b), c) => BoltPath(a.l.map(_.asInstanceOf[BoltNode]), b.l.map(_.asInstanceOf[BoltUnboundRelationship]), c.l.map(_.asInstanceOf[BoltInteger].i)) },
            { bp => ((BoltList(bp.nodes), BoltList(bp.relationships)), BoltList(bp.sequence.map(BoltInteger))) }
          ).decode(body)

        case UNBOUNDRELATIONSHIP =>

          (Codec[BoltInteger] ~ Codec[BoltString] ~ typedMapCodec(Typeable[BoltString], Typeable[BoltType]))
            .xmap[BoltUnboundRelationship](
            { case ((a, b), c) => BoltUnboundRelationship(a.i, b.s, c.m.map(e => e._1.asInstanceOf[BoltString].s -> e._2)) },
            { bp => ((BoltInteger(bp.relIdentity), BoltString(bp.`type`)), BoltMap(bp.properties.map(e => BoltString(e._1) -> e._2))) }
          ).decode(body)

        case _ =>

          (0 until lstLen.toInt).foldLeft(Attempt.successful((Seq.empty[BoltType], body))){
            (lstAtt, i) =>
              lstAtt.flatMap{
                lst =>
                  Codec[BoltType].decode(lst._2).map{
                    resLst =>
                      (lst._1 ++ Seq(resLst.value), resLst.remainder)
                  }
              }
          }.map(a => DecodeResult(BoltStructureContainer(a._1), a._2))

      }
    }

    if(markerHigh == S){ // using just the high part as the low is variable, the low determines the datasize
      uint4.decode(markerLow).flatMap{ n => resDecode(res, n.value) }
    } else if(marker == S8){
      val (len, listBody) = res.splitAt(8)
      uint8.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else if(marker == S16){
      val (len, listBody) = res.splitAt(16)
      uint16.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else {
      Attempt.failure(Err("Not a valid string representation of a BoltStructure"))
    }
  }
}
