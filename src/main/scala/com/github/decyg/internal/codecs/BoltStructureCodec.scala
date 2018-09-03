package com.github.decyg.internal.codecs

import com.github.decyg.internal._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.{uint16, uint4, uint8}
import scodec.bits._

object BoltStructureCodec extends Codec[BoltStructure] {

  // datatype markers
  val S = hex"B".bits.takeRight(4)
  val S8 = hex"DC".bits
  val S16 = hex"DD".bits

  // signature markers
  val NODE = hex"4E".bits
  val RELATIONSHIP = hex"52".bits
  val PATH = hex"50".bits
  val UNBOUNDRELATIONSHIP = hex"72".bits


  override def sizeBound: SizeBound = SizeBound.atLeast(8 * 2) // unbound but has to at least be two bytes (low order byte size and structure sig)

  override def encode(value: BoltStructure): Attempt[BitVector] = {

    /*
    val listLen = value.l.size

    val asValueList = value.l.foldLeft(Attempt.successful(BitVector(hex""))){
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
              (hex"90".bits | listLenBV.padLeft(8)) ++ bv
          }
      }
    } else if(listLen <= 255L){
      uint8.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => S8 ++ listLenBV.padLeft(8) ++ bv
          }
      }
    } else if(listLen <= 65535L){
      uint16.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => S16 ++ listLenBV.padLeft(16) ++ bv
          }
      }
    } else {
      Attempt.failure(Err("Can't encode a BoltStructure longer than 65535 individual fields"))
    }

*/
    Attempt.failure(Err("stub"))
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltStructure]] = {
    val (marker, res) = bits.splitAt(8)
    val (markerHigh, markerLow) = marker.splitAt(4)

    val resDecode = (lstDecode: BitVector, lstLen: Long) => {
      // the first 8 bits of the rest of it are still
      val (signatureBV, body) = lstDecode.splitAt(8)

      // yes, i am aware runtime validation is B A D, but i couldn't figure out how to do it with parameterised types so
      signatureBV match {
        case NODE => // must be len 3, int, list<string>, map<string, value>
          (Codec[BoltInteger] ~ Codec[BoltList] ~ Codec[BoltMap]).decode(body).flatMap{
            res =>
              val ((a, b), c) = res.value
              val lstStrBool = b.l.forall(_.isInstanceOf[BoltString])
              val mapStrBool = c.m.forall(_._1.isInstanceOf[BoltString])

              if(lstStrBool && mapStrBool){
                Attempt.successful(
                  DecodeResult(
                    BoltNode(
                      a.i,
                      b.l.map(_.asInstanceOf[BoltString].s),
                      c.m.map(a => (a._1.asInstanceOf[BoltString].s, a._2))
                    ),
                    res.remainder
                  )
                )
              } else {
                Attempt.failure(Err("Could not parse a valid BoltNode type from input"))
              }
          }

        case RELATIONSHIP =>
          (Codec[BoltInteger] ~ Codec[BoltInteger] ~ Codec[BoltInteger] ~ Codec[BoltString] ~ Codec[BoltMap]).decode(body)
          Attempt.failure(Err("")) //TODO
        case PATH =>
          (Codec[BoltList] ~ Codec[BoltList] ~ Codec[BoltList]).decode(body)
          Attempt.failure(Err(""))//TODO
        case UNBOUNDRELATIONSHIP =>
          (Codec[BoltInteger] ~ Codec[BoltString] ~ Codec[BoltMap]).decode(body)
          Attempt.failure(Err(""))//TODO
        case _ =>
          (0 until lstLen.toInt).foldLeft(Attempt.successful((List.empty[BoltType], lstDecode))){
            (lstAtt, i) =>
              lstAtt.flatMap{
                lst =>
                  Codec[BoltType].decode(lst._2).map{
                    resLst =>
                      (resLst.value :: lst._1, resLst.remainder)
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
