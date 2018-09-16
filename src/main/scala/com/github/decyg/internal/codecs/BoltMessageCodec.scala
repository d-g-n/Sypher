package com.github.decyg.internal.codecs

import com.github.decyg.internal._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.bits._
import scodec.codecs.{uint16, uint32, uint4, uint8}

object BoltMessageCodec extends Codec[BoltMessage] {

  val INIT = hex"01".bits
  val RUN = hex"10".bits
  val DISCARD_ALL = hex"2F".bits
  val PULL_ALL = hex"3F".bits
  val ACK_FAILURE = hex"0E".bits
  val RESET = hex"0F".bits
  val RECORD = hex"71".bits
  val SUCCESS = hex"70".bits
  val FAILURE = hex"7F".bits
  val IGNORED = hex"7E".bits


  override def sizeBound: SizeBound = SizeBound.atLeast(8 * 2) // unbound but has to at least be two bytes (low order byte size and structure sig)


  // A message is just a more refined version of a structure object, similar to the actual boltstructure objects
  override def encode(value: BoltMessage): Attempt[BitVector] = {
    val bsc = Codec[BoltStructure]
    value match {
      case BoltInit(clientName, authToken) =>
        bsc.encode(BoltStructureContainer(INIT, Seq(BoltString(clientName), BoltMap(authToken.map(e => BoltString(e._1) -> e._2)))))
      case BoltRun(statement, parameters) =>
        bsc.encode(BoltStructureContainer(RUN, Seq(BoltString(statement), BoltMap(parameters.map(e => BoltString(e._1) -> e._2)))))
      case BoltDiscardAll() =>
        bsc.encode(BoltStructureContainer(DISCARD_ALL))
      case BoltPullAll() =>
        bsc.encode(BoltStructureContainer(PULL_ALL))
      case BoltAckFailure() =>
        bsc.encode(BoltStructureContainer(ACK_FAILURE))
      case BoltReset() =>
        bsc.encode(BoltStructureContainer(RESET))
      case BoltRecord(fields) =>
        bsc.encode(BoltStructureContainer(RECORD, fields))
      case BoltSuccess(metadata) =>
        bsc.encode(BoltStructureContainer(SUCCESS, Seq(BoltMap(metadata.map(e => BoltString(e._1) -> e._2)))))
      case BoltFailure(metadata) =>
        bsc.encode(BoltStructureContainer(FAILURE, Seq(BoltMap(metadata.map(e => BoltString(e._1) -> e._2)))))
      case BoltIgnored() =>
        bsc.encode(BoltStructureContainer(IGNORED))
      case _ =>
        Attempt.failure(Err("Could not derive type of bolt message value"))

    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltMessage]] = {

    val (marker, body) = bits.splitAt(8)

    val signaturebits = if(marker.startsWith(hex"B".bits.takeRight(4))){ // marker is the 8 bits after the first 8
      Attempt.successful(body.take(8))
    } else if(marker == hex"DC".bits) {
      uint8.decode(body).map(dr => dr.remainder.take(8))
    } else if(marker == hex"DD".bits) { // DD
      uint16.decode(body).map(dr => dr.remainder.take(8))
    } else {
      Attempt.failure(Err("Could not resolve marker bits for bolt message"))
    }

    for{
      sig <- signaturebits
      decRes <- Codec[BoltStructure].decode(bits)
    } yield {
      decRes.map{
        bs =>
          val bsc = bs.asInstanceOf[BoltStructureContainer] // this should be a safe cast as it will always fall through
          val bscLS = bsc.l.size

          sig match {
            case INIT if bscLS == 2 =>
              BoltInit(bsc.l(0).asInstanceOf[BoltString].s, bsc.l(1).asInstanceOf[BoltMap].m.map(e => e._1.asInstanceOf[BoltString].s -> e._2))
            case RUN if bscLS == 2 =>
              BoltRun(bsc.l(0).asInstanceOf[BoltString].s, bsc.l(1).asInstanceOf[BoltMap].m.map(e => e._1.asInstanceOf[BoltString].s -> e._2))
            case DISCARD_ALL if bscLS == 0 =>
              BoltDiscardAll()
            case PULL_ALL if bscLS == 0 =>
              BoltPullAll()
            case ACK_FAILURE if bscLS == 0 =>
              BoltAckFailure()
            case RESET if bscLS == 0 =>
              BoltReset()
            case RECORD =>
              BoltRecord(bsc.l)
            case SUCCESS if bscLS == 1 =>
              BoltSuccess(bsc.l(0).asInstanceOf[BoltMap].m.map(e => e._1.asInstanceOf[BoltString].s -> e._2))
            case FAILURE if bscLS == 1 =>
              BoltFailure(bsc.l(0).asInstanceOf[BoltMap].m.map(e => e._1.asInstanceOf[BoltString].s -> e._2))
            case IGNORED =>
              BoltIgnored()
            case _ =>
              bsc
          }
      }
    }
  }
}
