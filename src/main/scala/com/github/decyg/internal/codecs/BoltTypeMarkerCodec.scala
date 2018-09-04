package com.github.decyg.internal.codecs

import com.github.decyg.internal.codecs.BoltTypeMarker.WithRange
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.bits._

object BoltTypeMarker extends Enumeration {
  type BoltTypeMarker = WithRange

  private implicit def singleAsTup(bv: BitVector): (BitVector, BitVector) = (bv, bv)
  case class WithRange(bvRanges: (BitVector, BitVector)*) extends Val

  val NULL = WithRange(BitVector(hex"C0"))
  val BOOLEAN = WithRange(BitVector(hex"C2") -> BitVector(hex"C3"))
  val INTEGER = WithRange(BitVector(hex"00") -> BitVector(hex"7F"), BitVector(hex"F0") -> BitVector(hex"FF"), BitVector(hex"C8") -> BitVector(hex"CB"))
  val FLOAT = WithRange(BitVector(hex"C1"))
  val STRING = WithRange(BitVector(hex"80") -> BitVector(hex"8F"), BitVector(hex"D0") -> BitVector(hex"D2"))
  val LIST = WithRange(BitVector(hex"90") -> BitVector(hex"9F"), BitVector(hex"D4") -> BitVector(hex"D6"))
  val MAP = WithRange(BitVector(hex"A0") -> BitVector(hex"AF"), BitVector(hex"D8") -> BitVector(hex"DA"))
  val STRUCTURE = WithRange(BitVector(hex"B0") -> BitVector(hex"BF"), BitVector(hex"DC") -> BitVector(hex"DD"))

}

object BoltTypeMarkerCodec extends Codec[BoltTypeMarker.WithRange] {

  override def sizeBound: SizeBound = SizeBound.exact(8)

  override def encode(value: BoltTypeMarker.WithRange): Attempt[BitVector] = { Attempt.successful(BitVector(hex"")) }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltTypeMarker.WithRange]] = {
    val lv = bits.take(8).toLong()

    BoltTypeMarker.values.find {
      v =>
        v.asInstanceOf[WithRange].bvRanges.exists {
          a =>
            lv >= a._1.toLong() && lv <= a._2.toLong()
        }
    } match {
      case Some(btmv) =>
        Attempt.successful(DecodeResult(btmv.asInstanceOf[WithRange], bits)) // don't consume any bits
      case None =>
        Attempt.failure(Err("Couldn't find a corresponding mapping "))

    }
  }
}
