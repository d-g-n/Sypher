package com.github.decyg.internal.codecs

import com.github.decyg.internal.BoltInteger
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.{int16, int32, int64, int8}
import scodec.bits._

object BoltIntegerCodec extends Codec[BoltInteger] {

  // datatype markers
  val I8 = hex"C8".bits
  val I16 = hex"C9".bits
  val I32 = hex"CA".bits
  val I64 = hex"CB".bits

  // bounds for types
  val TIR = (BigInt("-16"), BigInt("127"))
  val I8R = (BigInt("-128"), BigInt("-17"))
  val I16R = (BigInt("-32768"), BigInt("32767"))
  val I32R = (BigInt("-2147483648"), BigInt("2147483647"))
  val I64R = (BigInt("-9223372036854775808"), BigInt("9223372036854775807"))

  override def sizeBound: SizeBound = SizeBound.bounded(8, 8 * 9) // from just the marker types to full int_64 rep

  override def encode(value: BoltInteger): Attempt[BitVector] = {

    val bi = value.i
    val bv = BitVector(bi.toByteArray)

    if(bi >= TIR._1 && bi <= TIR._2){
      Attempt.successful(BitVector.empty ++ bv.padLeft(8))
    } else if(bi >= I8R._1 && bi <= I8R._2){
      Attempt.successful(I8 ++ bv.padLeft(8))
    } else if(bi >= I16R._1 && bi <= I16R._2){
      Attempt.successful(I16 ++ bv.padLeft(8 * 2))
    } else if(bi >= I32R._1 && bi <= I32R._2){
      Attempt.successful(I32 ++ bv.padLeft(8 * 4))
    } else if(bi >= I64R._1 && bi <= I64R._2){
      Attempt.successful(I64 ++ bv.padLeft(8 * 8))
    } else {
      Attempt.failure(Err("The stored BigInt is out of bounds for the internal INT_64 representation (-9 223 372 036 854 775 808 -> +9 223 372 036 854 775 807)"))
    }

  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltInteger]] = {

    val (marker, body) = bits.splitAt(8)

    marker match {
      case I8 =>
        int8.decode(body).map(_.map(i => BoltInteger(i)))
      case I16 =>
        int16.decode(body).map(_.map(i => BoltInteger(i)))
      case I32 =>
        int32.decode(body).map(_.map(i => BoltInteger(i)))
      case I64 =>
        int64.decode(body).map(_.map(i => BoltInteger(i)))
      case _ => // overflow case, match only if body is blank and ti is 8 long
        int8.decode(marker).map {
          a =>
            DecodeResult(BoltInteger(a.value), body)
        }
    }
  }

}
