package com.github.decyg.internal.codecs

import com.github.decyg.internal.BoltString
import scodec.bits.BitVector
import scodec.codecs.{uint16, uint32, uint4, uint8, utf8}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits._

object BoltStringCodec extends Codec[BoltString] {
  // datatype markers
  val S = hex"8".bits.takeRight(4)
  val S8 = hex"D0".bits
  val S16 = hex"D1".bits
  val S32 = hex"D2".bits

  override def sizeBound: SizeBound = SizeBound.bounded(8, 8L + 32L + (4294967295L * 8L)) // marker: 8, size: lo/8/16/32, data: 4294967295 bytes

  override def encode(value: BoltString): Attempt[BitVector] = {
    utf8.encode(value.s).flatMap{
      strBV =>
        val strLen = strBV.bytes.length
        val strLenBits = BitVector(strLen)

        if(strLen <= 15L){
          Attempt.successful((BitVector(hex"80") | strLenBits) ++ strBV)
        } else if(strLen <= 255L){
          uint8.encode(strLen.toInt).map{
            u8 =>
              BitVector(hex"D0") ++ u8.padLeft(8) ++ strBV
          }
        } else if(strLen <= 65535L){
          uint16.encode(strLen.toInt).map{
            u16 =>
              BitVector(hex"D1") ++ u16.padLeft(16) ++ strBV
          }
        } else if(strLen <= 4294967295L){
          uint32.encode(strLen.toInt).map{
            u32 =>
              BitVector(hex"D2") ++ u32.padLeft(32) ++ strBV
          }
        } else {
          Attempt.failure(Err("String length must be less than or equal to 4294967295 to be encoded"))
        }
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltString]] = {

    val (marker, body) = bits.splitAt(8)
    val (markerHigh, markerLow) = marker.splitAt(4)

    val resDecode = (strDecode: BitVector, lenBounds: Long) => {
      val (parseableBody, res) = body.splitAt(lenBounds * 8)
      utf8.decode(parseableBody).flatMap(a => Attempt.successful(DecodeResult(BoltString(a.value), res)))
    }

    if(markerHigh == S){ // using just the high part as the low is variable, the low determines the datasize
      uint4.decode(markerLow).flatMap{ n => resDecode(body, n.value) }
    } else if(marker == S8){
      val (len, stringBody) = body.splitAt(8)
      uint8.decode(len).flatMap{ n => resDecode(stringBody, n.value) }
    } else if(marker == S16){
      val (len, stringBody) = body.splitAt(16)
      uint16.decode(len).flatMap{ n => resDecode(stringBody, n.value) }
    } else if(marker == S32){
      val (len, stringBody) = body.splitAt(32)
      uint32.decode(len).flatMap{ n => resDecode(stringBody, n.value) }
    } else {
      Attempt.failure(Err("Not a valid BoltString representation"))
    }
  }
}
