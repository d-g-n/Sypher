package com.github.decyg.internal.codecs

import com.github.decyg.internal.{BoltList, BoltType}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs.{uint16, uint32, uint4, uint8}
import scodec.bits._
import shapeless.{Generic, Lazy}

class BoltListCodec[T <: BoltType] extends Codec[BoltList[T]] {
  // datatype markers
  val L = hex"9".bits.takeRight(4)
  val L8 = hex"D4".bits
  val L16 = hex"D5".bits
  val L32 = hex"D6".bits

  override def sizeBound: SizeBound = SizeBound.atLeast(8 * 2) // unbound but has to at least be two bytes

  override def encode(value: BoltList[T]): Attempt[BitVector] = {

    val listLen = value.l.size

    implicit lazy val test: Codec[BoltType] = BoltType.codec

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
            bv => L8 ++ listLenBV.padLeft(8) ++ bv
          }
      }
    } else if(listLen <= 65535L){
      uint16.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => L16 ++ listLenBV.padLeft(16) ++ bv
          }
      }
    } else if(listLen <= 4294967295L){
      uint32.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => L32 ++ listLenBV.padLeft(32) ++ bv
          }
      }
    } else {
      Attempt.failure(Err("Can't encode a BoltList longer than 4294967295 individual items"))
    }

  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltList[T]]] = {
    val (marker, body) = bits.splitAt(8)
    val (markerHigh, markerLow) = marker.splitAt(4)

    import BoltType._
    val resDecode = (lstDecode: BitVector, lstLen: Long) => {
      // arbritrary length bitvector and the number of "elements", just need to iteratively consume until its found all elems
      (0 until lstLen.toInt).foldLeft(Attempt.successful((List.empty[T], lstDecode))){
        (lstAtt, i) =>
          lstAtt.flatMap{
            lst =>


              Codec[T].decode(lst._2).map{
                resLst =>
                  (List(resLst.value) ++ lst._1, resLst.remainder)
              }
          }
      }.map(a => DecodeResult(BoltList(a._1), a._2))
    }

    if(markerHigh == L){ // using just the high part as the low is variable, the low determines the datasize
      uint4.decode(markerLow).flatMap{ n => resDecode(body, n.value) }
    } else if(marker == L8){
      val (len, listBody) = body.splitAt(8)
      uint8.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else if(marker == L16){
      val (len, listBody) = body.splitAt(16)
      uint16.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else if(marker == L32){
      val (len, listBody) = body.splitAt(32)
      uint32.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else {
      Attempt.failure(Err("Not a valid BoltList representation"))
    }
  }

}
