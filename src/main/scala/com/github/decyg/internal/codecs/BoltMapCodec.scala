package com.github.decyg.internal.codecs

import com.github.decyg.internal.{BoltMap, BoltType}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs.{uint16, uint32, uint4, uint8}
import scodec.bits._

object BoltMapCodec extends Codec[BoltMap[BoltType, BoltType]] {
  // datatype markers
  val M = hex"A".bits.takeRight(4)
  val M8 = hex"D8".bits
  val M16 = hex"D9".bits
  val M32 = hex"DA".bits

  override def sizeBound: SizeBound = SizeBound.atLeast(8 * 2) // unbound but has to at least be two bytes

  override def encode(value: BoltMap[BoltType, BoltType]): Attempt[BitVector] = {

    val listLen = value.m.size

    val asValueList = value.m.foldLeft(Attempt.successful(BitVector(hex""))){
      (cur, bt) =>
        cur.flatMap{
          bvL =>
            for{
              l <- Codec[BoltType].encode(bt._1)
              r <- Codec[BoltType].encode(bt._2)
            } yield {
              bvL ++ l ++ r
            }
        }
    }

    if(listLen <= 15L){
      uint4.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv =>
              (hex"A0".bits | listLenBV.padLeft(8)) ++ bv
          }
      }
    } else if(listLen <= 255L){
      uint8.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => M8 ++ listLenBV.padLeft(8) ++ bv
          }
      }
    } else if(listLen <= 65535L){
      uint16.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => M16 ++ listLenBV.padLeft(16) ++ bv
          }
      }
    } else if(listLen <= 4294967295L){
      uint32.encode(listLen).flatMap{
        listLenBV =>
          asValueList.map{
            bv => M32 ++ listLenBV.padLeft(32) ++ bv
          }
      }
    } else {
      Attempt.failure(Err("Can't encode a BoltMap longer than 4294967295 individual key pair entries"))
    }

  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltMap[BoltType, BoltType]]] = {
    val (marker, body) = bits.splitAt(8)
    val (markerHigh, markerLow) = marker.splitAt(4)

    val resDecode = (lstDecode: BitVector, lstLen: Long) => {
      // arbritrary length bitvector and the number of "elements", just need to iteratively consume until its found all elems
      (0 until lstLen.toInt).foldLeft(Attempt.successful((Map.empty[BoltType, BoltType], lstDecode))){
        (mapAtt, _) =>
          mapAtt.flatMap{
            mp =>
              Codec[BoltType].decode(mp._2).flatMap{
                leftVal =>
                  Codec[BoltType].decode(leftVal.remainder).map{
                    rightVal =>
                      (mp._1 + (leftVal.value -> rightVal.value), rightVal.remainder)
                  }
              }
          }
      }.map(res => DecodeResult(BoltMap(res._1), res._2))
    }

    if(markerHigh == M){ // using just the high part as the low is variable, the low determines the datasize
      uint4.decode(markerLow).flatMap{ n => resDecode(body, n.value) }
    } else if(marker == M8){
      val (len, listBody) = body.splitAt(8)
      uint8.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else if(marker == M16){
      val (len, listBody) = body.splitAt(16)
      uint16.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else if(marker == M32){
      val (len, listBody) = body.splitAt(32)
      uint32.decode(len).flatMap{ n => resDecode(listBody, n.value) }
    } else {
      Attempt.failure(Err("Not a valid BoltMap representation"))
    }
  }
}
