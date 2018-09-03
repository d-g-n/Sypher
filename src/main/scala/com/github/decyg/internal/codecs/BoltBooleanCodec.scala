package com.github.decyg.internal.codecs

import com.github.decyg.internal.BoltBoolean
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.bits._

object BoltBooleanCodec extends Codec[BoltBoolean] {
  override def sizeBound: SizeBound = SizeBound.exact(8)

  override def encode(value: BoltBoolean): Attempt[BitVector] = {
    if(value.b){
      Attempt.successful(hex"C3".bits)
    } else {
      Attempt.successful(hex"C2".bits)
    }
  }

  override def decode(b: BitVector): Attempt[DecodeResult[BoltBoolean]] = {
    val (marker, body) = b.splitAt(8)

    if (marker == hex"C3".bits){
      Attempt.successful(DecodeResult(BoltBoolean(true), body))

    } else if(marker == hex"C2".bits){
      Attempt.successful(DecodeResult(BoltBoolean(false), body))

    } else Attempt.failure(Err("Was not a BoltBoolean value!"))
  }
}
