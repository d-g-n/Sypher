package com.github.decyg.internal.codecs

import com.github.decyg.internal.BoltNull
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits._

object BoltNullCodec extends Codec[BoltNull] {
  override def sizeBound: SizeBound = SizeBound.exact(8)

  override def encode(value: BoltNull): Attempt[BitVector] = {
    Attempt.successful(hex"C0".bits)
  }

  override def decode(b: BitVector): Attempt[DecodeResult[BoltNull]] = {
    val (marker, body) = b.splitAt(8)
    if (marker == hex"C0".bits)
      Attempt.successful(DecodeResult(BoltNull(), body))
    else
      Attempt.failure(Err("Was not a BoltNull value!"))
  }
}
