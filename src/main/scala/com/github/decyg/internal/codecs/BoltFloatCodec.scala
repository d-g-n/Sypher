package com.github.decyg.internal.codecs

import com.github.decyg.internal.BoltFloat
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.BitVector
import scodec.codecs.double
import scodec.bits._

object BoltFloatCodec extends Codec[BoltFloat] {
  override def sizeBound: SizeBound = SizeBound.exact(8 * 9)

  override def encode(value: BoltFloat): Attempt[BitVector] = {
    double.encode(value.d).map(v => hex"C1".bits ++ v.padLeft(8 * 8))
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltFloat]] = {
    val (marker, body) = bits.splitAt(8)

    double.decode(body).map(_.map(i => BoltFloat(i)))
  }
}
