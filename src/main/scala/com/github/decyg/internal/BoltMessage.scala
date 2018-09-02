package com.github.decyg.internal

import scodec._
import scodec.bits._
import codecs._


sealed trait BoltMessage {
}

case class InitCodec() extends Codec[InitCodec] with BoltMessage {
  override def encode(value: InitCodec): Attempt[BitVector] = ???

  override def sizeBound: SizeBound = ???

  override def decode(bits: BitVector): Attempt[DecodeResult[InitCodec]] = ???
}

/*
case class RunCodec() extends Codec[InitCodec] with BoltMessage{
  override val signature: Byte = 0x10

}

case class DiscardAllCodec() extends Codec[DiscardAllCodec] with BoltMessage{
  override val signature: Byte = 0x2F

}

case class PullAllCodec() extends Codec[PullAllCodec] with BoltMessage{
  override val signature: Byte = 0x3F

}

case class AckFailureCodec() extends Codec[AckFailureCodec] with BoltMessage{
  override val signature: Byte = 0x0E

}

case class ResetCodec() extends Codec[ResetCodec] with BoltMessage{
  override val signature: Byte = 0x0F

}

case class RecordCodec() extends Codec[RecordCodec] with BoltMessage{
  override val signature: Byte = 0x71

}

case class SuccessCodec() extends Codec[SuccessCodec] with BoltMessage{
  override val signature: Byte = 0x70

}

case class FailureCodec() extends Codec[FailureCodec] with BoltMessage{
  override val signature: Byte = 0x7F

}

case class IgnoredCodec() extends Codec[IgnoredCodec] with BoltMessage{
  override val signature: Byte = 0x7E

}
*/