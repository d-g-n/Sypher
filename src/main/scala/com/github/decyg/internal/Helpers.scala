package com.github.decyg.internal

import scodec.Codec

trait BoltSerializable {
  val codec: Codec[this.type]
}
