package com.github.decyg

package object internal {

  implicit def int2Byte(i: Int): Byte = i.toByte
  implicit def int2ByteSeq(i: Seq[Int]): Seq[Byte] = i.map(_.toByte)

}
