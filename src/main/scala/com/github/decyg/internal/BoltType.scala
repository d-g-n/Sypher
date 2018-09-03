package com.github.decyg.internal

import com.github.decyg.internal.codecs._
import scodec.codecs.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._

sealed trait BoltType
case class BoltNull() extends BoltType
case class BoltBoolean(b: Boolean) extends BoltType
case class BoltInteger(i: BigInt) extends BoltType
case class BoltFloat(d: Double) extends BoltType
case class BoltString(s: String) extends BoltType
case class BoltList[T <: BoltType](l: List[T]) extends BoltType
case class BoltMap[L <: BoltType, R <: BoltType](m: Map[L, R]) extends BoltType

// Bolt nodes etc are actually just structure types, just defined by a different signature from normal
sealed trait BoltStructure extends BoltType

case class BoltStructureContainer(l: List[BoltType] = List()) extends BoltStructure
case class BoltNode(nodeIdentity: BoltInteger, labels: BoltList[BoltString], properties: BoltMap[BoltString, BoltType]) extends BoltStructure
case class BoltRelationship(relIdentity: BoltInteger, startNodeIdentity: BoltInteger, endNodeIdentity: BoltInteger, `type`: BoltString, properties: BoltMap[BoltString, BoltType]) extends BoltStructure
case class BoltUnboundRelationship(relIdentity: BoltInteger, `type`: BoltString, properties: BoltMap[BoltString, BoltType]) extends BoltStructure
case class BoltPath(nodes: BoltList[BoltNode], relationships: BoltList[BoltUnboundRelationship], sequence: BoltList[BoltInteger]) extends BoltStructure

object BoltType {

  def testa = {
    //val test = Codec[BoltList[BoltType]].encode(BoltList(l = List(BoltList(List(BoltNull())), BoltNull())))
    //val othertest = Codec[BoltList[BoltType]].encode(BoltList(List(BoltNull(), BoltNull())))

    //val htest = Codec[BoltList[BoltType]].decode(BitVector(hex"9291C0C0"))
    val hothertest = Codec[BoltList[BoltType]].decode(BitVector(hex"92C0C0"))
    //val hothertesttest = Codec[BoltType].decode(BitVector(hex"C0"))

    //val maptest = (Codec[BoltInteger] ~ Codec[BoltList[BoltType]] ~ Codec[BoltMap[BoltString, BoltType]]).decode(BitVector(hex"""01 90 A0"""))


    val n = 10+10
  }

  implicit val marker: Codec[BoltTypeMarker.WithRange] = BoltTypeMarkerCodec

  implicit val `null`: Codec[BoltNull] = BoltNullCodec

  implicit val boolean: Codec[BoltBoolean] = BoltBooleanCodec

  implicit val integer: Codec[BoltInteger] = BoltIntegerCodec

  implicit val float: Codec[BoltFloat] = BoltFloatCodec

  implicit val string: Codec[BoltString] = BoltStringCodec

  implicit val list: Codec[BoltList[BoltType]] = BoltListCodec

  implicit val map: Codec[BoltMap[BoltType, BoltType]] = BoltMapCodec

  implicit val structure: Codec[BoltStructure] = BoltStructureCodec

  implicit lazy val codec: Codec[BoltType] = shapeless.lazily {
    discriminated[BoltType].by(marker)
      .typecase(BoltTypeMarker.NULL, `null`)
      .typecase(BoltTypeMarker.BOOLEAN, boolean)
      .typecase(BoltTypeMarker.INTEGER, integer)
      .typecase(BoltTypeMarker.FLOAT, float)
      .typecase(BoltTypeMarker.STRING, string)
      .typecase(BoltTypeMarker.LIST, list)
      .typecase(BoltTypeMarker.MAP, map)
      .typecase(BoltTypeMarker.STRUCTURE, structure)
  }
}
