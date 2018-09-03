package com.github.decyg.internal

import com.github.decyg.internal.codecs._
import scodec.codecs.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._
import shapeless.Typeable


sealed trait BoltType
case class BoltNull() extends BoltType
case class BoltBoolean(b: Boolean) extends BoltType
case class BoltInteger(i: BigInt) extends BoltType
case class BoltFloat(d: Double) extends BoltType
case class BoltString(s: String) extends BoltType
case class BoltList(l: List[BoltType]) extends BoltType
case class BoltMap(m: Map[BoltType, BoltType]) extends BoltType

// Bolt nodes etc are actually just structure types, just defined by a different signature from normal
// the below structures also use the reduced versions of each, as the types when the structs are formed will be checked at runtime
// note this is suboptimal and i'm aware, i just couldn't figure out how to get scodec to respect nested ADTs with parameterised types
sealed trait BoltStructure extends BoltType

case class BoltStructureContainer(l: List[BoltType] = List()) extends BoltStructure
case class BoltNode(nodeIdentity: Integer, labels: List[String], properties: Map[String, BoltType]) extends BoltStructure
case class BoltRelationship(relIdentity: BigInt, startNodeIdentity: BigInt, endNodeIdentity: BigInt, `type`: String, properties: Map[String, BoltType]) extends BoltStructure
case class BoltUnboundRelationship(relIdentity: BigInt, `type`: String, properties: Map[String, BoltType]) extends BoltStructure
case class BoltPath(nodes: List[BoltNode], relationships: List[BoltUnboundRelationship], sequence: List[BigInt]) extends BoltStructure

object BoltType {

  def testa = {
    //val test = Codec[BoltList[BoltType]].encode(BoltList(l = List(BoltList(List(BoltNull())), BoltNull())))
    //val othertest = Codec[BoltList[BoltType]].encode(BoltList(List(BoltNull(), BoltNull())))

    //val htest = Codec[BoltList[BoltType]].decode(BitVector(hex"9291C0C0"))
    //val hothertest = bvToBTGeneric(BitVector(hex"93C0C0C0"))
    //val hothertesttest = Codec[BoltType].decode(BitVector(hex"C0"))

    val maptest = (integer ~ list ~ map).decode(BitVector(hex"""01 91 01 A1 01 C0"""))


    val n = 10+10
  }

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

  implicit val marker: Codec[BoltTypeMarker.WithRange] = BoltTypeMarkerCodec

  implicit val `null`: Codec[BoltNull] = BoltNullCodec

  implicit val boolean: Codec[BoltBoolean] = BoltBooleanCodec

  implicit val integer: Codec[BoltInteger] = BoltIntegerCodec

  implicit val float: Codec[BoltFloat] = BoltFloatCodec

  implicit val string: Codec[BoltString] = BoltStringCodec

  implicit val list: Codec[BoltList] = BoltListCodec

  implicit val map: Codec[BoltMap] = BoltMapCodec

  implicit val structure: Codec[BoltStructure] = BoltStructureCodec

}
