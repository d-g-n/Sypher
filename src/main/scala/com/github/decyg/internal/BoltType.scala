package com.github.decyg.internal
import scodec._
import scodec.bits._
import codecs._
import scodec.codecs.implicits._

sealed trait BoltType

case class BoltNull() extends BoltType
object BoltNull{ implicit val discriminator: Discriminator[BoltType, BoltNull, Int] = Discriminator(1) }

case class BoltBoolean(b: Boolean) extends BoltType
object BoltBoolean{ implicit val discriminator: Discriminator[BoltType, BoltBoolean, Int] = Discriminator(2) }

case class BoltInteger(i: BigInt) extends BoltType
object BoltInteger{ implicit val discriminator: Discriminator[BoltType, BoltInteger, Int] = Discriminator(3) }

case class BoltFloat(d: Double) extends BoltType
object BoltFloat{ implicit val discriminator: Discriminator[BoltType, BoltFloat, Int] = Discriminator(4) }

case class BoltString(s: String) extends BoltType
object BoltString{ implicit val discriminator: Discriminator[BoltType, BoltString, Int] = Discriminator(5) }

case class BoltList[T <: BoltType](l: List[T]) extends BoltType
object BoltList{ implicit val discriminator: Discriminator[BoltType, BoltList[BoltType], Int] = Discriminator(6) }

case class BoltMap[L <: BoltType, R <: BoltType](m: Map[L, R]) extends BoltType
object BoltMap{ implicit val discriminator: Discriminator[BoltType, BoltMap[BoltType, BoltType], Int] = Discriminator(7) }

case class BoltStructure(s: List[BoltType]) extends BoltType
object BoltStructure{ implicit val discriminator: Discriminator[BoltType, BoltStructure, Int] = Discriminator(8) }

case class BoltNode(nodeIdentity: BoltInteger, labels: BoltList[BoltString], properties: BoltMap[BoltString, BoltType]) extends BoltType
object BoltNode{ implicit val discriminator: Discriminator[BoltType, BoltNode, Int] = Discriminator(9) }

case class BoltRelationship(relIdentity: BoltInteger, startNodeIdentity: BoltInteger, endNodeIdentity: BoltInteger, `type`: BoltString, properties: BoltMap[BoltString, BoltType]) extends BoltType
object BoltRelationship{ implicit val discriminator: Discriminator[BoltType, BoltRelationship, Int] = Discriminator(10) }

case class BoltUnboundRelationship(relIdentity: BoltInteger, `type`: BoltString, properties: BoltMap[BoltString, BoltType]) extends BoltType
object BoltUnboundRelationship{ implicit val discriminator: Discriminator[BoltType, BoltUnboundRelationship, Int] = Discriminator(11) }

case class BoltPath(nodes: BoltList[BoltNode], relationships: BoltList[BoltUnboundRelationship], sequence: BoltList[BoltInteger])extends BoltType
object BoltPath{ implicit val discriminator: Discriminator[BoltType, BoltPath, Int] = Discriminator(12) }

object BoltType {

  implicit val discriminated: Discriminated[BoltType, Int] = Discriminated(uint8)

  implicit lazy val test: shapeless.Lazy[Codec[BoltType]] = {
    shapeless.Lazy[Codec[BoltType]].apply(Codec[BoltType])
  }

/**
  def btAsEncodedAttempt(boltType: BoltType): Attempt[BitVector] = {
    boltType match {
      case bt@BoltNull() => `null`.encode(bt)
      case bt@BoltBoolean(b) => boolean.encode(bt)
      case bt@BoltInteger(i) => integer.encode(bt)
      case
    }
  }
**/
  def decodedToBT(bv: BitVector): BoltType = ???

  implicit val `null`: Codec[BoltNull] = new Codec[BoltNull] {

    override def sizeBound: SizeBound = SizeBound.exact(8)

    override def encode(value: BoltNull): Attempt[BitVector] = {
      Attempt.successful(hex"C0".bits)
    }

    override def decode(b: BitVector): Attempt[DecodeResult[BoltNull]] = {
      if (b == hex"C0".bits)
        Attempt.successful(DecodeResult(BoltNull(), BitVector.empty))
      else Attempt.failure(Err("Was not null terminated!"))
    }
  }

  implicit val boolean: Codec[BoltBoolean] = new Codec[BoltBoolean] {

    override def sizeBound: SizeBound = SizeBound.exact(8)

    override def encode(value: BoltBoolean): Attempt[BitVector] = {
      if(value.b){
        Attempt.successful(hex"C3".bits)
      } else {
        Attempt.successful(hex"C2".bits)
      }
    }

    override def decode(b: BitVector): Attempt[DecodeResult[BoltBoolean]] = {
      if (b == hex"C3".bits){
        Attempt.successful(DecodeResult(BoltBoolean(true), BitVector.empty))

      } else if(b == hex"C2".bits){
        Attempt.successful(DecodeResult(BoltBoolean(true), BitVector.empty))

      } else Attempt.failure(Err("Was not a boolean value!"))
    }
  }

  implicit val integer: Codec[BoltInteger] = new Codec[BoltInteger] {

    // datatype markers
    val I8 = hex"C8".bits
    val I16 = hex"C9".bits
    val I32 = hex"CA".bits
    val I64 = hex"CB".bits

    // bounds for types
    val TIR = (BigInt("-16"), BigInt("127"))
    val I8R = (BigInt("-128"), BigInt("-17"))
    val I16R = (BigInt("-32768"), BigInt("32767"))
    val I32R = (BigInt("-2147483648"), BigInt("2147483647"))
    val I64R = (BigInt("-9223372036854775808"), BigInt("9223372036854775807"))

    override def sizeBound: SizeBound = SizeBound.bounded(8, 8 * 9) // from just the marker types to full int_64 rep

    override def encode(value: BoltInteger): Attempt[BitVector] = {

      val bi = value.i
      val bv = BitVector(bi.toByteArray)

      if(bi >= TIR._1 && bi <= TIR._2){
        Attempt.successful(BitVector.empty ++ bv.padLeft(8))
      } else if(bi >= I8R._1 && bi <= I8R._2){
        Attempt.successful(I8 ++ bv.padLeft(8))
      } else if(bi >= I16R._1 && bi <= I16R._2){
        Attempt.successful(I16 ++ bv.padLeft(8 * 2))
      } else if(bi >= I32R._1 && bi <= I32R._2){
        Attempt.successful(I32 ++ bv.padLeft(8 * 4))
      } else if(bi >= I64R._1 && bi <= I64R._2){
        Attempt.successful(I64 ++ bv.padLeft(8 * 8))
      } else {
        Attempt.failure(Err("The stored BigInt is larger than the internal INT_64 representation (-9 223 372 036 854 775 808 -> +9 223 372 036 854 775 807)"))
      }

    }

    override def decode(bits: BitVector): Attempt[DecodeResult[BoltInteger]] = {

      val (marker, body) = bits.splitAt(8)

      marker match {
        case I8 =>
          int8.decode(body).map(_.map(i => BoltInteger(i)))
        case I16 =>
          int16.decode(body).map(_.map(i => BoltInteger(i)))
        case I32 =>
          int32.decode(body).map(_.map(i => BoltInteger(i)))
        case I64 =>
          int64.decode(body).map(_.map(i => BoltInteger(i)))
        case ti if ti.size == 8 && body.size == 0 => // overflow case, match only if body is blank and ti is 8 long
          int8.decode(marker).map(_.map(i => BoltInteger(i)))
        case _ =>
          Attempt.failure(Err("Bit Vector is not a recognised numeric sequence"))
      }
    }
  }

  implicit val float: Codec[BoltFloat] = new Codec[BoltFloat] {

    override def sizeBound: SizeBound = SizeBound.exact(8 * 9)

    override def encode(value: BoltFloat): Attempt[BitVector] = {

      double.encode(value.d).map(v => hex"C1".bits ++ v.padLeft(8 * 8))
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[BoltFloat]] = {

      val (marker, body) = bits.splitAt(8)

      if(marker == hex"C1".bits && body.length == (8 * 8)){
        double.decode(body).map(_.map(d => BoltFloat(d)))
      } else {
        Attempt.failure(Err("This is not a valid Bolt Float type"))
      }
    }
  }

  implicit val string: Codec[BoltString] = new Codec[BoltString] {

    // datatype markers
    val S = hex"8".bits.takeRight(4)
    val S8 = hex"D0".bits
    val S16 = hex"D1".bits
    val S32 = hex"D2".bits

    override def sizeBound: SizeBound = SizeBound.bounded(8, 8L + 32L + (4294967295L * 8L)) // marker: 8, size: lo/8/16/32, data: 4294967295 bytes

    override def encode(value: BoltString): Attempt[BitVector] = {
      utf8.encode(value.s).flatMap{
        strBV =>
          val strLen = strBV.bytes.length
          val strLenBits = BitVector(strLen)

          if(strLen <= 15L){
            Attempt.successful((BitVector(hex"80") | strLenBits) ++ strBV)
          } else if(strLen <= 255L){
            uint8.encode(strLen.toInt).map{
              u8 =>
                BitVector(hex"D0") ++ u8.padLeft(8) ++ strBV
            }
          } else if(strLen <= 65535L){
            uint16.encode(strLen.toInt).map{
              u16 =>
                BitVector(hex"D1") ++ u16.padLeft(16) ++ strBV
            }
          } else if(strLen <= 4294967295L){
            uint32.encode(strLen.toInt).map{
              u32 =>
                BitVector(hex"D2") ++ u32.padLeft(32) ++ strBV
            }
          } else {
            Attempt.failure(Err("String is too large to be encoded"))
          }
      }
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[BoltString]] = {

      val (marker, body) = bits.splitAt(8)
      val (markerHigh, markerLow) = bits.splitAt(4)

      val resDecode = (strDecode: BitVector, lenBounds: Long) => {
        if(strDecode.length == (lenBounds * 8)){
          utf8.decode(strDecode).map(_.map(s => BoltString(s)))
        } else {
          Attempt.failure(Err(s"Doesn't conform to bits length ${lenBounds * 8}"))
        }
      }

      if(markerHigh == S){ // using just the high part as the low is variable, the low determines the datasize
        uint4.decode(markerLow).flatMap{ n => resDecode(body, n.value) }
      } else if(marker == S8){
        val (len, stringBody) = body.splitAt(8)
        uint8.decode(len).flatMap{ n => resDecode(stringBody, n.value) }
      } else if(marker == S16){
        val (len, stringBody) = body.splitAt(16)
        uint16.decode(len).flatMap{ n => resDecode(stringBody, n.value) }
      } else if(marker == S32){
        val (len, stringBody) = body.splitAt(32)
        uint32.decode(len).flatMap{ n => resDecode(stringBody, n.value) }
      } else {
        Attempt.failure(Err("Not a valid string representation"))
      }
    }
  }

  implicit val list: Codec[BoltList[BoltType]] = new Codec[BoltList[BoltType]] {

    // datatype markers
    val L = hex"9".bits.takeRight(4)
    val L8 = hex"D4".bits
    val L16 = hex"D5".bits
    val L32 = hex"D6".bits

    override def sizeBound: SizeBound = SizeBound.atLeast(8 * 2) // unbound but has to at least be two bytes

    override def encode(value: BoltList[BoltType]): Attempt[BitVector] = {

      val listLen = value.l.size

      if(listLen <= 15L){
        uint8.encode(listLen).map{
          listLenBV =>
             value.l.foldLeft(Attempt.successful(BitVector(hex""))){
              (cur, bt) =>
                cur.flatMap{
                  bvL =>
                    Codec[BoltType].encode(bt).map(bv => bvL ++ bv)
                    Attempt.successful(BitVector(hex""))
                }
            }.map{
              bv =>
                hex"9".bits ++ listLenBV ++ bv
            }
        }
      } else if(listLen <= 255L){

      } else if(listLen <= 65535L){

      } else if(listLen <= 4294967295L){

      } else {
        Attempt.failure(Err("Can't encode a list longer than 4294967295 individual items"))
      }

      Attempt.failure(Err(""))

    }

    override def decode(bits: BitVector): Attempt[DecodeResult[BoltList[BoltType]]] = {
      val (marker, body) = bits.splitAt(8)
      val (markerHigh, markerLow) = bits.splitAt(4)

      Attempt.failure(Err(""))
    }
  }
}
