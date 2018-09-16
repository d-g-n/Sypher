import com.github.decyg.{Neo4JConnection, internal}
import com.github.decyg.internal._
import org.scalatest.FlatSpec
import scodec.Codec

import scala.io.StdIn
import BoltType._
import com.github.decyg.internal.codecs.{BoltMessageCodec, BoltTransferCodec}
import scodec.bits._
import scodec.codecs._
import shapeless.Typeable

class BoltSpec extends FlatSpec{

  val c: Codec[BoltType] = Codec[BoltType]

  "A given hex string" should "be able to be encoded into various concrete types" in {}

  it should "be able to handle null values" in {
    assert(c.decode(hex"C0".bits).require.value == BoltNull())
    assert(c.encode(BoltNull()).require == hex"C0".bits)
  }

  it should "be able to handle boolean values" in {
    assert(c.decode(hex"C3".bits).require.value == BoltBoolean(true))
    assert(c.decode(hex"C2".bits).require.value == BoltBoolean(false))
    assert(c.encode(BoltBoolean(true)).require == hex"C3".bits)
    assert(c.encode(BoltBoolean(false)).require == hex"C2".bits)
  }

  it should "be able to handle integer values" in {

    // TINY_INT (-16 TO 127)
    assert(c.decode(hex"00".bits).require.value == BoltInteger(0))
    assert(c.decode(hex"7F".bits).require.value == BoltInteger(127))
    assert(c.decode(hex"F0".bits).require.value == BoltInteger(-16))
    assert(c.decode(hex"FF".bits).require.value == BoltInteger(-1))

    assert(c.encode(BoltInteger(0)).require == hex"00".bits)
    assert(c.encode(BoltInteger(127)).require == hex"7F".bits)
    assert(c.encode(BoltInteger(-16)).require == hex"F0".bits)
    assert(c.encode(BoltInteger(-1)).require == hex"FF".bits)

    // INT_8 (-128 TO 127)
    assert(c.decode(hex"C8 80".bits).require.value == BoltInteger(-128))
    assert(c.decode(hex"C8 7F".bits).require.value == BoltInteger(127))

    assert(c.encode(BoltInteger(-128)).require == hex"C8 80".bits)
    assert(c.encode(BoltInteger(-17)).require == hex"C8 EF".bits)

    // INT_16 (-32768 TO 32767)
    assert(c.decode(hex"C9 80 00".bits).require.value == BoltInteger(-32768))
    assert(c.decode(hex"C9 7F FF".bits).require.value == BoltInteger(32767))

    assert(c.encode(BoltInteger(-32768)).require == hex"C9 80 00".bits)
    assert(c.encode(BoltInteger(32767)).require == hex"C9 7F FF".bits)

    // INT_32 (-32768 TO 32767)
    assert(c.decode(hex"CA 80 00 00 00".bits).require.value == BoltInteger(-2147483648))
    assert(c.decode(hex"CA 7F FF FF FF".bits).require.value == BoltInteger(2147483647))

    assert(c.encode(BoltInteger(-2147483648)).require == hex"CA 80 00 00 00".bits)
    assert(c.encode(BoltInteger(2147483647)).require == hex"CA 7F FF FF FF".bits)

    // INT_64 (-32768 TO 32767)
    assert(c.decode(hex"CB 80 00 00 00 00 00 00 00".bits).require.value == BoltInteger(BigInt("-9223372036854775808")))
    assert(c.decode(hex"CB 7F FF FF FF FF FF FF FF".bits).require.value == BoltInteger(BigInt("9223372036854775807")))

    assert(c.encode(BoltInteger(BigInt("-9223372036854775808"))).require == hex"CB 80 00 00 00 00 00 00 00".bits)
    assert(c.encode(BoltInteger(BigInt("9223372036854775807"))).require == hex"CB 7F FF FF FF FF FF FF FF".bits)
  }

  it should "be able to handle float values" in {
    assert(c.decode(hex"C1 7F EF FF FF FF FF FF FF".bits).require.value == BoltFloat(Double.MaxValue))
    assert(c.decode(hex"C1 FF EF FF FF FF FF FF FF".bits).require.value == BoltFloat(Double.MinValue))

    assert(c.encode(BoltFloat(Double.MaxValue)).require == hex"C1 7F EF FF FF FF FF FF FF".bits)
    assert(c.encode(BoltFloat(Double.MinValue)).require == hex"C1 FF EF FF FF FF FF FF FF".bits)
  }

  it should "be able to handle string values" in {
    assert(c.decode(hex"80".bits).require.value == BoltString(""))
    assert(c.decode(hex"8F 61 61 61 61 61 61 61 61 61 61 61 61 61 61 61".bits).require.value == BoltString("aaaaaaaaaaaaaaa"))
    assert(c.decode(hex"D0 10 61 61 61 61 61 61 61 61 61 61 61 61 61 61 61 61".bits).require.value == BoltString("aaaaaaaaaaaaaaaa"))
    // D1 and D2 omitted as they should work is the earlier ones work

    assert(c.encode(BoltString("")).require == hex"80".bits)
    assert(c.encode(BoltString("aaaaaaaaaaaaaaa")).require == hex"8F 61 61 61 61 61 61 61 61 61 61 61 61 61 61 61".bits)
    assert(c.encode(BoltString("aaaaaaaaaaaaaaaa")).require == hex"D0 10 61 61 61 61 61 61 61 61 61 61 61 61 61 61 61 61".bits)
  }

  it should "be able to handle list values of mixed types, retaining order" in {
    val b = (0 to 16).map(i => BoltInteger(i))
    val n = Seq(BoltInteger(1), BoltInteger(2), BoltList(Seq(BoltInteger(3), BoltInteger(4), BoltList(Seq()))))

    assert(c.decode(hex"93 01 02 93 03 04 90".bits).require.value == BoltList(n))
    assert(c.decode(hex"D4 11 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10".bits).require.value == BoltList(b))

    assert(c.encode(BoltList(b)).require == hex"D4 11 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10".bits)
    assert(c.encode(BoltList(n)).require == hex"93 01 02 93 03 04 90".bits)

    val typedListCodec = BoltType.typedListCodec(Typeable[BoltNull])

    val validEncode = typedListCodec.encode(BoltList(List(BoltNull(), BoltNull())))
    val invalidEncode = typedListCodec.encode(BoltList(List(BoltNull(), BoltNull(), BoltString("not null"))))

    assert(validEncode.require == hex"92 C0 C0".bits)
    assert(validEncode.isSuccessful)

    assert(!invalidEncode.isSuccessful)

    val validDecode = typedListCodec.decode(hex"92 C0 C0".bits)
    val invalidDecode = typedListCodec.decode(hex"92 C0 90".bits)

    assert(validDecode.require.value == BoltList(List(BoltNull(), BoltNull())))
    assert(validDecode.isSuccessful)

    assert(!invalidDecode.isSuccessful)
  }

  it should "be able to handle map values of mixed types, retaining order" in {
    val n: Map[BoltType, BoltType] = Map(
      BoltString("a") -> BoltInteger(1),
      BoltString("b") -> BoltInteger(1),
      BoltString("c") -> BoltInteger(3),
      BoltString("d") -> BoltInteger(4),
      BoltString("e") -> BoltInteger(5),
      BoltString("f") -> BoltInteger(6),
      BoltString("g") -> BoltInteger(7),
      BoltString("h") -> BoltInteger(8),
      BoltString("i") -> BoltInteger(9),
      BoltString("j") -> BoltInteger(0),
      BoltString("k") -> BoltInteger(1),
      BoltString("l") -> BoltInteger(2),
      BoltString("m") -> BoltInteger(3),
      BoltString("n") -> BoltInteger(4),
      BoltString("o") -> BoltInteger(5),
      BoltString("p") -> BoltInteger(6),

    )

    assert(c.decode(hex"D8 10 81 61  01 81 62 01  81 63 03 81  64 04 81 65 05 81 66 06  81 67 07 81  68 08 81 69  09 81 6A 00 81 6B 01 81  6C 02 81 6D  03 81 6E 04  81 6F 05 81 70 06".bits).require.value == BoltMap(n))

    val typedMapCodec = BoltType.typedMapCodec(Typeable[BoltString], Typeable[BoltInteger])

    val validDecode = typedMapCodec.decode(hex"A1 81 61 01".bits)
    val invalidDecode = typedMapCodec.decode(hex"A1 81 61 90".bits)

    assert(validDecode.require.value == BoltMap(Map(BoltString("a") -> internal.BoltInteger(1))))
    assert(validDecode.isSuccessful)

    assert(!invalidDecode.isSuccessful)

  }

  it should "be able to handle encoding and decoding of messages vice versa" in {

    val bType1 = BoltInit("test string", Map(
      "one two three" -> BoltMap(
        Map(BoltNull() -> BoltString("inner map"))),
      "four five six" -> BoltBoolean(true)))

    val bType1Encode = BoltTransferCodec.encode(BoltTransferEncoding(bType1))

    assert(bType1Encode.isSuccessful)

    val bType1Decode = BoltTransferCodec.decode(bType1Encode.require)

    assert(bType1Decode.isSuccessful)
    assert(bType1Decode.require.value.message == bType1)
  }
}
