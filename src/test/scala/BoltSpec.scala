import com.github.decyg.Neo4JConnection
import com.github.decyg.internal._
import org.scalatest.FlatSpec
import scodec.Codec

import scala.io.StdIn
import BoltType._
import scodec.bits._
import scodec.codecs._

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

  }
}
