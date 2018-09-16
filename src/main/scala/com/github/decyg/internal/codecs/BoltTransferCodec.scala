package com.github.decyg.internal.codecs

import com.github.decyg.internal.{BoltTransferEncoding, BoltType}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.bits._
import scodec.codecs.{uint16, uint32, uint4, uint8}

object BoltTransferCodec extends Codec[BoltTransferEncoding] {

  val CHUNK_SIZE_BYTE_COUNT = 8192
  val CHUNK_START_MARKER = uint16.encode(CHUNK_SIZE_BYTE_COUNT).require.padLeft(2 * 8) // this will never not fail
  val CHUNK_END_MARKER = hex"00 00".bits

  override def sizeBound: SizeBound = SizeBound.atLeast(16)

  override def encode(value: BoltTransferEncoding): Attempt[BitVector] = {


    def recursiveProduce(bv: BitVector): Attempt[BitVector] = {

      val numBytes = bv.bytes.size

      if (numBytes == 0) {
        Attempt.successful(CHUNK_END_MARKER)
      } else {

        if (numBytes >= CHUNK_SIZE_BYTE_COUNT) {

          val (current, rest) = bv.splitAt(CHUNK_SIZE_BYTE_COUNT * 8)

          recursiveProduce(rest).map {
            recur =>
              CHUNK_START_MARKER ++ current ++ recur
          }

        } else {
          uint16.encode(numBytes.toInt).map {
            CHUNK_START_OF_END_MARKER =>
              CHUNK_START_OF_END_MARKER ++ bv ++ CHUNK_END_MARKER
          }
        }
      }
    }

    BoltMessageCodec.encode(value.message).flatMap(recursiveProduce)
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BoltTransferEncoding]] = {

    assert(bits.sizeGreaterThanOrEqual(16))

    def recursiveConsume(bv: BitVector): Attempt[DecodeResult[BitVector]] = {
      val (header, chunk) = bv.splitAt(16) // two byte header, and the rest

      uint16.decode(header).flatMap{
        res =>
          val chunkSize = res.value

          if(chunkSize == 0){ // if it's the "end marker" then it's expected that this is the end of the match, ensure that chunk is empty, if not, consumption is bad
            assert(chunk.isEmpty)

            Attempt.successful(DecodeResult(BitVector.empty, BitVector.empty))
          } else {

            // this is the chunk size in number of bytes
            val (curChunk, nextPart) = chunk.splitAt(chunkSize * 8)

            recursiveConsume(nextPart).map{
              nextDecode =>
                nextDecode.map{
                  nextPartBV =>
                    curChunk ++ nextPartBV
                }
            }
          }
      }
    }

    recursiveConsume(bits).flatMap{
      decodeRes =>
        val boltVal = decodeRes.value

        assert(decodeRes.remainder.isEmpty)

        BoltMessageCodec.decode(boltVal).map(_.map(BoltTransferEncoding))
    }
  }
}
