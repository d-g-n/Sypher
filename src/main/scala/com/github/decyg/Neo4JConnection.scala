package com.github.decyg

import java.net.URI
import java.nio.ByteBuffer

import com.github.decyg.internal.codecs.BoltTransferCodec
import com.github.decyg.internal.{BoltInit, BoltString, BoltTransferEncoding, BoltType}
import com.typesafe.scalalogging.LazyLogging
import org.java_websocket.client.WebSocketClient
import org.java_websocket.handshake.ServerHandshake
import scodec.bits._


object test{
  def main(args: Array[String]): Unit = {
    new Neo4JConnection().connect()

    while(true){

    }
  }
}
class Neo4JConnection(
           hostname: String = "localhost",
           port: Int = 7687
           ) extends WebSocketClient(new URI(s"ws://$hostname:$port/")) with LazyLogging {

  private implicit def byv2Array(i: ByteVector): Array[Byte] = i.toArray
  private implicit def bv2Array(i: BitVector): Array[Byte] = i.toByteArray

  override def onOpen(handshakedata: ServerHandshake): Unit = {
    logger.info("Connecting with the magic handshake")
    send(hex"60 60 B0 17")

    logger.info("Sending requested version (v1) as a list")
    send(hex"00 00 00 01")
    send(hex"00 00 00 00")
    send(hex"00 00 00 00")
    send(hex"00 00 00 00")

  }

  override def onMessage(message: ByteBuffer): Unit = {
    val bv = ByteVector(message)

    logger.info(s"Message received: $bv")


    val a = message.array()

    // TODO: this is just testing of a simple INIT clause
    if(a.deep == Array(0x00, 0x00, 0x00, 0x01).map(_.toByte).deep){
      logger.info("attempting to send init message")
      val res = Array(0xB2, 0x01).map(_.toByte) ++ """INIT "MyClient/1.0" { "scheme": "basic", "principal": "test", "credentials": "test" }""".getBytes("UTF-8")


      val sendTE = BoltTransferEncoding(BoltInit("MyClient/1.0", Map("scheme" -> BoltString("basic"), "principal" -> BoltString("test"), "credentials" -> BoltString("test"))))

      val sendVal = BoltTransferCodec.encode(sendTE).require

      send(sendVal)
      //sendRaw(res : _*)

    } else {
      println(BoltTransferCodec.decode(bv.bits))

      send(hex"""00 27 B2 10  D0 22 45 58  50 4C 41 49  4E 20 4D 41
  54 43 48 20  28 6E 29 2C  20 28 6D 29  20 52 45 54
  55 52 4E 20  6E 2C 20 6D  A0 00 00""".bits)
    }

  }

  override def onClose(code: Int, reason: String, remote: Boolean): Unit = {
    println(code, reason, remote)
  }

  override def onError(ex: Exception): Unit = {
    ex.printStackTrace()
  }

  override def onMessage(message: String): Unit = {
    println(message)
  }

}