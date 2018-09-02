package com.github.decyg

import java.net.URI
import java.nio.ByteBuffer

import com.typesafe.scalalogging.LazyLogging
import org.java_websocket.client.WebSocketClient
import org.java_websocket.handshake.ServerHandshake
import scodec.bits._

class Neo4JConnection(
           hostname: String = "localhost",
           port: Int = 7687
           ) extends WebSocketClient(new URI(s"ws://$hostname:$port/")) with LazyLogging {

  private implicit def bv2Array(i: ByteVector): Array[Byte] = i.toArray


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
    val a = message.array()

    logger.info("Message received: " + a.mkString(", "))
    // TODO: this is just testing of a simple INIT clause
    if(a.deep == Array(0x00, 0x00, 0x00, 0x01).map(_.toByte).deep){
      logger.info("attempting to return ")
      val res = Array(0xB2, 0x01).map(_.toByte) ++ """INIT "MyClient/1.0" { "scheme": "basic", "principal": "neo4j", "credentials": "secret" }""".getBytes("UTF-8")

      println(res.map("%02X" format _).mkString)
      //sendRaw(res : _*)

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