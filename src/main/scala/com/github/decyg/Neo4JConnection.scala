package com.github.decyg

import java.net.URI
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.TimeUnit

import akka.Done
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage, WebSocketRequest}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import org.java_websocket.client.WebSocketClient
import org.java_websocket.handshake.ServerHandshake

import scala.collection.immutable
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.io.StdIn

class Neo4JConnection(
           hostname: String = "localhost",
           port: Int = 7687
           ) extends WebSocketClient(new URI(s"ws://$hostname:$port/")) with LazyLogging {

  private implicit def int2Byte(i: Int): Byte = i.toByte
  private implicit def int2ByteSeq(i: Seq[Int]): Seq[Byte] = i.map(_.toByte)


  private def sendRaw(args: Byte*) = {
    val res = args.toArray
    send(res)
  }

  private def bytePairToBE16UInt(b1: Byte, b0: Byte) = {
    0x00000000 | (b1 << 8) | b0
  }

  private def dataFromChunk(args: Byte*): List[Byte] = {
    args.toList match {
      case 0x00 :: 0x00 :: Nil =>
        List()
      case b1 :: b0 :: a =>
        // this is a case in which a is a valid chunk of size
        val dataChunkLen = bytePairToBE16UInt(b1, b0)
        // the first datachunklen number of elements in a are for consumption
        val (left, right) = a.splitAt(dataChunkLen)
        left ++ dataFromChunk(right : _*)
      case _ =>
        List()
    }
  }


  val testdata = Array(
    0x00, 0x10, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, //0x00, 0x00,
    0x00, 0x10, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x00, 0x00
  ).map(_.toByte)
  println(testdata.map("%02X" format _).mkString(" "))
  println(dataFromChunk(testdata : _*).map("%02X" format _).mkString(" "))

  private def dataAsChunked(data: Array[Byte], chunkSize: Int = 16): Array[Byte] = {
    data match {
      case a if a.length > 0 =>
        // it's too long so split, wrap it and go recursive

        val (left, right) = a.splitAt(chunkSize)

        val outLen = 0x0000 | left.length
        Array((outLen >> 8).toByte, outLen.toByte) ++ left ++ dataAsChunked(right)
      case _ =>
        Array(0x00, 0x00)
    }
  }

  val testData2 = Array(
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
  ).map(_.toByte)

  println(testData2.map("%02X" format _).mkString(" "))
  println(dataAsChunked(testData2).map("%02X" format _).mkString(" "))


  override def onOpen(handshakedata: ServerHandshake): Unit = {
    logger.info("Connecting with the magic handshake")
    sendRaw(0x60, 0x60, 0xB0, 0x17)

    logger.info("Sending requested version (v1) as a list")
    sendRaw(0x00, 0x00, 0x00, 0x01)
    sendRaw(0x00, 0x00, 0x00, 0x00)
    sendRaw(0x00, 0x00, 0x00, 0x00)
    sendRaw(0x00, 0x00, 0x00, 0x00)

  }

  override def onMessage(message: ByteBuffer): Unit = {
    val a = message.array()

    logger.info("Message received: " + a.mkString(", "))
    // TODO: this is just testing of a simple INIT clause
    if(a.deep == Array(0x00, 0x00, 0x00, 0x01).map(_.toByte).deep){
      logger.info("attempting to return ")
      val res = Array(0xB2, 0x01).map(_.toByte) ++ """INIT "MyClient/1.0" { "scheme": "basic", "principal": "neo4j", "credentials": "secret" }""".getBytes("UTF-8")

      println(res.map("%02X" format _).mkString)
      sendRaw(res : _*)

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