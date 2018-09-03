package com.github.decyg.internal

import scodec._
import scodec.bits._
import codecs._


sealed trait BoltMessage

case class BoltInit(clientName: String, authToken: Map[String, BoltType]) extends BoltMessage
case class BoltRun(statement: String, parameters: Map[String, BoltType]) extends BoltMessage
case class BoltDiscardAll() extends BoltMessage
case class BoltPullAll() extends BoltMessage
case class BoltAckFailure() extends BoltMessage
case class BoltReset() extends BoltMessage
case class BoltRecord(fields: List[BoltType]) extends BoltMessage
case class BoltSuccess(metadata: Map[String, BoltType]) extends BoltMessage
case class BoltFailure(metadata: Map[String, BoltType]) extends BoltMessage
case class BoltIgnored() extends BoltMessage


object BoltMessage {

}