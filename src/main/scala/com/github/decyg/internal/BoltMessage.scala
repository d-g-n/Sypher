package com.github.decyg.internal

import scodec._
import scodec.bits._
import codecs._


sealed trait BoltMessage

case class BoltInit(clientName: BoltString, authToken: BoltMap[BoltString, BoltType]) extends BoltMessage
case class BoltRun(statement: BoltString, parameters: BoltMap[BoltString, BoltType]) extends BoltMessage
case class BoltDiscardAll() extends BoltMessage
case class BoltPullAll() extends BoltMessage
case class BoltAckFailure() extends BoltMessage
case class BoltReset() extends BoltMessage
case class BoltRecord(fields: BoltList[BoltType]) extends BoltMessage
case class BoltSuccess(metadata: BoltMap[BoltString, BoltType]) extends BoltMessage
case class BoltFailure(metadata: BoltMap[BoltString, BoltType]) extends BoltMessage
case class BoltIgnored() extends BoltMessage


object BoltMessage {

}