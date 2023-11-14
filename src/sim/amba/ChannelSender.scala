package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.bundle.AutoBundle
import hwlib.amba.Channel
import hwlib.sim.{ClockEnv, DelayQueue, DelayQueueConfig}



class ChannelSender[T <: AutoBundle, U](ch : Channel[T], cfg : DelayQueueConfig)(conv: (T, U) => Unit)(implicit val env : ClockEnv) {

  val queue = new DelayQueue[U](cfg)
  env.dbg.addChild(this, queue, "dq")

  var stateValid = false
  ch.valid #= stateValid

  env.onClk { () =>
    if (!stateValid || ch.ready.toBoolean) {
      val item = queue.rawGet()
      stateValid = item.isDefined
      if (item.isDefined) {
        conv(ch.payload, item.get)
        env.dbg.printForTag(this, "ChannelSender", s"Sent ${item.get}")
      }
    }
    ch.valid #= stateValid
  }

  def put(item : U, id : Long = 0) = queue.put(item, id)
  def bput(item : U, id : Long = 0) = queue.bput(item, id)
  def onPut(call : () => Option[(U, Long)]) = queue.onPut(call)
  def onPutDefault() : Unit = queue.onPutDefault()
}

