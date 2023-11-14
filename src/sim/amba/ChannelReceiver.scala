package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.bundle.AutoBundle
import hwlib.amba.Channel
import hwlib.sim.{ClockEnv, DelayQueue, DelayQueueConfig, GetAction}



class ChannelReceiver[T <: AutoBundle, U](ch : Channel[T], cfg : DelayQueueConfig)(conv : (T) => U)(implicit val env : ClockEnv) {

  val queue = new DelayQueue[U](cfg)
  env.dbg.addChild(this, queue, "dq")

  var stateReady = false
  ch.ready #= stateReady

  env.onClk { () =>
    if (stateReady && ch.valid.toBoolean) {
      val item = conv(ch.payload)
      env.dbg.printForTag(this, "ChannelReceiver", s"Received ${item}")
      queue.rawPut(item)
    }
    stateReady = queue.rawCanPut(1)
    ch.ready #= stateReady
  }

  def get(id : Option[Long] = None) = queue.get(id)
  def bget(id : Option[Long] = None) = queue.bget(id)
  def onGet(id : Option[Long] = None)(call : (U, Long) => GetAction) = queue.onGet(id)(call)
  def onGetDefault(id : Option[Long] = None) = queue.onGetDefault(id)
}

