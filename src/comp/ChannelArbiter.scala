package hwlib.comp

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.bundle.AutoBundle
import hwlib.amba.Channel
import hwlib.HwMain
import hwlib.sim._
import hwlib.sim.amba._



class ChannelArbiter[T <: AutoBundle](tPayload : HardType[T], count : Int, prio : Boolean = false) extends Component {
  val idWidth = log2Up(count)

  val io = new Bundle {
    val sOpt   = Vec( slave(new Channel(tPayload)), count)
    val mSel   =     master(new Channel(tPayload))
    val oSelId =        out(UInt(idWidth bits))
  }
  def TMask = UInt(count bits)
  def TId   = UInt(idWidth bits)

  val sRequest   = TMask
  val sNextGrant = TMask
  val sNextMask  = TMask
  val sNextId    = TId

  val rGrant     = Reg(TMask).init(U(0))
  val rMask      = Reg(TMask).init(U(0))
  val rId        = Reg(TId).init(U(0))

  io.mSel.payload.assign()
  io.mSel.valid := False
  for (idx <- 0 until count) {
    sRequest(idx) := io.sOpt(idx).valid & ~rGrant(idx)
    io.sOpt(idx).ready := rGrant(idx) && io.mSel.ready
    when (rGrant(idx)) {
      io.mSel.payload << io.sOpt(idx).payload
      io.mSel.valid := io.sOpt(idx).valid
    }
  }
  io.oSelId := rId

  val iGrantLogic = if (prio) new Area {
    sNextGrant := sRequest & (~sRequest + 1)
    sNextMask  := 0
    sNextId    := OHToUInt(sNextGrant)
  } else new Area {
    val sMaskedRequest = TMask
    val sPlainGrant    = TMask
    val sMaskedGrant   = TMask

    sMaskedRequest := sRequest & rMask
    sPlainGrant    := sRequest & (~sRequest + 1)
    sMaskedGrant   := sMaskedRequest & (~sMaskedRequest + 1)
    sNextGrant     := Mux(sMaskedGrant === 0, sPlainGrant, sMaskedGrant)
    sNextMask      := ~((sNextGrant - 1) | sNextGrant)
    sNextId        := OHToUInt(sNextGrant)
  }

  when (!io.mSel.valid || io.mSel.ready) {
    rGrant := sNextGrant
    rMask  := sNextMask
    rId    := sNextId
  }
}


case class ChannelArbiter_TestPayload(width : Int) extends AutoBundle {
  val data = Bits(width bits)
  element("data")
}

object ChannelArbiter extends HwMain[ChannelArbiter[ChannelArbiter_TestPayload]] {

  simSuiteWith("RoundRobin")(new ChannelArbiter(ChannelArbiter_TestPayload(8), 4, false))
  clockAt(200 MHz)
  simSuiteRun("RoundRobin"){ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.timeout(8192)
    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    val cfgOpt0  = DelayQueueConfig(dGet=RateGen(0.25))
    val cfgOpt1  = DelayQueueConfig(dGet=RateGen(0.10))
    val cfgOpt2  = DelayQueueConfig(dGet=RateGen(0.05))
    val cfgOpt3  = DelayQueueConfig(dGet=RateGen(0.05))
    val cfgSel   = DelayQueueConfig(dPut=RateGen(0.5))

    val sendOpt0 = new ChannelSender(dut.io.sOpt(0), cfgOpt0)((payload, num : Int) => payload.data #= num)
    val sendOpt1 = new ChannelSender(dut.io.sOpt(1), cfgOpt1)((payload, num : Int) => payload.data #= num)
    val sendOpt2 = new ChannelSender(dut.io.sOpt(2), cfgOpt2)((payload, num : Int) => payload.data #= num)
    val sendOpt3 = new ChannelSender(dut.io.sOpt(3), cfgOpt3)((payload, num : Int) => payload.data #= num)
    val recvSel = new ChannelReceiver(dut.io.mSel, cfgSel)(payload => payload.data.toInt)

    var count = 0
    val flag = Future[Unit]()
    recvSel.onGet(){ (num, time) =>
      println(num)
      count -= 1
      if (count == 0) {
        flag.resolve()
      }
      Take
    }

    env.waitFor()
    for (idx <- 0 until 256) {
      count += 4
      sendOpt0.put(idx)
      sendOpt1.put(idx)
      sendOpt2.put(idx)
      sendOpt3.put(idx)
    }

    flag.blockValue()
    env.waitFor(32)
  }

  simSuiteWith("Priority")(new ChannelArbiter(ChannelArbiter_TestPayload(8), 4, true))
  simSuiteRun("Priority"){ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.timeout(8192)
    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    val cfgOpt0  = DelayQueueConfig(dGet=RateGen(0.25))
    val cfgOpt1  = DelayQueueConfig(dGet=RateGen(0.10))
    val cfgOpt2  = DelayQueueConfig(dGet=RateGen(0.05))
    val cfgOpt3  = DelayQueueConfig(dGet=RateGen(0.05))
    val cfgSel   = DelayQueueConfig(dPut=RateGen(0.5))

    val sendOpt0 = new ChannelSender(dut.io.sOpt(0), cfgOpt0)((payload, num : Int) => payload.data #= num)
    val sendOpt1 = new ChannelSender(dut.io.sOpt(1), cfgOpt1)((payload, num : Int) => payload.data #= num)
    val sendOpt2 = new ChannelSender(dut.io.sOpt(2), cfgOpt2)((payload, num : Int) => payload.data #= num)
    val sendOpt3 = new ChannelSender(dut.io.sOpt(3), cfgOpt3)((payload, num : Int) => payload.data #= num)
    val recvSel = new ChannelReceiver(dut.io.mSel, cfgSel)(payload => payload.data.toInt)

    var count = 0
    val flag = Future[Unit]()
    recvSel.onGet(){ (num, time) =>
      println(num)
      count -= 1
      if (count == 0) {
        flag.resolve()
      }
      Take
    }

    env.waitFor()
    for (idx <- 0 until 256) {
      count += 4
      sendOpt0.put(idx)
      sendOpt1.put(idx)
      sendOpt2.put(idx)
      sendOpt3.put(idx)
    }

    flag.blockValue()
    env.waitFor(32)
  }


}

