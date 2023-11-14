package hwlib.base

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.sim._



class Barrier(count : Int) extends Component {
  def TVec = Bits(count bits)

  val io = new Bundle {
    val iStrobe = in(TVec)
    val oMask = out(TVec)
    val oContinue = out(Bool)
  }

  val rDone = Reg(TVec).init(B(0))
  val sContinue = Bool

  sContinue := (rDone | io.iStrobe).andR

  when (sContinue) {
    rDone := 0
  } otherwise {
    rDone := rDone | io.iStrobe
  }

  io.oMask := ~rDone
  io.oContinue := sContinue

}


object Barrier extends HwMain[Barrier] {

  simWith(new Barrier(4))
  clockAt(200 MHz)
  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    dut.io.iStrobe #= 0

    env.waitFor()

    for (idx <- 0 until 64) {
      dut.io.iStrobe #= Random.nextInt(16)
      env.waitFor()
    }

    env.waitFor(8)
  }
}


class ChannelFanIn(count : Int) extends Component {

  val io = new Bundle {
    val iValids =  in(Bits(count bits))
    val oReadys = out(Bits(count bits))

    val oValid  = out(Bool)
    val iReady  =  in(Bool)
  }

  io.oValid := io.iValids.andR
  io.oReadys := Mux(io.iReady && io.iValids.andR, ~io.oReadys.getZero, io.oReadys.getZero)
}


class ChannelFanOut(count : Int) extends Component {

  val io = new Bundle {
    val iValid  =  in(Bool)
    val oReady  = out(Bool)

    val oValids = out(Bits(count bits))
    val iReadys =  in(Bits(count bits))
  }

  val iBarrier = new Barrier(count)
  iBarrier.io.iStrobe := io.iReadys.andMask(io.iValid)
  io.oValids := iBarrier.io.oMask.andMask(io.iValid)
  io.oReady := iBarrier.io.oContinue
}

