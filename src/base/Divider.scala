package hwlib.base

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.bundle._
import hwlib.sim._
import hwlib.HwMain


class Divider(val intWidth : Int, val fractWidth : Int) extends Component {
  require(intWidth >= 0)
  require(fractWidth >= 0)
  val countWidth = intWidth + 1

  val io = new Bundle {
    val iDivInt = in(UInt(intWidth bits))
    val iDivFrac = in(UInt(fractWidth bits))
    val iEnable = in(Bool) default True
    val oActive = out(Bool)

    val oIndex = out(UInt(fractWidth bits))
    val oTick = out(Bool)
    val oTock = out(Bool)
  }

  val rBaseCount = Reg(UInt(countWidth bits))
  val rFrac = Reg(UInt(fractWidth bits))
  val rIndex = Reg(UInt(fractWidth bits))
  val rCounter = Reg(UInt(countWidth bits))


  val iFsm = new StateMachine {
    io.oActive := False
    io.oTick := False
    io.oTock := False

    val Disabled : State = new State with EntryPoint {
      whenIsActive {
        when (io.iEnable) {
          rBaseCount := io.iDivInt.resized
          rFrac := io.iDivFrac
          rIndex := 0
          rCounter := io.iDivInt.resized
          goto(Active)
        }
      }
    }

    val Active : State = new State {
      whenIsActive {
        io.oActive := True
        when (rCounter === 0) {
          when (rIndex.reversed < rFrac) {
            rCounter := rBaseCount + 1
          } otherwise {
            rCounter := rBaseCount
          }
          if (fractWidth > 0) {
            rIndex := rIndex + 1
          }
          io.oTick := True
          when (~rIndex === 0) {
            io.oTock := True
            when(!io.iEnable) {
              rIndex := 0
              goto(Disabled)
            }
          }
        } otherwise {
          rCounter := rCounter - 1
        }
      }
    }
  }

  io.oIndex := rIndex
}

object Divider {
  case class Config(divInt : BigInt, divFrac : BigInt, intWidth : Int)

  def config(tickFrequency : HertzNumber, fractWidth : Int, fixedIntWidth : Option[Int] = None) : Config = {

    val clockFrequency = ClockDomain.current.frequency match {
      case ClockDomain.FixedFrequency(freq) => freq
      case _ => SpinalError(s"Divider.config(${tickFrequency.decomposeString}) requires known frequency for ClockDomain ${ClockDomain.current}")
    }

    val divIdeal = (clockFrequency / tickFrequency).toDouble
    val fracts = (1 << fractWidth).toDouble
    val div = Math.round(divIdeal * fracts) / fracts

    val actualFreq = clockFrequency / div
    val freqErr = (actualFreq - tickFrequency) / tickFrequency

    val divInt = div.toInt - 1
    val divFrac = ((div - div.toInt) * fracts).toInt
    val intWidth = log2Up(div.toInt)


    fixedIntWidth.map {
      constraint =>
        if (intWidth > constraint) {
          SpinalError(s"Divider.config(${tickFrequency.decomposeString}) @${clockFrequency.decomposeString} requires intWidth = ${intWidth} but is constrained to ${constraint}")
        }
    }
    println(f"Divider.config(${tickFrequency.decomposeString}%s) @${clockFrequency.decomposeString}%s: ${divIdeal}%.3f | ${div} --> ${actualFreq.decomposeString}%s (${freqErr * 100.0}%.1f %%)")

    Config(divInt, divFrac, fixedIntWidth.getOrElse(intWidth))
  }

}

object DividerTest extends HwMain[Divider] {
  simWith(new Divider(4, 4))

  clockAt(100 MHz)

  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    dut.io.iDivInt #= 0
    dut.io.iDivFrac #= 0
    dut.io.iEnable #= false

    env.waitFor(5)

    for (i <- 0 until 16) {
      dut.io.iEnable #= false
      env.waitNextWhen(!dut.io.oActive.toBoolean)
      dut.io.iDivInt #= 0
      dut.io.iDivFrac #= i
      dut.io.iEnable #= true

      env.waitFor(64)
    }

    for (i <- 0 until 16) {
      dut.io.iEnable #= false
      env.waitNextWhen(!dut.io.oActive.toBoolean)
      dut.io.iDivInt #= i
      dut.io.iDivFrac #= i
      dut.io.iEnable #= true

      env.waitFor(256)
    }
  }
}

class StaticDivider(val tickFrequency : HertzNumber, val fractWidth : Int = 0) extends Component {

  val cfg = Divider.config(tickFrequency, fractWidth)

  val io = new Bundle {
    val oIndex = out(UInt(fractWidth bits))
    val oTick = out(Bool)
    val oTock = out(Bool)
  }

  val iDivider = new Divider(cfg.intWidth, fractWidth)
  iDivider.io.iDivInt := U(cfg.divInt)
  iDivider.io.iDivFrac := U(cfg.divFrac)
  io.oIndex := iDivider.io.oIndex
  io.oTick := iDivider.io.oTick
  io.oTock := iDivider.io.oTock
}

