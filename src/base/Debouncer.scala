package hwlib.base

import scala.util.Random

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.bundle._
import hwlib.sim._
import hwlib.HwMain


class Debouncer(val width : Int, depth : Int = 3) extends Component {
  require(width > 0)
  require(depth > 0)

  val io = new Bundle {
    val iTick  =  in(Bool)

    val iLine  =  in(Bits(width bits))
    val oState = out(Bits(width bits))
    val oRise  = out(Bits(width bits))
    val oFall  = out(Bits(width bits))
    val oValid = out(Bool)
  }

  val AllOnes = B((1 << depth) - 1, depth bits)
  val AllZeros = B(0, depth bits)

  val rLine = BufferCC(io.iLine)
  val rHistory = History(rLine, depth, io.iTick)

  val rStartup = Reg(UInt(log2Up(depth + 1) bits)) init U(depth, log2Up(depth + 1) bits)
  when (io.iTick && rStartup =/= 0) {
    rStartup := rStartup - 1
  }
  val sValid = rStartup === 0
  val sInit  = rStartup === 1

  val iLogic = for (idx <- 0 until width) yield new Area {
    val sSlice = Vec.tabulate(depth)(rHistory(_)(idx)).asBits

    val rState = Reg(Bool) init False
    val sRise = Bool
    val sFall = Bool

    sRise := False
    sFall := False
    when (io.iTick) {
      when (sInit) {
        rState := MajorityVote(sSlice)
      } elsewhen (sValid) {
        when (sSlice === AllOnes) {
          sRise := !rState
          rState := True
        } elsewhen (sSlice === AllZeros) {
          sFall := rState
          rState := False
        }
      }
    }

    io.oState(idx) := rState
    io.oRise(idx) := sRise
    io.oFall(idx) := sFall
  }
  io.oValid := sValid
}


class DebouncerTest(val width : Int, depth : Int = 3, freq : HertzNumber) extends Component {
  val io = new Bundle {
    val oTick  = out(Bool)

    val iLine = in(Bits(width bits))
    val oState = out(Bits(width bits))
    val oRise  = out(Bits(width bits))
    val oFall  = out(Bits(width bits))
    val oValid = out(Bool)
  }

  val iDivider = new StaticDivider(freq)
  io.oTick := iDivider.io.oTick

  val iDebouncer = new Debouncer(width, depth)
  iDebouncer.io.iTick := iDivider.io.oTick
  iDebouncer.io.iLine := io.iLine
  io.oState := iDebouncer.io.oState
  io.oRise  := iDebouncer.io.oRise
  io.oFall  := iDebouncer.io.oFall
  io.oValid := iDebouncer.io.oValid
}

object DebouncerTest extends HwMain[DebouncerTest] {


  simWith(new DebouncerTest(4, 3, 33 MHz))
  clockAt(100 MHz)

  val timeBase = ((100 MHz).toTime / (1 ps)).toLong
  val nBounceMin = 1
  val nBounceMax = 10
  val tBounceMin = 0
  val tBounceMax = 5
  val tDelayMin  = 100
  val tDelayMax  = 1000

  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    for (idx <- 0 until 4) {
      fork {
        while (true) {
          for (i <- 0 until Random.between(nBounceMin, nBounceMax + 1)) {
            sleep(Random.between(timeBase * tBounceMin, timeBase * tBounceMax + 1) max 1)
            val state = dut.io.iLine.toLong
            dut.io.iLine #= state ^ (1L << idx)
          }
          sleep(Random.between(timeBase * tDelayMin, timeBase * tDelayMax + 1))
        }
      }
    }

    env.waitFor(10000)
  }

}

