package hwlib.base

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.sim._
import hwlib.xilinx.blackbox.Shifter
import hwlib.HwMain



class ShiftFifo[T <: Data](tPayload : HardType[T], logDepth : Int) extends Component {
  require(logDepth > 0)
  require(logDepth <= 7)

  def TLevel = UInt(logDepth bits)
  val maxLevel = (1 << logDepth) - 1

  val io = new Bundle {
    val iPutData  =  in(tPayload())
    val iPutValid =  in(Bool)
    val oPutReady = out(Bool)
    val oGetData  = out(tPayload())
    val oGetValid = out(Bool)
    val iGetReady =  in(Bool)

    val oCount    = out(UInt((logDepth + 1) bits))
    val oSpace    = out(UInt((logDepth + 1) bits))
    val oFull     = out(Bool)
    val oEmpty    = out(Bool)
  }

  val rEmpty = Reg(Bool) init True
  val rLevel = Reg(TLevel) init 0
  val sLow = rLevel === 0
  val sFull = rLevel === maxLevel

  val sShift = Bool

  val iStage = new Stage(tPayload())
  iStage.io.iPutData := io.iPutData
  iStage.io.iPutValid := io.iPutValid
  io.oPutReady := iStage.io.oPutReady

  val iShifter = new Shifter(1 << logDepth, widthOf(tPayload()))
  iShifter.io.iData := iStage.io.oGetData.asBits
  iShifter.io.iAddr := rLevel
  iShifter.io.iEnable := sShift
  io.oGetData.assignFromBits(iShifter.io.oData)

  sShift := False
  when (!sFull && iStage.io.oGetValid && !rEmpty && io.iGetReady) {
    sShift := True
  } elsewhen (!sFull && iStage.io.oGetValid) {
    sShift := True
    when (rEmpty) {
      rEmpty := False
    } otherwise {
      rLevel := rLevel + 1
    }
  } elsewhen (!rEmpty && io.iGetReady) {
    when (sLow) {
      rEmpty := True
    } otherwise {
      rLevel := rLevel - 1
    }
  }

  iStage.io.iGetReady := !sFull
  io.oGetValid := !rEmpty

  io.oCount := Mux(rEmpty, U(0), rLevel.resize(logDepth + 1) + 1)
  io.oSpace := Mux(rEmpty, U(1 << logDepth), (~rLevel).resize(logDepth + 1))
  io.oEmpty := rEmpty
  io.oFull := sFull
}

object ShiftFifo extends HwMain[ShiftFifo[Bits]] {

  simWith(new ShiftFifo(Bits(8 bits), 3))
  clockAt(100 MHz)
  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val putValidSeq = BigInt("10010001000111111111111011000100001000000", 2)
    val getReadySeq = BigInt("10100100100001001000000011011111111111110", 2)
    val count = putValidSeq.bitLength max getReadySeq.bitLength + 1

    for (idx <- (count - 1) downto 0) {
      if (putValidSeq.testBit(idx)) {
        dut.io.iPutData #= idx
        dut.io.iPutValid #= true
      } else {
        dut.io.iPutValid #= false
      }
      dut.io.iGetReady #= getReadySeq.testBit(idx)
      env.clk.waitSampling()
    }
    env.clk.waitSampling(4)
  }
}

