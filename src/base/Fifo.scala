package hwlib.base

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.sim._



class FifoControl(logDepth : Int) extends Component {

  val depth = 1 << logDepth

  val io = new Bundle {
    val iPutValid =  in(Bool)
    val oPutReady = out(Bool)
    val oGetValid = out(Bool)
    val iGetReady =  in(Bool)

    val oWritePos = out(UInt(logDepth bits))
    val oReadPos  = out(UInt(logDepth bits))

    val oCount    = out(UInt((logDepth + 1) bits))
    val oSpace    = out(UInt((logDepth + 1) bits))
    val oFull     = out(Bool)
    val oEmpty    = out(Bool)
  }

  val rReadPos  = Counter(logDepth bits)
  val rWritePos = Counter(logDepth bits)
  val rFull     = Reg(Bool).init(False)

  val sEmpty    = (rReadPos === rWritePos) && !rFull
  val sCountRaw = rWritePos - rReadPos

  when (io.iPutValid && io.oPutReady && io.oGetValid && io.iGetReady) {
    rReadPos.increment()
    rWritePos.increment()
  } elsewhen (io.iPutValid && io.oPutReady) {
    rWritePos.increment()
    when (rWritePos.valueNext === rReadPos) {
      rFull := True
    }
  } elsewhen (io.oGetValid && io.iGetReady) {
    rReadPos.increment()
    rFull := False
  }

  io.oPutReady := !rFull
  io.oGetValid := !sEmpty

  io.oWritePos := rWritePos
  io.oReadPos  := rReadPos

  io.oCount    := Mux(rFull, U(1 << logDepth), sCountRaw.resize(logDepth + 1))
  io.oSpace    := Mux(rFull, U(0), U(1 << logDepth) - sCountRaw.resize(logDepth + 1))
  io.oFull     := rFull
  io.oEmpty    := sEmpty
}


class Fifo[T <: Data](tPayload : HardType[T], logDepth : Int, syncRead : Boolean = true) extends Component {

  val depth = 1 << logDepth

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

  val rBuffer = Mem(tPayload(), depth)

  val iControl = new FifoControl(logDepth)

  val iPutSide = new Area {
    val sDoWrite  = Bool

    sDoWrite := io.iPutValid && iControl.io.oPutReady

    iControl.io.iPutValid := io.iPutValid
    io.oPutReady := iControl.io.oPutReady
    rBuffer.write(iControl.io.oWritePos, io.iPutData, sDoWrite)
  }

  val iGetSide = if (syncRead) new Area {
    val rHasValue = Reg(Bool).init(False)
    val sDoRead = Bool

    sDoRead := False
    when (!rHasValue) {
      when (iControl.io.oGetValid) {
        sDoRead := True
        rHasValue := True
      }
    } otherwise {
      when (io.iGetReady && iControl.io.oGetValid) {
        sDoRead := True
      } elsewhen (io.iGetReady && !iControl.io.oGetValid) {
        rHasValue := False
      }
    }

    iControl.io.iGetReady := sDoRead
    io.oGetData := rBuffer.readSync(iControl.io.oReadPos, sDoRead)
    io.oGetValid := rHasValue
  } else new Area {
    iControl.io.iGetReady := io.iGetReady
    io.oGetValid := iControl.io.oGetValid
    io.oGetData := rBuffer.readAsync(iControl.io.oReadPos)
  }

  io.oCount    := iControl.io.oCount
  io.oSpace    := iControl.io.oSpace
  io.oFull     := iControl.io.oFull
  io.oEmpty    := iControl.io.oEmpty
}

object Fifo extends HwMain[Fifo[Bits]] {

  simSuiteWith("SyncRead")(new Fifo[Bits](Bits(8 bits), 3, true))
  clockAt(100 MHz)
  simSuiteRun("SyncRead"){ dut =>
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

  simSuiteWith("AsyncRead")(new Fifo[Bits](Bits(8 bits), 3, false))
  simSuiteRun("AsyncRead"){ dut =>
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

