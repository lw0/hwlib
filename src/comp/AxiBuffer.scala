package hwlib.comp

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import hwlib.base.{IrqConfig, TIrq, IrqRising}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, ApbConfig, TApb}
import hwlib.HwMain
import hwlib.sim._
import hwlib.sim.amba._



class AxiBuffer(val cfgAxi : AxiConfig,
                val irqTimeout : TimeNumber = 1 us) extends Component {
  val logDepth = cfgAxi.addrBits

  val cfgStm = AxiStreamConfig(dataBytes = cfgAxi.dataBytes, hasKeep = true)

  val bufferSize = 1 << logDepth
  val ptrBits = logDepth
  val idxBits = cfgAxi.fullSize
  val adrBits = logDepth - cfgAxi.fullSize

  def TData     = cfgAxi.TData
  def TMask     = cfgAxi.TMask
  def TPointer  = UInt(ptrBits bits)
  def TWIndex   = UInt(idxBits bits)
  def TWShift   = UInt((idxBits + 1) bits)
  def TWPointer = UInt(adrBits bits)

  val io = new Bundle {
    val sBuf =  slave(TAxi(cfgAxi))

    val sStm =  slave(TAxiStream(cfgStm))

    /* sReg Map (word addresses):
      +0x00 - Read Begin (RW): writing advances circular buffer / releases space
      +0x01 - Read Limit (RO)
      +0x02 - Read Wrap Mask (RO)
      +0x03 - Irq Threshold (W): == 0 disables irq, > 0 generates irq when available > threshold
      +0x03 - Fill Level (R): current number of available elements
     */
    val sReg =  slave(TApb(AxiBuffer.CfgApb))

    val mIrq = master(TIrq(AxiBuffer.CfgIrq))
  }

  val rBuffer = Mem(TData, 1 << adrBits)

  // TODO-lw add soft-reset option, maybe writing to reg(1) ?
  //val sReset = Bool // driven by register file (write 0x1)

  val rWritePtr  = Reg(TPointer) init U(0)
  val sReadPtr   = TPointer // driven by register file (0x0)
  val sWritePtr  = TPointer // driven by pipeline delayed to rBuffer access

  val sAvail     = sWritePtr - sReadPtr
  val sFree      = sReadPtr - rWritePtr - 1

  val sThres     = TPointer // driven by register file (0x3)
  val sThresPass = (sThres =/= 0) && (sAvail >= sThres)

  val rLastPtr   = Reg(TPointer) init U(0)
  val rLastValid = Reg(Bool) init False
  val sLastClear = Bool // driven by irq acknowledge logic / write to register 0x0
  val sLastLevel = rLastPtr - sReadPtr
  val sLastAvail = rLastValid && (sAvail >= sLastLevel)


  /// Stream Ingress Stage 1 ///
  // conservative check for maximum size,
  //  to avoid combinatorial dependency between keep -> ready
  val s1CanFit  = sFree >= cfgAxi.dataBytes
  val s1Count   = CountOne(io.sStm.keep)

  val r1Pointer = Reg(TPointer)
  val r1Data    = Reg(TData)
  val r1Mask    = Reg(TMask)
  val r1Valid   = Reg(Bool) init False

  io.sStm.ready := s1CanFit
  when (io.sStm.valid && s1CanFit) {
    r1Valid := True
    rWritePtr := rWritePtr + s1Count
    r1Pointer := rWritePtr
    r1Data    := io.sStm.data
    r1Mask    := io.sStm.keep
  } otherwise {
    r1Valid := False
  }

  when (io.sStm.valid && io.sStm.last && s1CanFit) {
    rLastPtr := rWritePtr + s1Count - 1
    rLastValid := True
  } elsewhen (sLastClear) {
    rLastValid := False
  }

  /// Stream Compaction Mask & Mux Stage 2 ///
  val s2Select   = Vec(TMask, cfgAxi.dataBytes)
  val s2Remain   = Vec(TMask, cfgAxi.dataBytes)
  val s2Data     = TData
  val s2Mask     = TMask

  val r2WritePtr = RegNext(rWritePtr)
  val r2Pointer  = RegNext(r1Pointer)
  val r2Data     = RegNext(s2Data)
  val r2Mask     = RegNext(s2Mask)
  val r2Valid    = RegNext(r1Valid)

  s2Remain(0) := r1Mask
  for (idx <- 0 until cfgAxi.dataBytes) {
    s2Select(idx) := OHMasking.first(s2Remain(idx))
    if ((idx + 1) < cfgAxi.dataBytes) {
      s2Remain(idx + 1) := s2Remain(idx) & ~s2Select(idx)
    }
    s2Mask(idx) := s2Select(idx).orR
    s2Data(8*idx, 8 bits) := Mux(s2Select(idx).orR, MuxOH(s2Select(idx), r1Data.subdivideIn(8 bits)), B(0, 8 bits))
  }

  /// Stream Write Alignment Stage 3 ///
  val s3LShift   = TWIndex
  val s3RShift   = TWShift
  val s3Data     = TData
  val s3Mask     = TMask
  val s3Address  = TWPointer
  val s3Valid    = Bool

  val r3Data     = Reg(TData) init 0
  val r3Mask     = Reg(TMask) init 0
  val r3Address  = Reg(TWPointer) init 0
  val r3WritePtr = RegNext(r2WritePtr)

  s3LShift  := r2Pointer(0, idxBits bits)
  s3RShift  := (~r2Pointer(0, idxBits bits)).resize(idxBits + 1) + 1

  when (r2Valid) {
    s3Valid := True
    s3Data := (r2Data |<< (s3LShift << 3)) | r3Data
    s3Mask := (r2Mask |<<  s3LShift) | r3Mask
    s3Address := r2Pointer(idxBits, adrBits bits)
    r3Data := r2Data |>> (s3RShift << 3)
    r3Mask := r2Mask |>>  s3RShift
    r3Address := r2Pointer(idxBits, adrBits bits) + 1
  } otherwise {
    s3Valid   := r3Mask.orR
    s3Data    := r3Data
    s3Mask    := r3Mask
    s3Address := r3Address
    r3Mask := 0
    r3Data := 0
    r3Address := 0
  }

  rBuffer.write(s3Address, s3Data, s3Valid, s3Mask)
  sWritePtr := r3WritePtr


  /// Control Interface ///

  val iRegs = new ApbRegFile(AxiBuffer.CfgApb)
  iRegs.io.sApb << io.sReg

  // Reg 0: Base (Read Pointer)
  sReadPtr := iRegs.io.oReg(0).asUInt.resized
  iRegs.io.iReg(0) := sReadPtr.asBits.resized

  // Reg 1: Limit (Write Pointer delayed to rBuffer access)
  iRegs.io.iReg(1) := sWritePtr.asBits.resized

  // Reg 2: Mask (Exposes rBuffer addressing range)
  iRegs.io.iReg(2) := B(bufferSize - 1)

  // Reg 3: IRQ Threshold (Write) / Available (Read)
  sThres := iRegs.io.oReg(3).asUInt.resized
  iRegs.io.iReg(3) := sAvail.asBits.resized

  // IRQ Logic
  val rIrqGate = Timeout(irqTimeout)

  sLastClear := False
  when (iRegs.io.oRegWr(0)) {
    when (!sLastAvail) {
      sLastClear := True
    }
    rIrqGate.clear()
  }

  io.mIrq.irq := (sThresPass || sLastAvail) && rIrqGate


  /// Buffer Interface ///

  val iMemInterface = new AxiMemInterface(cfgAxi, denyWrite=true, hasGrant=false)
  iMemInterface.io.sAxi << io.sBuf

  iMemInterface.io.mPortRd.rdata := rBuffer.readSync(iMemInterface.io.mPortRd.addr, iMemInterface.io.mPortRd.req)

}


object AxiBuffer extends HwMain[AxiBuffer] {

  val CfgApb = ApbConfig(addrBits = 2, dataBytes = 4)
  val CfgIrq = IrqConfig(sense = IrqRising)

  val dataSize = 2 // 4 bytes

  simWith(new AxiBuffer(AxiConfig(addrBits=8, dataBytes=(1 << dataSize)), irqTimeout=100 ns))
  clockAt(200 MHz)

  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    val doneFlag = Future[Unit]()
    env.timeout(64*1024)

    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    val iRegRW = new ApbMaster(dut.io.sReg, DelayQueueConfig())

    val axiConfig = AxiModelConfig(dataRate=RateGen(0.4))
    val iAxiRd = new AxiRdMaster(dut.io.sBuf, axiConfig)

    val stmConfig = AxiModelConfig(dataRate=RateGen(0.6))
    val iStm = new AxiStreamMaster(dut.io.sStm, stmConfig)

    env.waitFor()

    println("=== Begin ===")

    fork {
      // Stream Generator Process
      val test = Array[Int](0x40302010, 0x41312111, 0x42322212, 0x43332313,
                            0x44342414, 0x45352515, 0x46362616, 0x47372717,
                            0x48382818, 0x49392919, 0x4a3a2a1a, 0x4b3b2b1b,
                            0x4c3c2c1c, 0x4d3d2d1d, 0x4e3e2e1e, 0x4f3f2f1f)

      iStm.sendInts(Array.concat(test, test, test), nullBytes=AverageRate(0.8))
      env.waitFor(500)
      iStm.sendInts(test)
      env.waitFor(200)
      iStm.sendInts(Array.concat(test, test), nullBytes=AverageRate(0.6))
      env.waitFor(500)
      iStm.sendInts(Array.concat(test, test, test, test, test, test))
      env.waitFor(500)
      iStm.sendInts(test, nullBytes=AverageRate(0.4))
      env.waitFor(500)
      iStm.sendInts(Array.concat(test, test, test))
      env.waitFor(500)
      iStm.sendInts(test, nullBytes=AverageRate(0.8))
    }

    val stableEmpty = Future[Unit]()
    val controlMutex = SimMutex()

    fork {
      // Polling Process
      //  waits for 32 successive stable fill levels to give Interrupt Handler Process a chance
      var stableCount = 0L
      var lastAvailable = 0L
      var repeat = true
      while (repeat) {
        controlMutex.lock()
        val available = iRegRW.bread(0x3).data.toLong
        controlMutex.unlock()
        if (lastAvailable == available) {
          stableCount += 1
        } else {
          stableCount = 0
        }
        lastAvailable = available
        if (stableCount > 128) {
          if (lastAvailable == 0) {
            repeat = false
          }
        }
      }
      stableEmpty.resolve()
    }

    fork {
      // Interrupt Handler Process
      controlMutex.lock()
      iRegRW.bwrite(0x3, 0x80)
      controlMutex.unlock()
      while (true) {
          env.waitNextWhen(dut.io.mIrq.irq.toBoolean)

        println(s"Available IRQ")
        controlMutex.lock()
        performFetch(iRegRW, iAxiRd)
        controlMutex.unlock()
      }
    }

    stableEmpty.blockValue()
    println("=== End ===")
    env.waitFor(108)
  }

  def performFetch(iReg : ApbMaster, iBuf : AxiRdMaster) {
    val base   = iReg.bread(0x0).data.toInt
    val limit  = iReg.bread(0x1).data.toInt
    val mask   = iReg.bread(0x2).data.toInt
    val wbase  = base >> dataSize
    val wlimit = limit >> dataSize
    val wmask  = mask >> dataSize
    if (base > limit) {
      val countUpper = roundUp(mask - base + 1, 4).toInt / 4
      val countLower = roundUp(limit - 1, 4).toInt / 4
      println(f"Reading 0x${base}%04x + 0x${countUpper << dataSize}%x and 0x0000 + 0x${countLower << dataSize}%x")
      val reqUpper = iBuf.read(base, countUpper, Size.from(dataSize), Burst.Incr)
      val reqLower = iBuf.read(0x0, countLower, Size.from(dataSize), Burst.Incr)
      println(reqUpper.blockValue())
      println(reqLower.blockValue())
    } else if (base < limit) {
      val count = roundUp(limit - base + 1, 4).toInt / 4
      println(f"Reading 0x${base}%04x + 0x${count << dataSize}%x")
      val req = iBuf.read(base, count, Size.from(dataSize), Burst.Incr)
      println(req.blockValue())
    }
    iReg.bwrite(0x0, limit)
  }
}

