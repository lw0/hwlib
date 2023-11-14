package hwlib.comp

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import hwlib.comp.streamer._
import hwlib.base.{MultiFifo, ChannelFanOut}
import hwlib.bundle.{AutoBundle, AutoBundleBidir}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, ApbConfig, TApb, Amba}
import hwlib.HwMain
import hwlib.sim._
import hwlib.sim.amba._



class AxiReader(val baseAxi : AxiConfig,
                val baseApb : ApbConfig,
                val streamLogCaps : Seq[Int],
                val queueLogDepth : Int = 5) extends Component {
  val stmCount = streamLogCaps.length
  val cfgAxi = baseAxi.copy(hasWr=false, hasRd=true)
  val cfgApb = baseApb.copy(addrBits=2+log2Up(stmCount))

  val iCore = new ReaderCore(cfgAxi, streamLogCaps, queueLogDepth)

  val io = new Bundle {
    val mMem   =     master(TAxi(cfgAxi))

    val sReg   =      slave(TApb(cfgApb))
    // sReg Map:
    //  4*i + 0: Control and Status (read or write clears oIntr(i))
    //      0: write 1 to start  | read idle if 1, active if 0
    //      8: write ignored     | read error if 1 and idle=1
    //     24: write intr enable | read back intr enable
    //  4*i + 1: Unit
    //   8..0: write and read back maximum burst len (0->1, 255->256)
    //  4*i + 2: Base
    //  n..0: write and read back base address (dataWidth multiples)
    //  4*i + 3: Count
    //  m..0: write and read back count (dataWidth multiples)
    val oIntr  =        out(Bits(stmCount bits))

    val mStm   = Vec(master(TAxiStream(iCore.stmCfg)), stmCount)
  }

  iCore.io.mAxi >> io.mMem

  val iRegs = new ApbRegFile(cfgApb)
  iRegs.io.sApb << io.sReg

  val iStreams = for (idx <- 0 until stmCount) yield new Area {
    val iGen = new AddrGen_Sequential(cfgAxi)
    iGen.io.mGen >> iCore.io.sGen(idx)
    iCore.io.mStm(idx) >> io.mStm(idx)

    iCore.io.iStart(idx) := iRegs.io.oReg(idx*4+0)(0) && iRegs.io.oRegWr(idx*4+0)
    val rIntr = Reg(Bool).init(False)
    when (iCore.io.oIdle(idx).rise) {
      rIntr := True
    } elsewhen (iRegs.io.oRegRd(idx*4+0) || iRegs.io.oRegWr(idx*4+0)) {
      rIntr := False
    }
    val rIntrEn = Reg(Bool).init(False)
    when (iRegs.io.oRegWr(idx*4+0)) {
      rIntrEn := iRegs.io.oReg(idx*4+0)(24)
    }
    iRegs.io.iReg(idx*4+0) := ( 0 -> iCore.io.oIdle(idx),
                                8 -> iCore.io.oError(idx),
                               24 -> rIntrEn,
                               default -> False)

    iGen.io.iUnit := iRegs.io.oReg(idx*4+1).resize(widthOf(iGen.io.iUnit)).asUInt
    iRegs.io.iReg(idx*4+1) := iRegs.io.oReg(idx*4+1)

    iGen.io.iBase := iRegs.io.oReg(idx*4+2).resize(widthOf(iGen.io.iBase)).asUInt
    iRegs.io.iReg(idx*4+2) := iRegs.io.oReg(idx*4+2)

    iGen.io.iCount := iRegs.io.oReg(idx*4+3).resize(widthOf(iGen.io.iCount)).asUInt
    iRegs.io.iReg(idx*4+3) := iRegs.io.oReg(idx*4+3)

    io.oIntr(idx) := rIntr && rIntrEn
  }

}


object AxiReader extends HwMain[AxiReader] {
  simWith(new AxiReader(AxiConfig(addrBits=32, dataBytes=16, idBits=1), ApbConfig(addrBits=32, dataBytes=4), Seq(7, 6, 5, 4)))
  clockAt(200 MHz)
  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.timeout(64*1024)

    env.onRawCycle(2) { () =>
      env.setRst(false)
    }
    env.waitFor()

    case class Op(base : Int, count : Int, unit : Int)

    val testSet = Map(
      0 -> Seq(Op(0x100, 0x080, 0x07)),
      1 -> Seq(Op(0x200, 0x080, 0x07), Op(0x2C0, 0x080, 0x0A)),
      2 -> Seq(Op(0x300, 0x080, 0x07), Op(0x380, 0x055, 0x06)),
      3 -> Seq(Op(0x400, 0x080, 0x07), Op(0xFC0, 0x080, 0x0F)))

    val iRegRW = new ApbMaster(dut.io.sReg, DelayQueueConfig())

    val axiConfig = AxiModelConfig(
          dataRate=RateGen(0.8),
          addrRate=RateGen(0.5),
          issueReorder=true,
          resolveDelay=DelayGen(20.0),
          resolveRate=RateGen(0.5))
    val iAxiRd = new AxiRdSlave(dut.io.mMem, axiConfig)
    val iAxiMem = new MemBuffer(12)
    for (i <- 0 until 0x10000) {
      iAxiMem.writeInt(i*4, i*4)
    }
    iAxiRd.onRead(0x000, 0x10000L) {
      (addr, bytes) =>
        println(f"Read 0x${bytes}%x bytes at ${addr}%x")
        val data = iAxiMem.read(addr, bytes)
        (Resp.Okay, data)
    }

    val doneFlag = Future[Unit]()
    var doneCount = 0
    val stmConfig = AxiModelConfig(dataRate=RateGen(0.2))
    for (idx <- 0 until 4) {
      val iStm = new AxiStreamSlave(dut.io.mStm(idx), stmConfig)
      fork {
        for (pos <- 0 until testSet(idx).length) {
          val op = testSet(idx)(pos)
          println(f"Stm($idx): Initiate addr=0x${op.base}%x count=0x${op.count}%x unit=0x${op.unit}%x")
          iRegRW.bwrite(idx*4 + 1, op.unit)
          iRegRW.bwrite(idx*4 + 2, op.base)
          iRegRW.bwrite(idx*4 + 3, op.count)
          iRegRW.bwrite(idx*4 + 0, 0x01000001)
          env.waitNextWhen(dut.io.oIntr.toBigInt.testBit(idx))
          val res = iRegRW.bread(idx*4 + 0)
          println(s"Stm($idx): Completed error=${res.data.testBit(8)}")
        }
        doneCount += 1
        if (doneCount == 4) {
          doneFlag.resolve()
        }
      }
    }

    doneFlag.blockValue()
    env.waitFor(1024)
  }
}

