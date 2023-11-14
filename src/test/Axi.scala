package hwlib.test

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.amba.{AxiConfig, TAxi}
import hwlib.sim._
import hwlib.sim.amba._

import Burst.Burst
import Size.Size
import Resp.Resp



class Axi_TB(cfg : AxiConfig) extends Component {

  val io = new Bundle {
    val sMem = slave(TAxi(cfg))
    val mMem = master(TAxi(cfg))
  }
  io.sMem.nameThis()
  io.mMem.nameThis()

  //val iPlug = io.sMem.plugFsm()
  //io.mMem.assign(false)
  val cAwBuf = io.sMem.aw.bufferToFsm(io.mMem.aw)
  val cWBuf = io.sMem.w.bufferToFsm(io.mMem.w)
  val cBBuf = io.mMem.b.bufferToFsm(io.sMem.b)
  val cArBuf = io.sMem.ar.bufferToFsm(io.mMem.ar)
  val cRBuf = io.mMem.r.bufferToFsm(io.sMem.r)
}

object Axi_TB extends HwMain[Axi_TB] {

  val cfg = AxiConfig(addrBits = 32, dataBytes = 4)

  simWith(new Axi_TB(cfg))

  clockAt(200 MHz)

  simRun("test") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    val masterCfg = AxiModelConfig()
    val iWrMaster = new AxiWrMaster(dut.io.sMem, masterCfg)
    val iRdMaster = new AxiRdMaster(dut.io.sMem, masterCfg)
    val slaveCfg = AxiModelConfig(dataRate=RateGen(0.5))
    val iWrSlave = new AxiWrSlave(dut.io.mMem, slaveCfg)
    val iRdSlave = new AxiRdSlave(dut.io.mMem, slaveCfg)

    val iMemBuf = new MemBuffer(12)
    for (i <- 0 until 0x100) {
      iMemBuf.writeInt(i*4, i | (i << 12))
    }
    iWrSlave.onWrite(0x000, 0x1000) {
      (addr, bytes, data, mask) =>
        iMemBuf.writeMask(addr, bytes, data, mask)
        Resp.Okay
    }
    iRdSlave.onRead(0x000, 0x1000) {
      (addr, bytes) =>
        val data = iMemBuf.read(addr, bytes)
        (Resp.Okay, data)
    }

    env.clk.waitSampling()
    println("=== Begin ===")
    val reqA = iWrMaster.write(0x100, 0x6c)
    val reqB = iRdMaster.read(0x104)
    val reqC = iRdMaster.read(0x0C0, 256, Size.S4B, Burst.Incr)
    val testValue = BigInt("ffffeeeeddddccccbbbbaaaa9999888877776666555544443333222211110000", 16)
    val reqD = iWrMaster.write(0x40a, testValue, 16, Size.S2B, Burst.Wrap)
    val reqE = iWrMaster.write(0x524, testValue, 8, Burst.Wrap)
    val reqF = iRdMaster.read(0xC000)
    syncWr(s"write(0x0100, 0x6c) -> ", reqA)
    syncRd(s"read(0x0104) -> ", reqB)
    syncRd(s"read(0x00C0) -> ", reqC)
    syncWr(s"write(0x040a, Wrap 2x16) -> ", reqD)
    syncWr(s"write(0x0524,Wrap 4x8) -> ", reqE)
    syncRd(s"read(0xC000) -> ", reqF)
    println("=== End ===")
    env.clk.waitSampling(32)
    println(iMemBuf.dump(0x000, 0x600))

    testBurst(AxiConfig(addrBits=16, dataBytes=8), 0x1f5, 37, Size.S2B, Burst.Incr)
  }

  def syncWr(msg : String, fut : Future[AxiWrResult]) : Unit = {
    val wres = fut.blockValue()
    println(s"[${simTime/1000}ns] ${msg}${wres.resp}")
  }

  def syncRd(msg : String, fut : Future[AxiRdResult]) : Unit = {
    val rres = fut.blockValue()
    println(s"[${simTime/1000}ns] ${msg}${rres.resp}: 0x${rres.data.toString(16)}")
  }

  def testBurst(config : AxiConfig, base : Long, len : Int, size : Size, burst : Burst) : Unit = {
    implicit val cfg = config
    val apack = APack(base, len, size, burst, 0)
    println(s"Burst for $apack")
    apack.genBurst{
      (idx : Int, addr : Long, dpos : Int, bpos : Int, cnt : Int, last : Boolean) =>
        val partL = (0 until (config.dataBytes-bpos-cnt)).map(i => "-").mkString("")
        val partM = (0 until (cnt)).map(i => "v").mkString("")
        val partR = (0 until (bpos)).map(i => "-").mkString("")
        println(f"T${idx}%03d: D(${dpos+cnt-1}%3d .. ${dpos}%3d) -> ${partL}${partM}${partR} @${addr.toHexString}")
    }
  }
}


