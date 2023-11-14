package hwlib.comp

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.amba.{AxiConfig, TAxi, Amba}
import hwlib.base.Fifo
import hwlib.sim._
import hwlib.sim.amba._



class AxiIdMirror(val cfg : AxiConfig, val logDepth : Int = 3) extends Component {
  val cfgNoId = cfg.copy(idBits = 0)

  val io = new Bundle {
    val sAxi =  slave(TAxi(cfg))
    val mAxi = master(TAxi(cfgNoId))
  }

  val iWrIdFifo = ifGen(cfg.hasId && cfg.hasWr) { new Fifo(cfg.TId, logDepth, syncRead=false) }
  if (cfg.hasId && cfg.hasWr) {
    iWrIdFifo.io.iPutData := io.sAxi.aw.id
    iWrIdFifo.io.iPutValid := io.sAxi.aw.valid && io.mAxi.aw.ready
    io.sAxi.aw.ready := io.mAxi.aw.ready && iWrIdFifo.io.oPutReady
    io.mAxi.aw.valid := io.sAxi.aw.valid && iWrIdFifo.io.oPutReady
    io.mAxi.aw.payload << io.sAxi.aw.payload

    io.mAxi.w << io.sAxi.w

    iWrIdFifo.io.iGetReady := io.sAxi.b.ready && io.mAxi.b.valid
    io.sAxi.b.valid := io.mAxi.b.valid && iWrIdFifo.io.oGetValid
    io.mAxi.b.ready := io.sAxi.b.ready && iWrIdFifo.io.oGetValid
    io.sAxi.b.payload << io.mAxi.b.payload
    io.sAxi.b.id.allowOverride
    io.sAxi.b.id := iWrIdFifo.io.oGetData
  }

  val iRdIdFifo = ifGen(cfg.hasId && cfg.hasRd) { new Fifo(cfg.TId, logDepth, syncRead=false) }
  if (cfg.hasId && cfg.hasRd) {
    iRdIdFifo.io.iPutData := io.sAxi.ar.id
    iRdIdFifo.io.iPutValid := io.sAxi.ar.valid && io.mAxi.ar.ready
    io.sAxi.ar.ready := io.mAxi.ar.ready && iRdIdFifo.io.oPutReady
    io.mAxi.ar.valid := io.sAxi.ar.valid && iRdIdFifo.io.oPutReady
    io.mAxi.ar.payload << io.sAxi.ar.payload

    // TODO-lw handle read channel relying on last flag!
    if (cfg.hasBurst) {
      iRdIdFifo.io.iGetReady := io.sAxi.r.ready && io.mAxi.r.valid && io.mAxi.r.last
    } else {
      iRdIdFifo.io.iGetReady := io.sAxi.r.ready && io.mAxi.r.valid
    }
    io.sAxi.r.valid := io.mAxi.r.valid && iRdIdFifo.io.oGetValid
    io.mAxi.r.ready := io.sAxi.r.ready && iRdIdFifo.io.oGetValid
    io.sAxi.r.payload << io.mAxi.r.payload
    io.sAxi.r.id.allowOverride
    io.sAxi.r.id := iRdIdFifo.io.oGetData
  }

  if (!cfg.hasId) {
    io.mAxi << io.sAxi
  }
}

object AxiIdMirror extends HwMain[AxiIdMirror] {

  simSuiteWith("AxiWithId")(new AxiIdMirror(AxiConfig(addrBits = 16, dataBytes = 4, hasBurst = true, idBits = 4)))
  clockAt(200 MHz)
  simSuiteRun("AxiWithId"){ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val masterCfg = AxiModelConfig()
    val iWrMaster = new AxiWrMaster(dut.io.sAxi, masterCfg)
    val iRdMaster = new AxiRdMaster(dut.io.sAxi, masterCfg)
    val slaveCfg = AxiModelConfig(dataRate=RateGen(0.5))
    val iWrSlave = new AxiWrSlave(dut.io.mAxi, slaveCfg)
    val iRdSlave = new AxiRdSlave(dut.io.mAxi, slaveCfg)
    val iMemBuf = new MemBuffer(12)
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
    val testValue = BigInt("ffffeeeeddddccccbbbbaaaa9999888877776666555544443333222211110000", 16)
    val reqA = iWrMaster.writeOn(0x1, 0x00a, testValue, 16, Size.S2B, Burst.Wrap)
    val reqB = iWrMaster.writeOn(0x2, 0x124, testValue, 8, Burst.Wrap)
    val reqC = iRdMaster.readOn(0x1, 0x104, 256, Burst.Incr)
    val reqD = iRdMaster.readOn(0x3, 0x003, 16, Size.S1B, Burst.Wrap)

    reqA.blockValue()
    reqB.blockValue()
    reqC.blockValue()
    reqD.blockValue()

    env.clk.waitSampling(4)
  }

  // TODO-lw add AxiLiteWithId test with Request reordering

  // TODO-lw add AxiNoId test for the idBits=0 passthrough case

}
