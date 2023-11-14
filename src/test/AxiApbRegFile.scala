package hwlib.test

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.amba.{AxiConfig, TAxi, ApbConfig}
import hwlib.comp.{AxiApbBridge, ApbRegFile}
import hwlib.sim._
import hwlib.sim.amba._




class AxiApbRegFile_TB extends Component {
  val axiCfg   = AxiConfig(addrBits=12, dataBytes=4, hasStrb=false, hasBurst=false)
  val apbCfg16 = ApbConfig.fromAxi(axiCfg).copy(addrBits=4)
  val apbCfg8  = ApbConfig.fromAxi(axiCfg).copy(addrBits=3)
  val apbCfg4  = ApbConfig.fromAxi(axiCfg).copy(addrBits=2)

  val io = new Bundle {
    val sAxi = slave(TAxi(axiCfg))
  }

  val iBridge = new AxiApbBridge(axiCfg,
       0L -> 16L,
       8L -> 16L,
      28L ->  4L,
      32L ->  8L)
  iBridge.io.sAxi << io.sAxi

  val iRegsA = new ApbRegFile(apbCfg16)
  iRegsA.io.sApb << iBridge.io.mApbs(0)
  iRegsA.io.iReg := iRegsA.io.oReg

  val iRegsB = new ApbRegFile(apbCfg16)
  iRegsB.io.sApb << iBridge.io.mApbs(1)
  iRegsB.io.iReg := iRegsB.io.oReg

  val iRegsC = new ApbRegFile(apbCfg4)
  iRegsC.io.sApb << iBridge.io.mApbs(2)
  iRegsC.io.iReg := iRegsC.io.oReg

  val iRegsD = new ApbRegFile(apbCfg8)
  iRegsD.io.sApb << iBridge.io.mApbs(3)
  iRegsD.io.iReg := iRegsD.io.oReg
}

object AxiApbRegFile_TB extends HwMain[AxiApbRegFile_TB] {

  simWith(new AxiApbRegFile_TB())

  clockAt(200 MHz)

  simRun("test") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)
    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    val masterCfg = AxiModelConfig()
    val iWrMaster = new AxiWrMaster(dut.io.sAxi, masterCfg)
    val iRdMaster = new AxiRdMaster(dut.io.sAxi, masterCfg)

    println("=== Begin ===")
    env.clk.waitSampling()

    val reqRd = (0 until 40).map((idx) => iRdMaster.read(idx << dut.axiCfg.fullSize))

    val reqWr = (0 until 40).map((idx) => iWrMaster.write(idx << dut.axiCfg.fullSize, (Random.nextInt(256) << 8) | idx))

    for (idx <- 0 until 40) {
      val resRd = reqRd(idx).blockValue()
      println(s"Rd #${idx}: ${resRd.resp} (${resRd.data})")
      val resWr = reqWr(idx).blockValue()
      println(s"Wr #${idx}: ${resWr.resp}")
    }
    println("=== End ===")
    env.clk.waitSampling(32)
  }
}


