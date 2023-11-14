package hwlib.test

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.amba.{AxiStreamConfig, TAxiStream}
import hwlib.sim._
import hwlib.sim.amba._



class AxiStream_TB(cfg : AxiStreamConfig) extends Component {

 val io = new Bundle {
   val sStm = slave(TAxiStream(cfg))
   val mStm = master(TAxiStream(cfg))
 }
 io.sStm.nameThis()
 io.mStm.nameThis()
 noIoPrefix()

 val cTBuf = io.sStm.t.bufferToFsm(io.mStm.t)
}


object AxiStream_TB extends HwMain[AxiStream_TB] {

  val cfg = AxiStreamConfig(dataBytes = 8, hasKeep = true, hasStrb = true, idBits = 4, destBits = 4)

  simWith(new AxiStream_TB(cfg))

  clockAt(200 MHz)
  simRun("test") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val iStmM = new AxiStreamMaster(dut.io.sStm, AxiModelConfig(dataRate=RateGen(1.0), issuePrio=true))
    val iStmS = new AxiStreamSlave(dut.io.mStm, AxiModelConfig(dataRate=RateGen(1.0)))

    iStmS.onRecvFrom(2) {
     (pkg) =>
       println(s"Dest 2 Received ${pkg}")
       println(s"i.e. ${pkg.onlyShorts().map(_.toHexString).mkString(" ")}")
    }
    iStmS.onRecv {
     (pkg) =>
       println(s"Received ${pkg}")
    }

    val testA = "Heute ist ein schöner Tag".getBytes()
    val testB = Array[Int](0x14131211, 0x24232221, 0x34333231, 0x44434241)
    val testC = Array[Short](0x0111, 0x0222, 0x0333, 0x0444, 0x0555, 0x0666, 0x0777, 0x0888, 0x0999, 0x0aaa, 0x0bbb, 0x0ccc, 0x0ddd, 0x0eee, 0x0fff)
    val testD = Random.nextBytes(256)

    env.clk.waitSampling()
    println("=== Begin ===")
    iStmM.send(testA, nullBytes=AverageRate(0.8), dest=0, id=0)
    iStmM.sendInts(testB, posBytes=FixedRotate(dut.io.sStm.cfg.dataBytes, 4), dest=2, id=0)
    iStmM.sendShorts(testC, posBytes=FixedRotate(dut.io.sStm.cfg.dataBytes, 2), dest=2, id=1)
    iStmM.send(testD, dest=0, id=0)
    println("=== End ===")
    env.clk.waitSampling(1024)
  }
}

