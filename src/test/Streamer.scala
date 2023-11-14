package hwlib.test

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._


import hwlib.HwMain
import hwlib.amba.{AxiConfig, TAxi, ApbConfig}
import hwlib.comp.{AxiApbBridge, AxiReader, AxiWriter}
import hwlib.sim._
import hwlib.sim.amba._



class StreamLoop extends Component {
  // val cfgReg  = AxiConfig(addrBits = 32, dataBytes = 4, hasBurst = false)
  // val cfgMem  = AxiConfig(addrBits = 32, dataBytes = 16, idBits=1)
  val cfgReg  = AxiConfig(addrBits=32, dataBytes=4,  idBits=0,
                          hasBurst=false, hasProt=true)
  val cfgMem  = AxiConfig(addrBits=32, dataBytes=16, idBits=2,
                          hasBurst=true, hasProt=true,
                          hasLock=true, hasCache=true,
                          hasQos=true, hasRegion=true)

  val io = new Bundle {
    val sReg_16    =  slave(TAxi(cfgReg))
    val mMem_32    = master(TAxi(cfgMem))
    val oRd0Done_H =    out(Bool)
    val oRd1Done_H =    out(Bool)
    val oWr0Done_H =    out(Bool)
    val oWr1Done_H =    out(Bool)
  }

  val iBridge = new AxiApbBridge(cfgReg,
       0x20000000L -> 8L,
       0x20000008L -> 8L)
  iBridge.io.sAxi << io.sReg_16

  val iReader = new AxiReader(cfgMem, ApbConfig.fromAxi(cfgReg), Seq(6, 6))
  iReader.io.mMem >> io.mMem_32
  iReader.io.sReg << iBridge.io.mApbs(0)
  io.oRd0Done_H := iReader.io.oIntr(0)
  io.oRd1Done_H := iReader.io.oIntr(1)

  val iWriter = new AxiWriter(cfgMem, ApbConfig.fromAxi(cfgReg), Seq(6, 6))
  iWriter.io.mMem >> io.mMem_32
  iWriter.io.sReg << iBridge.io.mApbs(1)
  io.oWr0Done_H := iWriter.io.oIntr(0)
  io.oWr1Done_H := iWriter.io.oIntr(1)

  iReader.io.mStm(0) >> iWriter.io.sStm(0)
  iReader.io.mStm(1) >> iWriter.io.sStm(1)
}


object Streamer extends HwMain[StreamLoop] {
  genWith(new StreamLoop)


  postGen{ (args, report) =>
    report.printPruned()
    println(" ===== Specs =====")
  }
}

