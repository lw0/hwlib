package hwlib.base

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.sim._



class MultiFifo[T <: Data](tPayload : HardType[T], logDepth : Int, tagBits : Int, selBits : Int = 0, syncRead : Boolean = true) extends Component {

  require(tagBits >= 0 && selBits >= 0)
  val remBits = (tagBits - selBits) max 0

  def TTag = UInt(tagBits bits)
  def TSel = UInt(selBits bits)
  def TRem = UInt(remBits bits)

  def cPayloadZero = tPayload().getZero

  case class TagPayload() extends Bundle {
    val data = tPayload()
    val rem = TRem
  }

  val io = new Bundle {
    val iPutTag   =  in(TTag)
    val iPutData  =  in(tPayload())
    val iPutValid =  in(Bool)
    val oPutReady = out(Bool)
    val oPutSel   = out(TSel)

    val iGetSel   =  in(TSel)
    val oGetTag   = out(TTag)
    val oGetData  = out(tPayload())
    val oGetValid = out(Bool)
    val iGetReady =  in(Bool)
  }

  val sPutSel = TSel
  val sPutRem = TRem
  val sGetRem = TRem

  sPutSel := io.iPutTag.asBits.resize(selBits).asUInt
  sPutRem := io.iPutTag.asBits.resizeLeft(remBits).asUInt
  io.oPutSel := sPutSel

  io.oGetTag := (sGetRem.asBits ## io.iGetSel.asBits).resize(tagBits).asUInt

  io.oPutReady := False
  io.oGetData := cPayloadZero
  sGetRem := U(0)
  io.oGetValid := False

  val iArray = for (idx <- 0 until (1 << selBits)) yield new Area {
    val iFifo = new Fifo(TagPayload(), logDepth, syncRead)

    iFifo.io.iPutData.data := cPayloadZero
    iFifo.io.iPutData.rem := sPutRem
    iFifo.io.iPutValid := False
    when (sPutSel === idx) {
      iFifo.io.iPutData.data := io.iPutData
      iFifo.io.iPutValid := io.iPutValid
      io.oPutReady := iFifo.io.oPutReady
    }

    iFifo.io.iGetReady := False
    when (io.iGetSel === idx) {
      io.oGetData := iFifo.io.oGetData.data
      sGetRem := iFifo.io.oGetData.rem
      io.oGetValid := iFifo.io.oGetValid
      iFifo.io.iGetReady := io.iGetReady
    }
  }
}

object MultiFifo extends HwMain[MultiFifo[Bits]] {

  simSuiteWith("Normal")(new MultiFifo(Bits(8 bits), 4, 4, 2, false))
  clockAt(200 MHz)
  simSuiteRun("Normal"){ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val sendFlag = Future[Unit]()
    val recvFlag = Future[Unit]()

    fork {
      dut.io.iPutTag   #= 0
      dut.io.iPutData  #= 0
      dut.io.iPutValid #= false
      env.waitFor()

      for (idx <- 0 until 256) {
        val tag = Random.nextInt(16)
        dut.io.iPutTag #= tag
        dut.io.iPutData #= idx
        dut.io.iPutValid #= true
        env.waitNextWhen(dut.io.oPutReady.toBoolean)
        println(s"SENT ${idx} on ${tag}")
        val delay = Random.nextInt(2)
        if (delay > 0) {
          dut.io.iPutValid     #= false
          env.waitFor(delay)
        }
      }
      sendFlag.resolve()
    }

    fork {
      dut.io.iGetSel       #= 0
      dut.io.iGetReady     #= false

      var count = 256
      while (count > 0) {
        dut.io.iGetSel       #= Random.nextInt(4)
        dut.io.iGetReady     #= true
        env.waitFor()

        if (dut.io.oGetValid.toBoolean) {
          println(s"RECV ${dut.io.oGetData.toInt} on ${dut.io.oGetTag.toInt}")
          count -= 1
          val delay = Random.nextInt(2)
          if (delay > 0) {
            dut.io.iGetReady     #= false
            env.waitFor(delay)
          }
        }
      }

      recvFlag.resolve()
    }

    sendFlag.blockValue()
    recvFlag.blockValue()
  }

  simSuiteWith("NoSel")(new MultiFifo(Bits(8 bits), 4, 4, 0, false))
  clockAt(200 MHz)
  simSuiteRun("NoSel"){ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val sendFlag = Future[Unit]()
    val recvFlag = Future[Unit]()

    fork {
      dut.io.iPutTag   #= 0
      dut.io.iPutData  #= 0
      dut.io.iPutValid #= false
      env.waitFor()

      for (idx <- 0 until 256) {
        val tag = Random.nextInt(16)
        dut.io.iPutTag #= tag
        dut.io.iPutData #= idx
        dut.io.iPutValid #= true
        env.waitNextWhen(dut.io.oPutReady.toBoolean)
        println(s"SENT ${idx} on ${tag}")
        val delay = Random.nextInt(2)
        if (delay > 0) {
          dut.io.iPutValid     #= false
          env.waitFor(delay)
        }
      }
      sendFlag.resolve()
    }

    fork {
      dut.io.iGetReady     #= false

      var count = 256
      while (count > 0) {
        dut.io.iGetReady     #= true
        env.waitFor()

        if (dut.io.oGetValid.toBoolean) {
          println(s"RECV ${dut.io.oGetData.toInt} on ${dut.io.oGetTag.toInt}")
          count -= 1
          val delay = Random.nextInt(2)
          if (delay > 0) {
            dut.io.iGetReady     #= false
            env.waitFor(delay)
          }
        }
      }

      recvFlag.resolve()
    }

    sendFlag.blockValue()
    recvFlag.blockValue()
  }

  simSuiteWith("NoRem")(new MultiFifo(Bits(8 bits), 4, 4, 4, false))
  clockAt(200 MHz)
  simSuiteRun("NoRem"){ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val sendFlag = Future[Unit]()
    val recvFlag = Future[Unit]()

    fork {
      dut.io.iPutTag   #= 0
      dut.io.iPutData  #= 0
      dut.io.iPutValid #= false
      env.waitFor()

      for (idx <- 0 until 256) {
        val tag = Random.nextInt(16)
        dut.io.iPutTag #= tag
        dut.io.iPutData #= idx
        dut.io.iPutValid #= true
        env.waitNextWhen(dut.io.oPutReady.toBoolean)
        println(s"SENT ${idx} on ${tag}")
        val delay = Random.nextInt(2)
        if (delay > 0) {
          dut.io.iPutValid     #= false
          env.waitFor(delay)
        }
      }
      sendFlag.resolve()
    }

    fork {
      dut.io.iGetSel       #= 0
      dut.io.iGetReady     #= false

      var count = 256
      while (count > 0) {
        dut.io.iGetSel       #= Random.nextInt(16)
        dut.io.iGetReady     #= true
        env.waitFor()

        if (dut.io.oGetValid.toBoolean) {
          println(s"RECV ${dut.io.oGetData.toInt} on ${dut.io.oGetTag.toInt}")
          count -= 1
          val delay = Random.nextInt(2)
          if (delay > 0) {
            dut.io.iGetReady     #= false
            env.waitFor(delay)
          }
        }
      }

      recvFlag.resolve()
    }

    sendFlag.blockValue()
    recvFlag.blockValue()


  }
}
