package hwlib.base

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.HwMain
import hwlib.sim._



class SplitFifo[T <: Data](tPayload : HardType[T], logDepth : Int, idBits : Int) extends Component {
  val dataBits = widthOf(tPayload())
  val logDataBits = log2Up(dataBits)
  val idxBits = logDepth + idBits
  require(dataBits > 0)
  require(idBits > 0 && idBits <= 7) // up to RAM128X1D available (xilinx platform dependent)
  require(logDepth > 0 && (idxBits + logDataBits) <= 16) // up to two cascaded RAMB36

  def TId = UInt(idBits bits)
  def TPtr = UInt(logDepth bits)
  def TIdx = UInt (idxBits bits)

  val io = new Bundle {
    val iPutId     =  in(TId)
    val oPutFull   = out(Bool)
    val iPutData   =  in(tPayload())
    val iPutStrobe =  in(Bool)

    val iGetId     =  in(TId)
    val oGetEmpty  = out(Bool)
    val iGetStrobe =  in(Bool)
    val oGetData   = out(tPayload()) // follows (iGetStrobe & !oGetEmpty) by n cycles
  }

  val sPutWrPtr = TPtr
  val sPutRdPtr = TPtr
  val sPutFull = Bool
  val sPutWrUpdate = Bool
  val sPutWrIndex = TIdx

  val sGetWrPtr = TPtr
  val sGetRdPtr = TPtr
  val sGetEmpty = Bool
  val sGetRdUpdate = Bool
  val sGetRdIndex = TIdx


  val mWrPtrs = Mem(TPtr, Array.fill(1 << idBits)(U(0, logDepth bits)))
  sPutWrPtr := mWrPtrs.readAsync(io.iPutId)
  sGetWrPtr := mWrPtrs.readAsync(io.iGetId)

  val mRdPtrs = Mem(TPtr, Array.fill(1 << idBits)(U(0, logDepth bits)))
  sPutRdPtr := mRdPtrs.readAsync(io.iPutId)
  sGetRdPtr := mRdPtrs.readAsync(io.iGetId)


  sPutWrIndex  := (io.iPutId ## sPutWrPtr).asUInt
  sPutFull  := (sPutWrPtr + 1) === sPutRdPtr
  sPutWrUpdate := io.iPutStrobe && !sPutFull

  mWrPtrs.write(io.iPutId, sPutWrPtr + 1, sPutWrUpdate)


  sGetRdIndex := (io.iGetId ## sGetRdPtr).asUInt
  sGetEmpty := sGetWrPtr === sGetRdPtr
  sGetRdUpdate := io.iGetStrobe && !sGetEmpty

  mRdPtrs.write(io.iGetId, sGetRdPtr + 1, sGetRdUpdate)


  val mData   = Mem(tPayload, 1 << idxBits)

  mData.write(sPutWrIndex, io.iPutData, sPutWrUpdate)
  io.oPutFull  := sPutFull

  io.oGetData  := mData.readSync(sGetRdIndex, sGetRdUpdate)
  io.oGetEmpty := sGetEmpty
}

object SplitFifo extends HwMain[SplitFifo[Bits]] {

  simWith(new SplitFifo(Bits(4 bits), 4, 6))
  clockAt(200 MHz)
  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val sendDone = Future[Unit]()
    val recvDone = Future[Unit]()

    val delayQueueCfg = DelayQueueConfig(
          dPut = RateGen(0.8),
          dLat = DelayGen(4.0),
          dGet = RateGen(0.8),
          capacity = 12,
          reorder = false)
    val queue = new DelayQueue[(Int, Int)](delayQueueCfg)

    val itemCount = 256


    fork {
      // Put process
      dut.io.iPutId     #= 0
      dut.io.iPutData   #= 0
      dut.io.iPutStrobe #= false
      env.waitFor()

      for (idx <- 0 until itemCount) {
          val id = Random.nextInt(4)
        val data = Random.nextInt(16)
        queue.bput((id, data), id)
        env.dbg.print(f"PUT ${data}%x @${id}%d")
        dut.io.iPutId #= id
        dut.io.iPutData #= data
        dut.io.iPutStrobe #= true
        env.waitFor()
        dut.io.iPutStrobe #= false
      }

      sendDone.resolve()
    }

    fork {
      // Get process
      dut.io.iGetId     #= 0
      dut.io.iGetStrobe #= false
      env.waitFor()

      var lastItem : Option[(Int, Int)] = None
      for (idx <- 0 until itemCount) {
        val item = queue.bget().get
        dut.io.iGetId #= item._1
        dut.io.iGetStrobe #= true
        env.waitFor()
        if (lastItem.isDefined) {
          val refId = lastItem.get._1
          val refData = lastItem.get._2
          val dutData = dut.io.oGetData.toInt
          if (refData == dutData) {
            env.dbg.pass(f"${dutData}%x == ${refData}%x @${refId}%d")
          } else {
            env.dbg.fail(f"${dutData}%x != ${refData}%x @${refId}%d")
          }
          lastItem = None
        }
        lastItem = Some(item)
        dut.io.iGetStrobe #= false
      }

      recvDone.resolve()
    }

    sendDone.blockValue()
    recvDone.blockValue()
  }

}


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
