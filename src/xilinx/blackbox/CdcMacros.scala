package hwlib.xilinx.blackbox

import scala.collection.mutable

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._

import spinal.lib.fsm._


case class CdcResetConfig(
    async : Boolean = true,
    activeHigh : Boolean = true,
    destStages : Int = 4,
    simAssert : Boolean = true,
    simInit : Boolean = false) {
  require(destStages >= 2 && destStages <= 4)

  def activeHighValue = if (activeHigh) { 1 } else { 0 }
  // sync reset should be initially active
  def initValue = if (activeHigh) { 1 } else { 0 }

  def simInitValue = if (simInit) { 1 } else { 0 }
  def simAssertValue = if (simAssert) { 1 } else { 0 }
}

class CdcReset(val cfg : CdcResetConfig, val dstClock : ClockDomain = null) extends BlackBox {

  val macroName = if (cfg.async) { "xpm_cdc_async_rst" } else { "xpm_cdc_sync_rst" }
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  addGeneric("DEST_SYNC_FF", cfg.destStages)
  addGeneric("INIT_SYNC_FF", cfg.simInitValue)

  if (cfg.async) {
    addGeneric("RST_ACTIVE_HIGH", cfg.activeHighValue)
  } else {
    addGeneric("INIT", cfg.initValue)
    addGeneric("SIM_ASSERT_CHK", cfg.simAssertValue)
  }

  val io = new Bundle {
    val src_rst  =  in(Bool)
    val dest_rst = out(Bool)
    val dest_clk =  in(Bool)
  }

  val clock = if (null != dstClock) { dstClock } else { ClockDomain.current }
  mapClockDomain(clock, io.dest_clk)

  noIoPrefix()
  addTag(noNumericType)
}


case class CdcIndividualConfig(
    width : Option[Int],
    srcStages : Int = 1,
    destStages : Int = 4,
    simInit : Boolean = false,
    simAssert : Boolean = true) {
  require(!width.isDefined || width.get > 0 && width.get <= 1024)
  require(srcStages >= 0 && srcStages <= 1)
  require(destStages >= 2 && destStages <= 10)

  def isArray = width.isDefined
  def TPayload = Bits(width.getOrElse(0) bits)
  def hasSrcClock = srcStages != 0

  def simInitValue = if (simInit) { 1 } else { 0 }
  def simAssertValue = if (simAssert) { 1 } else { 0 }
}

class CdcIndividual(cfg : CdcIndividualConfig, dstClock : ClockDomain = null, srcClock : ClockDomain = null) extends BlackBox {
  val macroName = if (cfg.isArray) { "xpm_cdc_array_single" } else { "xpm_cdc_single" }
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  if (cfg.isArray) {
    addGeneric("WIDTH", cfg.width.get)
  }
  addGeneric("SRC_INPUT_REG", cfg.srcStages)
  addGeneric("DEST_SYNC_FF", cfg.destStages)
  addGeneric("INIT_SYNC_FF", cfg.simInitValue)
  addGeneric("SIM_ASSERT_CHK", cfg.simAssertValue)

  val io = new Bundle {
    val src_clk = in(Bool) default False

    val dest_clk = in(Bool)

    val iSrcPayload =  cfg.isArray generate  in(cfg.TPayload)
    val oDstPayload =  cfg.isArray generate out(cfg.TPayload)
    val iSrcSignal  = !cfg.isArray generate  in(Bool)
    val oDstSignal  = !cfg.isArray generate out(Bool)
  }

  if (cfg.isArray) {
    io.iSrcPayload.setName("src_in")
    io.oDstPayload.setName("dest_out")
  } else {
    io.iSrcSignal.setName("src_in")
    io.oDstSignal.setName("dest_out")
  }

  if (cfg.hasSrcClock) {
    if (null != srcClock) {
      mapClockDomain(srcClock, io.src_clk)
    } else {
      mapClockDomain(ClockDomain.current, io.src_clk)
    }
  }

  if (null != dstClock) {
    mapClockDomain(dstClock, io.dest_clk)
  } else {
    mapClockDomain(ClockDomain.current, io.dest_clk)
  }

  noIoPrefix()
  addTag(noNumericType)
}


case class CdcHandshakeConfig(
    width : Int,
    extern : Boolean = false,
    srcStages : Int = 4,
    destStages : Int = 4,
    simAssert : Boolean = true,
    simInit : Boolean = false) {
  require(srcStages >= 2 && srcStages <= 10)
  require(destStages >= 2 && destStages <= 10)

  def TPayload = Bits(width bits)
}

class CdcHandshake(cfg : CdcHandshakeConfig, srcClock : ClockDomain = null, dstClock : ClockDomain = null) extends BlackBox {
  val macroName = "xpm_cdc_handshake"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  addGeneric("DEST_EXT_HSK", (if (cfg.extern) {1} else {0}))
  addGeneric("DEST_SYNC_FF", cfg.destStages)
  addGeneric("INIT_SYNC_FF", (if (cfg.simInit) {1} else {0}))
  addGeneric("SIM_ASSERT_CHK", (if (cfg.simAssert) {1} else {0}))
  addGeneric("SRC_SYNC_FF", cfg.srcStages)
  addGeneric("WIDTH", cfg.width)

  val io = new Bundle {
    val src_clk  =  in(Bool)

    val dest_clk =  in(Bool)

    val iSrcPayload =  in(cfg.TPayload)
    val iSrcReq     =  in(Bool)
    val oSrcAck     = out(Bool)

    val oDstPayload = out(cfg.TPayload)
    val oDstReq     = out(Bool)
    val iDstAck     =  in(Bool) default False
  }

  io.iSrcPayload.setName("src_in")
  io.iSrcReq.setName("src_send")
  io.oSrcAck.setName("src_rcv")
  io.oDstPayload.setName("dest_out")
  io.oDstReq.setName("dest_req")
  io.iDstAck.setName("dest_ack")

  if (null != srcClock) {
    mapClockDomain(srcClock, io.src_clk)
  } else {
    mapClockDomain(ClockDomain.current, io.src_clk)
  }

  if (null != dstClock) {
    mapClockDomain(dstClock, io.dest_clk)
  } else {
    mapClockDomain(ClockDomain.current, io.dest_clk)
  }

  noIoPrefix()
  addTag(noNumericType)
}

class CdcHandshakeAuto(cfg : CdcHandshakeConfig, srcClock : ClockDomain = null, dstClock : ClockDomain = null) extends Component {

  val io = new Bundle {
    val iSrcEnable  =  in(Bool) default True
    val iSrcPayload =  in(cfg.TPayload)
    val oSrcAck    = out(Bool)

    val oDstPayload = out(cfg.TPayload)
    val oDstReq    = out(Bool)
  }

  val iHandshake = new CdcHandshake(cfg.copy(extern=false), srcClock, dstClock)
  iHandshake.io.iSrcPayload := io.iSrcPayload

  val iFsm = new StateMachine {
    io.oSrcAck := False
    iHandshake.io.iSrcReq := False
    val Idle : State = new State with EntryPoint {
      whenIsActive {
        when(!iHandshake.io.oSrcAck && io.iSrcEnable) {
          goto(Sending)
        }
      }
    }

    val Sending : State = new State {
      whenIsActive {
        iHandshake.io.iSrcReq := True
        when (iHandshake.io.oSrcAck) {
          io.oSrcAck := True
          goto(Waiting)
        }
      }
    }

    val Waiting : State = new State {
      whenIsActive {
        when (!iHandshake.io.oSrcAck) {
          when(io.iSrcEnable) {
            goto(Sending)
          } otherwise {
            goto(Idle)
          }
        }
      }
    }
  }

  io.oDstPayload := iHandshake.io.oDstPayload
  io.oDstReq := iHandshake.io.oDstReq
}


case class CdcPulseConfig(
    withReset : Boolean = true,
    dstReg : Boolean = false,
    dstStages : Int = 4,
    simInit : Boolean = false,
    simAssert : Boolean = true) {
  require(dstStages >= 2 && dstStages <= 10)

  def withResetValue = if (withReset) { 1 } else { 0 }
  def dstRegValue = if (dstReg) { 1 } else { 0 }

  def simInitValue = if (simInit) { 1 } else { 0 }
  def simAssertValue = if (simAssert) { 1 } else { 0 }
}

class CdcPulse(val cfg : CdcPulseConfig, srcClock : ClockDomain = null, dstClock : ClockDomain = null) extends BlackBox {
  val macroName = "xpm_cdc_pulse"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  addGeneric("DEST_SYNC_FF", cfg.dstStages)
  addGeneric("INIT_SYNC_FF", cfg.simInitValue)
  addGeneric("SIM_ASSERT_CHK", cfg.simAssertValue)
  addGeneric("REG_OUTPUT", cfg.dstRegValue)
  addGeneric("RST_USED", cfg.withResetValue)

  val io = new Bundle {
    val src_clk = in(Bool)
    val src_rst = in(Bool) default False

    val dest_clk = in(Bool)
    val dest_rst = in(Bool) default False

    val iSrcPulse = in(Bool)
    val oDstPulse = out(Bool)
  }

  io.iSrcPulse.setName("src_pulse")
  io.oDstPulse.setName("dest_pulse")

  val useSrcClock = if (null != srcClock) { srcClock } else { ClockDomain.current }
  if (cfg.withReset) {
    // TODO-lw assert correct clock domain reset type (config)
    mapClockDomain(useSrcClock, io.src_clk, io.src_rst)
  } else {
    mapClockDomain(useSrcClock, io.src_clk)
  }

  val useDstClock = if (null != dstClock) { dstClock } else { ClockDomain.current }
  if (cfg.withReset) {
    // TODO-lw assert correct clock domain reset type (config)
    mapClockDomain(useDstClock, io.dest_clk, io.dest_rst)
  } else {
    mapClockDomain(useDstClock, io.dest_clk)
  }

  noIoPrefix()
  addTag(noNumericType)
}


// TODO-lw
class CdcGray() extends BlackBox {
  val macroName = "xpm_cdc_gray"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  val io = new Bundle {
  }

  noIoPrefix()
  addTag(noNumericType)
}


