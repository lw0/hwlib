package hwlib.xilinx.blackbox

import scala.collection.mutable

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._



class CdcAsyncReset(val activeHigh : Boolean = true, val stageCount : Int = 2, val simInit : Boolean = true) extends BlackBox {
  val macroName = "xpm_cdc_async_rst"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  addGeneric("RST_ACTIVE_HIGH", if (activeHigh) { 1 } else { 0 })
  if (stageCount < 2 || stageCount > 10) {
    SpinalError(s"CdcAsyncReset requires stageCount=${stageCount} between 2 and 10")
  }
  addGeneric("DEST_SYNC_FF", stageCount)
  addGeneric("INIT_SYNC_FF", if (simInit) { 1 } else { 0 })

  val io = new Bundle {
    val src_arst  =  in(Bool)
    val dest_clk  =  in(Bool)
    val dest_arst = out(Bool)
  }

  noIoPrefix()
  addTag(noNumericType)
}

class CdcSyncReset(val stageCount : Int = 2, val simInit : Boolean = true) extends BlackBox {
  val macroName = "xpm_cdc_sync_rst"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  // TODO-lw
  val io = new Bundle {
    val src_arst  =  in(Bool)
    val dest_clk  =  in(Bool)
    val dest_arst = out(Bool)
  }

  noIoPrefix()
  addTag(noNumericType)
}

class CdcIndividual() extends BlackBox {
  val macroName = "xpm_cdc_array_single"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  // TODO-lw
  val io = new Bundle {
  }

  noIoPrefix()
  addTag(noNumericType)
}

class CdcPulse() extends BlackBox {
  val macroName = "xpm_cdc_pulse"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  // TODO-lw
  val io = new Bundle {
  }

  noIoPrefix()
  addTag(noNumericType)
}


class CdcHandshake() extends BlackBox {
  val macroName = "xpm_cdc_handshake"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  // TODO-lw
  val io = new Bundle {
  }

  noIoPrefix()
  addTag(noNumericType)
}


class CdcGray() extends BlackBox {
  val macroName = "xpm_cdc_gray"
  if (globalData.config.mode == VHDL) {
    librariesUsages += "xpm"
    setDefinitionName(s"vcomponents.${macroName}")
  } else {
    setDefinitionName(macroName)
  }

  // TODO-lw
  val io = new Bundle {
  }

  noIoPrefix()
  addTag(noNumericType)
}


