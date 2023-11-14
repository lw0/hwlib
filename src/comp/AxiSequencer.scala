package hwlib.comp

import scala.util.Random

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.amba.AxiConfig
import hwlib.base.Mask
import hwlib.sim._
import hwlib.HwMain


class AxiSequencer(val cfg : AxiConfig, singleError : Boolean = false) extends Component {
  require(cfg.hasBurst)

  val io = new Bundle {
    val iAddr   =  in(cfg.TAddr)
    val iLen    =  in(cfg.TLen)
    val iSize   =  in(cfg.TSize)
    val iBurst  =  in(cfg.TBurst)
    val iStart  =  in(Bool)

    val oActive = out(Bool)

    val oAddr   = out(cfg.TAddr)
    val oWAddr  = out(cfg.TWAddr)
    val oWIdx   = out(cfg.TWIdx)
    val oMask   = out(cfg.TMask)
    val oBegin  = out(Bool)
    val oEnd    = out(Bool)
    val oLast   = out(Bool)
    val oError  = out(Bool)
    val iNext   =  in(Bool)
  }

  /// Checks
  // when cfg.hasStrb : (iSize <= cfg.fullSize) else (iSize == cfg.fullSize)
  // when iBurst == WRAP : (iAddr aligned to iSize)
  // when iBurst == WRAP : (iLen in (2, 4, 8, 16) i.e. (1, 3, 7, 15))
  // when iBurst == FIXED : (iLen <= 16 i.e. 15)
  // when iBurst == INCR : (iLen <= maxlen)
  val sSizeValid = if (cfg.hasStrb) {
    io.iSize <= cfg.fullSize
  } else {
    io.iSize === cfg.fullSize
  }
  val sReach = io.iLen.resize(15) << io.iSize // bytes covered in this transaction
  val sBound = ~io.iAddr((cfg.bound - 1) downto 0) // bytes until boundary is reached
  val sLenIncrValid = sReach <= sBound
  val sLenWrapValid = io.iLen === 1 || io.iLen === 3 || io.iLen === 7 || io.iLen === 15
  // TODO-lw currently not checking for iAddr alignment in Wrap case, but it causes no problems anyway
  val sLenFixedValid = io.iLen <= 15
  ///

  object State extends SpinalEnum(binarySequential) {
    val Error, Fixed, Incr, Wrap = newElement()
  }

  val rActive    = Reg(Bool) init False
  val rState     = Reg(State())
  val rCounter   = Reg(cfg.TLen)

  val rAddr      = Reg(cfg.TAddr)
  val rSizeBit   = Reg(UInt((cfg.fullSize + 1) bits))
  val rSizeMask  = Reg(UInt( cfg.fullSize      bits))
  val rWrapMask  = Reg(UInt((cfg.fullSize + 4) bits))
  val rFirst     = Reg(Bool)

  val rPrevWAddrBit = Reg(Bool)

  val sNextAddr = cfg.TAddr

  when (!rActive) {
    when(io.iStart) {
      rActive := True
      rCounter := io.iLen
      rAddr := io.iAddr
      rSizeBit  := Mask.BitOneU(io.iSize, cfg.fullSize + 1)
      rSizeMask := Mask.RightOnesU(io.iSize, cfg.fullSize)
      rWrapMask := Mask.RangeOnesU(io.iSize, CountOne(io.iLen), cfg.fullSize + 4)
      rFirst := True
      when ((io.iBurst === cfg.cBurstFixed) && sLenFixedValid && sSizeValid) {
        rState := State.Fixed
      } elsewhen ((io.iBurst === cfg.cBurstWrap) && sLenWrapValid && sSizeValid) {
        rState := State.Wrap
      } elsewhen ((io.iBurst === cfg.cBurstIncr) && sLenIncrValid && sSizeValid) {
        rState := State.Incr
      } otherwise {
        if (singleError) {
          rCounter := 0
        }
        rState := State.Error
      }
    }
  } otherwise {
    when (io.iNext) {
      rFirst := False
      rAddr := sNextAddr
      rPrevWAddrBit := rAddr(cfg.fullSize)
      when (rCounter === 0) {
        rActive := False
      } otherwise {
        rCounter := rCounter - 1
      }
    }
  }

  val sLaneFirst = rAddr.takeLow(cfg.fullSize).asUInt
  val sLaneCount = rSizeBit - (sLaneFirst & rSizeMask)
  val sMask = Mask.RangeOnes(sLaneFirst, sLaneCount, cfg.dataBytes)

  val sNextWAddrBit = sNextAddr(cfg.fullSize)
  val sWAddrBit = rAddr(cfg.fullSize)
  val sLast = rCounter === 0
  val sBegin = rState.mux(
    State.Error -> False,
    State.Fixed -> True,
    default     -> ((sWAddrBit =/= rPrevWAddrBit) || rFirst))
  val sEnd = rState.mux(
    State.Error -> False,
    State.Fixed -> True,
    default     -> ((sNextWAddrBit =/= sWAddrBit) || sLast))

  val sError = rState.mux(
    State.Error -> True,
    default     -> False)

  val sAlignMask = ~rSizeMask.resize(cfg.addrBits)
  val sWrapMask = rWrapMask.resize(cfg.addrBits)
  sNextAddr := rState.mux(
    State.Incr ->  ((rAddr + rSizeBit)                                   & sAlignMask),
    State.Wrap -> (((rAddr + rSizeBit) & sWrapMask | rAddr & ~sWrapMask) & sAlignMask),
    default    ->    rAddr)

  io.oActive := rActive
  io.oAddr   := rAddr
  io.oWAddr  := rAddr.takeHigh(cfg.waddrBits).asUInt
  io.oWIdx   := sLaneFirst
  io.oMask   := Mask.RangeOnes(sLaneFirst, sLaneCount, cfg.dataBytes)
  io.oBegin  := sBegin
  io.oEnd    := sEnd
  io.oLast   := sLast
  io.oError  := sError
}


object AxiSequencer extends HwMain[AxiSequencer] {
  clockAt(100 MHz)

  genWith(new AxiSequencer(AxiConfig(addrBits=32, dataBytes=4)))

  simSuiteWith("OneLane")(new AxiSequencer(AxiConfig(addrBits=24, dataBytes=1)))
  simSuiteRun("OneLane") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    init(dut)
    env.waitFor()

    perform(dut, 0x103, 5, S2, Fixed) // Invalid Size
    perform(dut, 0xffc, 4, S1, Incr) // Boundary cross
    perform(dut, 0xffc, 3, S1, Incr)
    perform(dut, 0x103, 4, S1, Wrap) // Invalid Wrap Length
    perform(dut, 0x103, 7, S1, Wrap)
    perform(dut, 0x80, 3, S1, Fixed)

    env.waitFor(27)
  }

  simSuiteWith("FourLanes")(new AxiSequencer(AxiConfig(addrBits=32, dataBytes=4)))
  simSuiteRun("FourLanes") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    init(dut)
    env.waitFor()

    // TODO-lw extend test cases
    perform(dut, 0x102, 5, S8, Incr) // Invalid Size
    perform(dut, 0xff0, 4, S4, Incr) // Boundary cross
    perform(dut, 0xff1, 3, S4, Incr)
    perform(dut, 0x103, 15, S1, Wrap) // Invalid Wrap Length
    perform(dut, 0x103, 7, S2, Wrap)
    perform(dut, 0x103, 7, S4, Wrap)
    perform(dut, 0x82, 3, S2, Fixed)
    perform(dut, 0x81, 3, S2, Fixed)

    env.waitFor(27)
  }

  simSuiteWith("ManyLanes")(new AxiSequencer(AxiConfig(addrBits=64, dataBytes=64)))
  simSuiteRun("ManyLanes") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    init(dut)
    env.waitFor()

    // TODO-lw adapt test cases
    perform(dut, 0x102, 5, S8, Incr)
    perform(dut, 0xff0, 4, S4, Incr) // Boundary cross
    perform(dut, 0xff1, 3, S4, Incr)
    perform(dut, 0x103, 15, S1, Wrap) // Invalid Wrap Length
    perform(dut, 0x123, 7, S4, Wrap)
    perform(dut, 0x103, 7, S16, Wrap)
    perform(dut, 0x82, 3, S2, Fixed)

    env.waitFor(27)
  }


  def S1 = 0
  def S2 = 1
  def S4 = 2
  def S8 = 3
  def S16 = 4
  def S32 = 5
  def S64 = 6
  def S128 = 7

  def Fixed = 0
  def Incr = 1
  def Wrap = 2

  def init(dut : AxiSequencer) = {
    dut.io.iNext #= false
    dut.io.iAddr #= 0
    dut.io.iLen #= 0
    dut.io.iSize #= 0
    dut.io.iBurst #= 0
    dut.io.iStart #= false
  }

  def perform(dut : AxiSequencer, addr : BigInt, len : Int, size : Int, burst : Int)(implicit env : ClockEnv) = {
    val addrBits = widthOf(dut.io.oAddr)
    val maskBits = widthOf(dut.io.oMask)

    env.waitNextWhen(!dut.io.oActive.toBoolean)
    dut.io.iNext #= false
    dut.io.iAddr #= addr
    dut.io.iLen #= len
    dut.io.iSize #= size
    dut.io.iBurst #= burst
    dut.io.iStart #= true

    val addrStr = addr.toString(16).reverse.padTo(addrBits/4+1, '0').reverse
    val burstStr = burst match {
      case 0 => "FIXED"
      case 1 => "INCR "
      case 2 => "WRAP "
      case _ => "<INVALID>"
    }
    env.dbg.print(s"BURST 0x${addrStr} ${burstStr} ${1<<size}B #BEATS=${len+1}")
    env.waitNextWhen(dut.io.oActive.toBoolean)
    dut.io.iStart #= false

    for (idx <- 0 to len) {
      val delay = Random.between(0, 5)
      if (delay > 0) {
        dut.io.iNext #= false
        env.waitFor(delay)
      }
      dut.io.iNext #= true
      env.waitFor(1)

      val beatStr = if (dut.io.oError.toBoolean) {
        "ERROR"
      } else {
        val addr = dut.io.oAddr.toBigInt
        val addrStr = addr.toString(16).reverse.padTo(addrBits/4+1, '0').reverse
        val widx = dut.io.oWIdx.toBigInt
        val wadr = dut.io.oWAddr.toBigInt
        val wadrStr = f"0x${wadr}%x+${widx}%x"
        val mask = dut.io.oMask.toBigInt
        val maskStr = ((maskBits - 1) downto 0).map(i => if (mask.testBit(i)) { "B" } else { "-" }).mkString("")
        val begStr = if (dut.io.oBegin.toBoolean) { ">" } else { " " }
        val endStr = if (dut.io.oEnd.toBoolean) { "<" } else { " " }
        val lastStr = if (dut.io.oLast.toBoolean) { " LAST" } else { "" }
        s"0x${addrStr} ${maskStr} ${wadrStr} ${begStr}${endStr}${lastStr}"
      }
      env.dbg.print(f" BEAT#${idx}%03d ${beatStr}")
    }
    dut.io.iNext #= false
  }

}


