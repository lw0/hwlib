package hwlib.comp

import scala.util.Random
import scala.collection.mutable

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, ApbConfig, TApb}
import hwlib.base.{ShiftFifo, Stage}
import hwlib.HwMain
import hwlib.sim._
import hwlib.sim.amba._


case class AxiTracerConfig(
    val withAR : Boolean = true,
    val withAW : Boolean = true,
    val withR  : Boolean = true,
    val withB  : Boolean = true,
    val withW  : Boolean = false,
    val timeShifts : Seq[Int] = Seq(0, 2, 4, 6, 8, 10, 12, 14),
    val addrShifts : Seq[Int] = Seq(0, 2, 6, 9, 12, 16, 20, 23)) {

  val timeBits = 20
  def TTime = UInt(timeBits bits)
  def TTimeSel = UInt(log2Up(timeShifts.length) bits)
  def fTimeShift(sel : UInt, time : UInt) = {
    val matches = for (idx <- 0 until timeShifts.length) yield {
      (idx, time.dropLow(timeShifts(idx)).resize(timeBits).asUInt)
    }
    if (timeShifts.length == (1 << widthOf(TTimeSel))) {
      sel.muxList(matches)
    } else {
      sel.muxList(time.resize(timeBits), matches)
    }
  }
  def fTimeResidue(sel : UInt, time : UInt) = {
    val matches = for (idx <- 0 until timeShifts.length) yield {
      (idx, time.takeLow(timeShifts(idx)).resize(timeBits).asUInt)
    }
    if (timeShifts.length == (1 << widthOf(TTimeSel))) {
      sel.muxList(matches)
    } else {
      sel.muxList(U(0, timeBits bits), matches)
    }
  }
  def fTimeUnit(sel : UInt) = {
    val matches = for (idx <- 0 until timeShifts.length) yield {
      (idx, U(timeShifts(idx), 8 bit))
    }
    if (timeShifts.length == (1 << widthOf(TTimeSel))) {
      sel.muxList(matches)
    } else {
      sel.muxList(U(0, 8 bits), matches)
    }
  }

  val addrBits = 56
  def TAddr = Bits(addrBits bits)
  def TAddrSel = UInt(log2Up(addrShifts.length) bits)
  def fAddrShift(sel : UInt, addr : Bits) = {
    val matches = for (idx <- 0 until addrShifts.length) yield {
      (idx, addr.dropLow(addrShifts(idx)).resize(addrBits).asBits)
    }
    if (addrShifts.length == (1 << widthOf(TAddrSel))) {
      sel.muxList(matches)
    } else {
      sel.muxList(addr.resize(addrBits), matches)
    }
  }
  def fAddrUnit(sel : UInt) = {
    val matches = for (idx <- 0 until addrShifts.length) yield {
      (idx, U(addrShifts(idx), 8 bit))
    }
    if (addrShifts.length == (1 << widthOf(TAddrSel))) {
      sel.muxList(matches)
    } else {
      sel.muxList(U(0, 8 bits), matches)
    }
  }

  val idBits = 7
  def TId = Bits(idBits bits)

  val kindBits = 4
  def TKind = Bits(kindBits bits)
}


class AxiTracer(val cfgAxi : AxiConfig,
                val cfg : AxiTracerConfig) extends Component {


  def TVolume = UInt(log2Up(cfgAxi.maxLogVolume) bit)

  def TReqPack = new Bundle {
    val addr = cfgAxi.TAddr
    val vol = TVolume
    val id = cfgAxi.hasId generate cfgAxi.TId
  }

  def TRspPack = new Bundle {
    val error = Bool
    val id = cfgAxi.hasId generate cfgAxi.TId
  }

  def TDatPack = new Bundle {
    val id = cfgAxi.hasWId generate cfgAxi.TId
  }

  var channelCount = 0
  val channelWidths = mutable.ArrayBuffer[Int]()
  val idxAR : Option[Int] = if (cfg.withAR) {
    val idx = channelCount
    channelCount += 1
    channelWidths += TReqPack.getBitsWidth
    Some(idx)
  } else { None }
  val idxAW : Option[Int] = if (cfg.withAW) {
    val idx = channelCount
    channelCount += 1
    channelWidths += TReqPack.getBitsWidth
    Some(idx)
  } else { None }
  val idxR : Option[Int] = if (cfg.withR) {
    val idx = channelCount
    channelCount += 1
    channelWidths += TRspPack.getBitsWidth
    Some(idx)
  } else { None }
  val idxB : Option[Int] = if (cfg.withB) {
    val idx = channelCount
    channelCount += 1
    channelWidths += TRspPack.getBitsWidth
    Some(idx)
  } else { None }
  val idxW : Option[Int] = if (cfg.withW) {
    val idx = channelCount
    channelCount += 1
    channelWidths += TDatPack.getBitsWidth
    Some(idx)
  } else { None }


  val io = new Bundle {
    val sAxi   =  slave(TAxi(cfgAxi))
    val mAxi   = master(TAxi(cfgAxi))

    val mTrace = master(TAxiStream(AxiTracer.CfgStm))

    val sReg =  slave(TApb(AxiTracer.CfgApb))
  }


  io.mAxi << io.sAxi


  /// Axi Monitors and Funnel ///

  val sCtlStart = Bool
  val sCtlStop  = Bool
  val sCtlActive = Bool
  val sCtlSkewed = Bool
  val rCtlStarting = Reg(Bool) init False
  val rCtlStopping = Reg(Bool) init False

  val sCfgTimeSel = cfg.TTimeSel
  val sCfgAddrSel = cfg.TAddrSel
  val sCfgIdSel = Bool
  val sCfgARSel = Bool
  val sCfgAWSel = Bool
  val sCfgRSel  = Bool
  val sCfgWSel  = Bool
  val sCfgBSel  = Bool


  when (sCtlActive) {
    rCtlStarting := False
    when (sCtlStop) {
      rCtlStopping := True
    }
  } otherwise {
    rCtlStopping := False
    when (sCtlStart) {
      rCtlStarting := True
    }
  }

  val iCollector = new TraceCollector(channelWidths.toSeq, timeBits=12, logDepth=9)
  iCollector.io.iStart := rCtlStarting
  iCollector.io.iStop  := rCtlStopping
  sCtlActive := iCollector.io.oActive
  sCtlSkewed := iCollector.io.oSkewed

  val iARMonitor = idxAR.isDefined generate new Area {
    val sPack = TReqPack
    sPack.addr := io.sAxi.ar.addr
    sPack.vol := OHToUInt(OHMasking.last(io.sAxi.ar.len ## True)) + io.sAxi.ar.size.resized
    if (cfgAxi.hasId) {
      sPack.id := io.sAxi.ar.id
    }
    iCollector.io.iEnable(idxAR.get) := sCfgARSel
    iCollector.io.iChannel(idxAR.get) := sPack.asBits
    iCollector.io.iTrigger(idxAR.get) := io.sAxi.ar.valid && io.mAxi.ar.ready
  }

  val iAWMonitor = idxAW.isDefined generate new Area {
    val sPack = TReqPack
    sPack.addr := io.sAxi.aw.addr
    sPack.vol := OHToUInt(OHMasking.last(io.sAxi.aw.len ## True)) + io.sAxi.aw.size.resized
    if (cfgAxi.hasId) {
      sPack.id := io.sAxi.aw.id
    }
    iCollector.io.iEnable(idxAW.get) := sCfgAWSel
    iCollector.io.iChannel(idxAW.get) := sPack.asBits
    iCollector.io.iTrigger(idxAW.get) := io.sAxi.aw.valid && io.mAxi.aw.ready
  }

  val iRMonitor = idxR.isDefined generate new Area {
    val sPack = TRspPack
    sPack.error := io.mAxi.r.resp(1)
    if (cfgAxi.hasId) {
      sPack.id := io.mAxi.r.id
    }
    iCollector.io.iEnable(idxR.get) := sCfgRSel
    iCollector.io.iChannel(idxR.get) := sPack.asBits
    iCollector.io.iTrigger(idxR.get) := io.mAxi.r.last && io.mAxi.r.valid && io.sAxi.r.ready
  }

  val iBMonitor = idxB.isDefined generate new Area {
    val sPack = TRspPack
    sPack.error := io.mAxi.r.resp(1)
    if (cfgAxi.hasId) {
      sPack.id := io.mAxi.b.id
    }
    iCollector.io.iEnable(idxB.get) := sCfgBSel
    iCollector.io.iChannel(idxB.get) := sPack.asBits
    iCollector.io.iTrigger(idxB.get) := io.mAxi.b.valid && io.sAxi.b.ready
  }

  val iWMonitor = idxW.isDefined generate new Area {
    val sPack = TDatPack
    if (cfgAxi.hasWId) {
      sPack.id := io.sAxi.w.id
    }
    iCollector.io.iEnable(idxW.get) := sCfgWSel
    iCollector.io.iChannel(idxW.get) := sPack.asBits
    iCollector.io.iTrigger(idxW.get) := io.sAxi.w.last && io.sAxi.w.valid && io.mAxi.w.ready
  }

  val sEncReady = Bool
  val s1EncReady = Bool
  val s2EncReady = Bool


  /// First Pre-Encoding Stage ///

  iCollector.io.iReady := s1EncReady

  val sEncReqPack = TReqPack
  val sEncRspPack = TRspPack
  val sEncDatPack = TDatPack
  sEncReqPack.assignFromBits(iCollector.io.oChannel)
  sEncRspPack.assignFromBits(iCollector.io.oChannel)
  sEncDatPack.assignFromBits(iCollector.io.oChannel)

  val sEncWrite   = False
  val sEncKind    = B(0, 4 bits)
  val sEncId      = B(0, 7 bits)
  val sEncUseAddr = False
  val sEncUseId   = False

  val sEncVolume = cfg.TKind
  sEncVolume := sEncReqPack.vol.asBits.resized
  if (widthOf(TVolume) >= widthOf(cfg.TKind)) {
    when (sEncReqPack.vol > 12) {
      sEncVolume := 12
    }
  }

  if (idxAR.isDefined) {
    when (iCollector.io.oIndex === idxAR.get) {
      sEncWrite := False
      sEncUseAddr := True
      sEncKind := sEncVolume
      if (cfgAxi.hasId) {
        sEncId := sEncReqPack.id.asBits.resized
        sEncUseId := sCfgIdSel
      }
    }
  }
  if (idxAW.isDefined) {
    when(iCollector.io.oIndex === idxAW.get) {
      sEncWrite := True
      sEncUseAddr := True
      sEncKind := sEncVolume
      if (cfgAxi.hasId) {
        sEncId := sEncReqPack.id.asBits.resized
        sEncUseId := sCfgIdSel
      }
    }
  }
  if (idxR.isDefined) {
    when (iCollector.io.oIndex === idxR.get) {
      sEncWrite := False
      when (sEncRspPack.error) {
        sEncKind := 14
      } otherwise {
        sEncKind := 15
      }
      if (cfgAxi.hasId) {
        sEncId := sEncRspPack.id.asBits.resized
        sEncUseId := sCfgIdSel
      }
    }
  }
  if (idxB.isDefined) {
    when (iCollector.io.oIndex === idxB.get) {
      sEncWrite := True
      when (sEncRspPack.error) {
        sEncKind := 14
      } otherwise {
        sEncKind := 15
      }
      if (cfgAxi.hasId) {
        sEncId := sEncRspPack.id.asBits.resized
        sEncUseId := sCfgIdSel
      }
    }
  }
  if (idxW.isDefined) {
    when (iCollector.io.oIndex === idxW.get) {
      sEncWrite := True
      sEncKind := 13
      if (cfgAxi.hasWId) {
        sEncId := sEncDatPack.id.asBits.resized
        sEncUseId := sCfgIdSel
      }
    }
  }

  val r1EncTimeResidue = Reg(cfg.TTime) init 0

  val sEncTime = iCollector.io.oRelTime + r1EncTimeResidue
  val sEncTimeShift = cfg.fTimeShift(sCfgTimeSel, sEncTime)
  val sEncTimeResidue = cfg.fTimeResidue(sCfgTimeSel, sEncTime)
  when (s1EncReady && iCollector.io.oValid) {
    when (iCollector.io.oLast) {
      r1EncTimeResidue := 0
    } otherwise {
      r1EncTimeResidue := sEncTimeResidue
    }
  }

  val sEncAddrShift = cfg.fAddrShift(sCfgAddrSel, sEncReqPack.addr.asBits)

  val r1EncTime        = RegNextWhen(sEncTimeShift, s1EncReady)
  val r1EncAddr        = RegNextWhen(sEncAddrShift, s1EncReady)
  val r1EncId          = RegNextWhen(sEncId, s1EncReady)
  val r1EncOver        = RegNextWhen(iCollector.io.oOver, s1EncReady)
  val r1EncWrite       = RegNextWhen(sEncWrite, s1EncReady)
  val r1EncKind        = RegNextWhen(sEncKind, s1EncReady)
  val r1EncUseEvent    = RegNextWhen(iCollector.io.oHasData, s1EncReady)
  val r1EncUseAddr     = RegNextWhen(iCollector.io.oHasData && sEncUseAddr, s1EncReady)
  val r1EncUseId       = RegNextWhen(iCollector.io.oHasData && sEncUseId, s1EncReady)
  val r1EncLast        = RegNextWhen(iCollector.io.oLast, s1EncReady)
  val r1EncValid       = RegNextWhen(iCollector.io.oValid, s1EncReady) init False

  s1EncReady := !r1EncValid || s2EncReady


  /// Second Pre-Encoding Stage ///

  val sEncTimeNull = (r1EncTime(13, 7 bits) === 0) ##
                     (r1EncTime( 6, 7 bits) === 0) ##
                     (r1EncTime( 0, 6 bits) === 0)
  val sEncTimeSignificant = PropagateOnes.toLsb(~sEncTimeNull)

  val sEncAddrNull = (sEncAddrShift(49, 7 bits) === 0) ##
                     (sEncAddrShift(42, 7 bits) === 0) ##
                     (sEncAddrShift(35, 7 bits) === 0) ##
                     (sEncAddrShift(28, 7 bits) === 0) ##
                     (sEncAddrShift(21, 7 bits) === 0) ##
                     (sEncAddrShift(14, 7 bits) === 0) ##
                     (sEncAddrShift( 7, 7 bits) === 0) ##
                     (sEncAddrShift( 0, 7 bits) === 0)
  val sEncAddrSignificant = PropagateOnes.toLsb(~sEncAddrNull)


  val rEncTime            = RegNextWhen(r1EncTime, s2EncReady)
  val rEncTimeSignificant = RegNextWhen(sEncTimeSignificant, s2EncReady)
  val rEncAddr            = RegNextWhen(r1EncAddr, s2EncReady)
  val rEncAddrSignificant = RegNextWhen(sEncAddrSignificant, s2EncReady)
  val rEncId              = RegNextWhen(r1EncId, s2EncReady)
  val rEncOver            = RegNextWhen(r1EncOver, s2EncReady)
  val rEncWrite           = RegNextWhen(r1EncWrite, s2EncReady)
  val rEncKind            = RegNextWhen(r1EncKind, s2EncReady)
  val rEncUseEvent        = RegNextWhen(r1EncUseEvent, s2EncReady)
  val rEncUseAddr         = RegNextWhen(r1EncUseAddr, s2EncReady)
  val rEncUseId           = RegNextWhen(r1EncUseId, s2EncReady)
  val rEncLast            = RegNextWhen(r1EncLast, s2EncReady)
  val rEncValid           = RegNextWhen(r1EncValid, s2EncReady) init False

  s2EncReady := !rEncValid || sEncReady



  /// Byte Encoding Stage ///

  /* Trace Format:
     (higher / later in stream bytes to the left
      lower / earlier in stream bytes to the right)

     Timestamp Packet
     ttttttt0 ttttttt0 tttttt01
       ------01 introducer byte mandatory
       -------0 follow bytes optional
       t - {6, 13, 20} bit relative timestamp

     Event Packet
     aaaaaaa0 aaaaaaa0 aaaaaaa0 aaaaaaa0
     aaaaaaa0 aaaaaaa0 aaaaaaa0 aaaaaaa0
     iiiiiii0 kkkkwv11
       ------11 introducer byte mandatory
       -------0 follow bytes optional
       a - {7, 14, 21, 28, 35, 42, 49, 56} bit address
       i - 7 bit id
       k - 4 bit kind
           0..12 = Request Volume {1B..4kB}
           13    = Write Data Complete
           14    = Response Error
           15    = Response Okay
       w - operation (1 write 0 read)
       v - overflow
   */

  val sOutMap   = UInt(2 bits)
  val sOutMask  = Bits(4 bits)
  val sOutLast  = Bool
  val sOutValid = Bool
  val sOutReady = Bool

  /*
   Stream Maps:
   0 - E? T? T? T  - event, time up to 20 bit
   1 - I? E  T? T? - id, event, time up to 13 bit
   2 - A? A? A? A  - low addr up to 28 bit
   3 - A? A? A? A  - high addr up to 56 bit
   */

  sOutValid := rEncValid
  val iOutFsm = new StateMachine {
    sOutMap := 0
    sOutMask := 0
    sOutLast := False
    sEncReady := False

    val Normal : State = new State with EntryPoint {
      whenIsActive {
        when (!rEncUseEvent) {
          sOutMap   := 0
          sOutMask  := False ## rEncTimeSignificant
          sEncReady := sOutReady
        } elsewhen (!rEncUseId) {
          sOutMap   := 0
          sOutMask  := True ## rEncTimeSignificant
          sEncReady := sOutReady && !rEncUseAddr
          sOutLast  := rEncLast && !rEncUseAddr
          when (rEncValid && sOutReady && rEncUseAddr) {
            goto(HalfAddr)
          }
        } elsewhen (!rEncTimeSignificant(2)) {
          sOutMap   := 1
          sOutMask  := True ## True ## rEncTimeSignificant(1 downto 0)
          sEncReady := sOutReady && !rEncUseAddr
          sOutLast  := rEncLast && !rEncUseAddr
          when (rEncValid && sOutReady && rEncUseAddr) {
            goto(HalfAddr)
          }
        } otherwise {
          sOutMap   := 0
          sOutMask  := False ## rEncTimeSignificant
          when (rEncValid && sOutReady) {
            goto(FullEvent)
          }
        }
      }
    }

    val FullEvent : State = new State {
      whenIsActive {
        sOutMap   := 1
        sOutMask  := True ## True ## False ## False
        sEncReady := sOutReady && !rEncUseAddr
        sOutLast  := rEncLast && !rEncUseAddr
        when (rEncValid && sOutReady) {
          when (rEncUseAddr) {
            goto(HalfAddr)
          } otherwise {
            goto(Normal)
          }
        }
      }
    }

    val HalfAddr: State = new State {
      whenIsActive {
        sOutMap   := 2
        sOutMask  := rEncAddrSignificant(3 downto 0)
        sEncReady := sOutReady && !rEncAddrSignificant(4)
        sOutLast  := rEncLast && !rEncAddrSignificant(4)
        when (rEncValid && sOutReady) {
          when (rEncAddrSignificant(4)) {
            goto(FullAddr)
          } otherwise {
            goto(Normal)
          }
        }
      }
    }

    val FullAddr : State = new State {
      whenIsActive {
        sOutMap   := 3
        sOutMask  := rEncAddrSignificant(7 downto 4)
        sEncReady := sOutReady
        sOutLast  := rEncLast
        when (rEncValid && sOutReady) {
          goto(Normal)
        }
      }
    }
  }

  val sMap0 = Bits(32 bits)
  // E / T3 / T2 / T1 /
  sMap0( 1 downto  0) := B"01"
  sMap0( 7 downto  2) := rEncTime(5 downto 0).asBits
  sMap0( 8 downto  8) := B"0"
  sMap0(15 downto  9) := rEncTime(12 downto 6).asBits
  sMap0(16 downto 16) := B"0"
  sMap0(23 downto 17) := rEncTime(19 downto 13).asBits
  sMap0(25 downto 24) := B"11"
  sMap0(26)           := rEncOver
  sMap0(27)           := rEncWrite
  sMap0(31 downto 28) := rEncKind

  val sMap1 = Bits(32 bits)
  // I / E / T2 / T1 /
  sMap1( 1 downto  0) := B"01"
  sMap1( 7 downto  2) := rEncTime(5 downto 0).asBits
  sMap1( 8 downto  8) := B"0"
  sMap1(15 downto  9) := rEncTime(12 downto 6).asBits
  sMap1(17 downto 16) := B"11"
  sMap1(18)           := rEncOver
  sMap1(19)           := rEncWrite
  sMap1(23 downto 20) := rEncKind
  sMap1(24 downto 24) := B"0"
  sMap1(31 downto 25) := rEncId

  val sMap2 = Bits(32 bits)
  // A4 / A3 / A2 / A1 /
  sMap2( 0 downto  0) := B"0"
  sMap2( 7 downto  1) := rEncAddr(6 downto 0)
  sMap2( 8 downto  8) := B"0"
  sMap2(15 downto  9) := rEncAddr(13 downto 7)
  sMap2(16 downto 16) := B"0"
  sMap2(23 downto 17) := rEncAddr(20 downto 14)
  sMap2(24 downto 24) := B"0"
  sMap2(31 downto 25) := rEncAddr(27 downto 21)

  val sMap3 = Bits(32 bits)
  // A8 / A7 / A6 / A5 /
  sMap3( 0 downto  0) := B"0"
  sMap3( 7 downto  1) := rEncAddr(34 downto 28)
  sMap3( 8 downto  8) := B"0"
  sMap3(15 downto  9) := rEncAddr(41 downto 35)
  sMap3(16 downto 16) := B"0"
  sMap3(23 downto 17) := rEncAddr(48 downto 42)
  sMap3(24 downto 24) := B"0"
  sMap3(31 downto 25) := rEncAddr(55 downto 49)

  io.mTrace.data := sOutMap.mux(0 -> sMap0, 1 -> sMap1, 2 -> sMap2, 3 -> sMap3)
  io.mTrace.keep := sOutMask
  io.mTrace.last := sOutLast
  io.mTrace.valid := sOutValid
  sOutReady := io.mTrace.ready


  /// Control Register Interface ///

  val iRegs = new ApbRegFile(AxiTracer.CfgApb)

  iRegs.io.sApb << io.sReg

  /* Reg 0 Control
               0 - RW Stop  (Write 1, Clear automatically)
               1 - RW Start (Write 1, Clear automatically)
              23 - RO Skewed, current or past trace had timer held for at least 1 cycle
              31 - RO Active
   */
  sCtlStop  := iRegs.io.oReg(0)(0) && iRegs.io.oRegWr(0)
  sCtlStart := iRegs.io.oReg(0)(1) && iRegs.io.oRegWr(0)
  iRegs.io.iReg(0) := (31 -> sCtlActive,
                       23 -> sCtlSkewed,
                        1 -> rCtlStarting,
                        0 -> rCtlStopping,
                       default -> False)


  /* Reg 1 Config
               0 - RW Enable AR Channel
               1 - RW Enable AW Channel
               2 - RW Enable  R Channel
               3 - RW Enable  B Channel
               4 - RW Enable  W Channel
               5 - RW Enable ID Reporting
               6 - RO Is WID Reporting enabled
        15 ..  8 - RO Log of Actual Time Unit
        19 .. 16 - RW Select Time Unit
        23 .. 20 - RW Select Address Unit
        31 .. 24 - RO Log of Actual Address Unit
   */
  sCfgARSel := iRegs.io.oReg(1)(0)
  sCfgAWSel := iRegs.io.oReg(1)(1)
  sCfgRSel  := iRegs.io.oReg(1)(2)
  sCfgBSel  := iRegs.io.oReg(1)(3)
  sCfgWSel  := iRegs.io.oReg(1)(4)
  sCfgIdSel := iRegs.io.oReg(1)(5)
  sCfgTimeSel := iRegs.io.oReg(1)(19 downto 16).asUInt.resized
  sCfgAddrSel := iRegs.io.oReg(1)(23 downto 20).asUInt.resized
  iRegs.io.iReg(1) := ( (0) -> (if (idxAR.isDefined) { sCfgARSel } else { False }),
                        (1) -> (if (idxAW.isDefined) { sCfgAWSel } else { False }),
                        (2) -> (if (idxR.isDefined)  { sCfgRSel } else { False }),
                        (3) -> (if (idxB.isDefined)  { sCfgBSel } else { False }),
                        (4) -> (if (idxW.isDefined)  { sCfgWSel } else { False }),
                        (5) -> (if (cfgAxi.hasId)    { sCfgIdSel } else { False }),
                        (6) -> (if (cfgAxi.hasWId)   { sCfgIdSel } else { False }),
                        (15 downto  8) -> cfg.fTimeUnit(sCfgTimeSel).asBits.resized,
                        (19 downto 16) -> sCfgTimeSel.asBits.resized,
                        (23 downto 20) -> sCfgAddrSel.asBits.resized,
                        (31 downto 24) -> cfg.fAddrUnit(sCfgTimeSel).asBits.resized,
                        default -> False)

}


object AxiTracer extends HwMain[AxiTracer] {

  val CfgApb = ApbConfig(addrBits = 1, dataBytes = 4)
  val CfgStm = AxiStreamConfig(dataBytes = 4, hasKeep = true)

  val dataSize = 2 // 4 bytes

  simWith(new AxiTracer(AxiConfig(addrBits=32, dataBytes=(1 << dataSize), idBits=4), AxiTracerConfig()))
  clockAt(222.222 MHz)

  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    val doneFlag = Future[Unit]()
    //env.timeout(64*1024)

    env.onRawCycle(2) { () =>
      env.setRst(false)
    }

    val iRegRW = new ApbMaster(dut.io.sReg, DelayQueueConfig())

    val masterCfg = AxiModelConfig()
    val iWrite = new AxiWrMaster(dut.io.sAxi, masterCfg)
    val iRead = new AxiRdMaster(dut.io.sAxi, masterCfg)

    val slaveCfg = AxiModelConfig(dataRate=RateGen(0.8))
    val iWrSlave = new AxiWrSlave(dut.io.mAxi, slaveCfg)
    val iRdSlave = new AxiRdSlave(dut.io.mAxi, slaveCfg)
    val iSlaveMem = new MemBuffer(12)
    iWrSlave.onWrite(0x000, 0x10000) {
      (addr, bytes, data, mask) =>
        iSlaveMem.writeMask(addr, bytes, data, mask)
        Resp.Okay
    }
    iRdSlave.onRead(0x000, 0x10000) {
      (addr, bytes) =>
        val data = iSlaveMem.read(addr, bytes)
        (Resp.Okay, data)
    }

    val iStm = new AxiStreamSlave(dut.io.mTrace, AxiModelConfig(dataRate=RateGen(0.2)))
    iStm.onRecv {
     (pkg) =>
       println(s"Received ${pkg}")
    }

    env.waitFor()

    println("=== Begin ===")

    val rdReqs = mutable.Queue[Future[AxiRdResult]]()
    val wrReqs = mutable.Queue[Future[AxiWrResult]]()
    val reqDone = Future[Unit]()
    fork {
      iRegRW.bwrite(0x1, 0xf) // enable tracing on all channels
      val config = iRegRW.bread(0x1).data
      env.dbg.print(s"CONFIG 0x${config.toString(16)}")
      iRegRW.bwrite(0x0, 0x2) // initiate tracer startup
      var repeat = true
      while (repeat) {
        val status = iRegRW.bread(0x0).data.toLong
        println(s"[0x0] = 0x${status.toHexString}")
        repeat = (status & 0x80000000) == 0
      }
      for (i <- 0 until 1024) {
        val id = Random.between(0, 16)
        val addr = Random.between(0L, 0x10000L)
        val len = Random.between(1, 0x101)
        val size = Size.from(Random.between(0, dataSize+1))
        val delay = (if (Random.nextDouble() < 0.01) {16384} else {Random.between(0, 4)})
        val write = Random.nextBoolean()
        env.dbg.print(s"REQ${i} ${if(write){"WR"}else{"RD"}} 0x${addr.toHexString} ${len}x${size} ON 0x${id.toHexString}")
        if (write) {
          val dataBits = (1 << size.id) * (len + 1) * 8
          wrReqs.enqueue(iWrite.writeOn(id, addr, BigInt(dataBits, Random.self), len, size, Burst.Incr))
        } else {
          rdReqs.enqueue(iRead.readOn(id, addr, len, size, Burst.Incr))
        }
        env.waitFor(delay)
        if (delay > 16) {
          iRegRW.bwrite(0x1, 0x0044002f)
          val config = iRegRW.bread(0x1).data
          env.dbg.print(s"CONFIG 0x${config.toString(16)}")
        }
      }
      iRegRW.bwrite(0x0, 0x1) // initiate tracer shutdown
      reqDone.resolve()
    }

    var repeat = true
    while (repeat) {
      env.waitNextWhen(reqDone.isResolved || rdReqs.nonEmpty || wrReqs.nonEmpty)
      while (rdReqs.nonEmpty) {
        val rsp = rdReqs.dequeue().blockValue()
        env.dbg.print(s"RSP RD ${rsp}")
      }
      while (wrReqs.nonEmpty) {
        val rsp = wrReqs.dequeue().blockValue()
        env.dbg.print(s"RSP WR ${rsp}")
      }
      if (reqDone.isResolved) {
        repeat = false
      }
    }

    println("=== End ===")
    env.waitFor(108)
  }
}

