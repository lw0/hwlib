package hwlib.comp.streamer

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.comp.ChannelArbiter
import hwlib.base.{Fifo, MultiFifo, ChannelFanOut}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, Amba}



class WriterCore(val cfg : AxiConfig,
                 val streamLogCaps : Seq[Int],
                 val queueLogDepth : Int = 5) extends Component {
  require(cfg.hasWr && cfg.hasStrb)
  val stmCfg = AxiStreamConfig(dataBytes=cfg.dataBytes, hasStrb=true)
  val stmCount = streamLogCaps.length
  val stmTagBits = log2Up(stmCount)
  def TTag = UInt(stmTagBits bits)

  val io = new Bundle {
    val mAxi   =     master(TAxi(cfg))

    val iStart = Vec(    in(Bool), stmCount)
    val oOver  = Vec(   out(Bool), stmCount)
    val oError = Vec(   out(Bool), stmCount)
    val oIdle  = Vec(   out(Bool), stmCount)

    val sGen   = Vec( slave(AddrGenPort(cfg)), stmCount)

    val sStm   = Vec( slave(TAxiStream(stmCfg)), stmCount)
  }

  val sAddrTag   = TTag

  val sDataTag   = TTag
  val sData      = cfg.TData
  val sDataStrb  = cfg.TMask
  val sDataLast  = Bool
  val sDataValid = Bool
  val sDataReady = Bool

  val sRespTag   = TTag
  val sResp      = cfg.TResp
  val sRespAct   = Bool


  /////////////////////////////////////////////////////////////////////////////
  // AW Channel and Queues
  val iAddrArbiter = new ChannelArbiter(AddrGenPack(cfg), stmCount)
  sAddrTag := iAddrArbiter.io.oSelId

  val iBarrier    = new ChannelFanOut(3)
  iBarrier.io.iValid := iAddrArbiter.io.mSel.valid
  iAddrArbiter.io.mSel.ready := iBarrier.io.oReady

  val iQueueW = new Fifo(TTag, queueLogDepth, false)
  iQueueW.io.iPutData := sAddrTag
  iQueueW.io.iPutValid := iBarrier.io.oValids(0)
  iBarrier.io.iReadys(0) := iQueueW.io.oPutReady

  val iQueueB = new MultiFifo(Bits(0 bits), queueLogDepth, stmTagBits, cfg.idBits, false)
  iQueueB.io.iPutTag := iAddrArbiter.io.oSelId
  iQueueB.io.iPutValid := iBarrier.io.oValids(1)
  iBarrier.io.iReadys(1) := iQueueB.io.oPutReady

  io.mAxi.aw.payload.assign(
    "addr"  -> iAddrArbiter.io.mSel.payload.addr.asBits.resizeLeft(widthOf(cfg.TAddr)).asUInt,
    "len"   -> iAddrArbiter.io.mSel.payload.len,
    "size"  -> cfg.cSizeFull,
    "burst" -> cfg.cBurstIncr,
    "id"    -> iQueueB.io.oPutSel)
  io.mAxi.aw.valid := iBarrier.io.oValids(2)
  iBarrier.io.iReadys(2) := io.mAxi.aw.ready


  /////////////////////////////////////////////////////////////////////////////
  // W Channel
  io.mAxi.w.payload.assign(
    "data" -> sData,
    "strb" -> sDataStrb,
    "last" -> sDataLast)
  sDataTag := iQueueW.io.oGetData
  io.mAxi.w.valid := sDataValid && iQueueW.io.oGetValid
  sDataReady := io.mAxi.w.ready && iQueueW.io.oGetValid
  iQueueW.io.iGetReady := sDataLast && sDataValid && io.mAxi.w.ready


  /////////////////////////////////////////////////////////////////////////////
  // B Channel
  if (cfg.hasId) {
    iQueueB.io.iGetSel := io.mAxi.b.id
  }
  sRespTag := iQueueB.io.oGetTag
  sResp := io.mAxi.b.resp
  sRespAct := io.mAxi.b.valid && iQueueB.io.oGetValid
  io.mAxi.b.ready := sRespAct
  iQueueB.io.iGetReady := sRespAct


  /////////////////////////////////////////////////////////////////////////////
  // Stream Demux
  sData      := B(0)
  sDataStrb  := B(0)
  sDataLast  := False
  sDataValid := False
  val iChannelLogic = for (idx <- 0 until stmCount) yield new Area {
    val iControl = new WriterSlice(cfg, streamLogCaps(idx), queueLogDepth)

    iControl.io.iStart := io.iStart(idx)
    io.oOver(idx)      := iControl.io.oOver
    io.oError(idx)     := iControl.io.oError
    io.oIdle(idx)      := iControl.io.oIdle

    iControl.io.sGen << io.sGen(idx)
    iControl.io.mGen >> iAddrArbiter.io.sOpt(idx)

    iControl.io.iStmData := io.sStm(idx).data
    iControl.io.iStmStrb := io.sStm(idx).strb
    iControl.io.iStmLast := io.sStm(idx).last
    iControl.io.iStmValid := io.sStm(idx).valid
    io.sStm(idx).ready := iControl.io.oStmReady

    iControl.io.iWChReady := False
    when (sDataTag === idx) {
      sData      := iControl.io.oWChData
      sDataStrb  := iControl.io.oWChStrb
      sDataLast  := iControl.io.oWChLast
      sDataValid := iControl.io.oWChValid
      iControl.io.iWChReady := sDataReady
    }

    iControl.io.iResp := cfg.cRespOkay
    iControl.io.iRespAct := False
    when (sRespTag === idx) {
      iControl.io.iResp    := sResp
      iControl.io.iRespAct := sRespAct
    }
  }
}

