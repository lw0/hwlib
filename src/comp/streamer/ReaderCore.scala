package hwlib.comp.streamer

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.comp.{ChannelArbiter, ChannelFifo}
import hwlib.base.{MultiFifo, ChannelFanOut}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, Amba, TAxiTPayload}



class ReaderCore(val cfg : AxiConfig,
                 val streamLogCaps : Seq[Int],
                 val queueLogDepth : Int = 5) extends Component {
  require(cfg.hasRd)
  val stmCfg = AxiStreamConfig(dataBytes=cfg.dataBytes, hasStrb=true)
  val stmCount = streamLogCaps.length
  val stmTagBits = log2Up(stmCount)
  def TTag = UInt(stmTagBits bits)

  val io = new Bundle {
    val mAxi   =     master(TAxi(cfg))

    val iStart = Vec(    in(Bool), stmCount)
    val oError = Vec(   out(Bool), stmCount)
    val oIdle  = Vec(   out(Bool), stmCount)

    val sGen   = Vec( slave(AddrGenPort(cfg)), stmCount)

    val mStm   = Vec(master(TAxiStream(stmCfg)), stmCount)
  }

  val sAddrTag   = TTag

  val sDataTag   = TTag
  val sData      = cfg.TData
  val sDataLast  = Bool
  val sDataResp  = cfg.TResp
  val sDataValid = Bool
  val sDataReady = Bool

  /////////////////////////////////////////////////////////////////////////////
  // AR Channel
  val iAddrArbiter = new ChannelArbiter(AddrGenPack(cfg), stmCount)
  sAddrTag := iAddrArbiter.io.oSelId

  val iBarrier = new ChannelFanOut(2)
  iBarrier.io.iValid := iAddrArbiter.io.mSel.valid
  iAddrArbiter.io.mSel.ready := iBarrier.io.oReady

  val iQueueR = new MultiFifo(Bits(0 bits), queueLogDepth, stmTagBits, cfg.idBits, false)
  iQueueR.io.iPutTag := sAddrTag
  iQueueR.io.iPutValid := iBarrier.io.oValids(0)
  iBarrier.io.iReadys(0) := iQueueR.io.oPutReady

  io.mAxi.ar.payload.assign(
    "addr" -> iAddrArbiter.io.mSel.payload.addr.asBits.resizeLeft(widthOf(cfg.TAddr)).asUInt,
    "len" -> iAddrArbiter.io.mSel.payload.len,
    "size" -> cfg.cSizeFull,
    "burst" -> cfg.cBurstIncr,
    "id" -> iQueueR.io.oPutSel)
  io.mAxi.ar.valid := iBarrier.io.oValids(1)
  iBarrier.io.iReadys(1) := io.mAxi.ar.ready


  /////////////////////////////////////////////////////////////////////////////
  // R Channel
  if (cfg.hasId) {
    iQueueR.io.iGetSel := io.mAxi.r.id
  }
  sDataTag := iQueueR.io.oGetTag
  sData := io.mAxi.r.data
  sDataResp := io.mAxi.r.resp
  sDataLast := io.mAxi.r.last
  sDataValid := io.mAxi.r.valid && iQueueR.io.oGetValid
  io.mAxi.r.ready := sDataReady && iQueueR.io.oGetValid
  iQueueR.io.iGetReady := io.mAxi.r.last && io.mAxi.r.valid && sDataReady
  // io.mAxi.r.ready := sDataReady && sDataValid //TODO-lw is that equivalent?!
  // iQueueR.io.iGetReady := sDataReady && sDataValid && io.mAxi.r.last

  /////////////////////////////////////////////////////////////////////////////
  // Demux
  sDataReady := False
  val iChannelLogic = for (idx <- 0 until stmCount) yield new Area {
    val iControl = new ReaderSlice(cfg, streamLogCaps(idx), queueLogDepth)
    iControl.io.iStart := io.iStart(idx)
    io.oError(idx) := iControl.io.oError
    io.oIdle(idx)  := iControl.io.oIdle

    iControl.io.sGen << io.sGen(idx)
    iControl.io.mGen >> iAddrArbiter.io.sOpt(idx)

    io.mStm(idx).data := iControl.io.oStmData
    io.mStm(idx).strb := iControl.io.oStmStrb
    io.mStm(idx).last := iControl.io.oStmLast
    io.mStm(idx).valid := iControl.io.oStmValid
    iControl.io.iStmReady := io.mStm(idx).ready

    iControl.io.iRChData := sData
    iControl.io.iRChLast := sDataLast
    iControl.io.iRChResp := sDataResp
    iControl.io.iRChValid := False
    when (sDataTag === idx) {
      iControl.io.iRChValid := sDataValid
      sDataReady := iControl.io.oRChReady
    }
  }
}

