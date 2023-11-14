package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxiBPayload}

import Resp.Resp



class BPack(
  val resp : Resp,
  val id : Long,
  val cfg : AxiConfig) {
  override def toString : String = {
    s"BPack(${resp} @${id})"
  }
}

object BPack {
  def apply(resp : Resp, id : Long)(implicit cfg : AxiConfig) : BPack = {
    val actId = if (id > cfg.maxId) {
      SpinalWarning(s"Id ${id} above interface maximum ${cfg.maxId}, thus truncated to ${id & cfg.maxId}")
      id & cfg.maxId
    } else {
      id
    }
    new BPack(resp, actId, cfg)
  }

  def drive(payload : TAxiBPayload, value : BPack) : Unit = {
    payload.resp #= value.resp.id
    if (payload.cfg.hasId) {
      payload.id #= value.id
    }
  }

  def read(payload : TAxiBPayload) : BPack = {
    val rawResp = payload.resp.toInt
    val resp = if (rawResp >= Resp.maxId) {
      SpinalWarning(s"Invalid resp signal 0x${rawResp.toHexString}, force RSlvErr")
      Resp.SlvErr
    } else {
      Resp(rawResp)
    }

    val id = if (payload.cfg.hasId) {
      payload.id.toLong
    } else {
      0L
    }

    apply(resp, id)(payload.cfg)
  }
}

