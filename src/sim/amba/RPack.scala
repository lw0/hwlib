package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxiRPayload}

import Resp.Resp



class RPack(
  val data : BigInt,
  val resp : Resp,
  val last : Boolean,
  val id : Long,
  val cfg : AxiConfig) {
  override def toString : String = {
    s"RPack(${resp} 0x${data.toString(16)} @${id} ${if(last) " last" else ""})"
  }
}


object RPack {
  def apply(data : BigInt, resp : Resp, last : Boolean, id : Long)(implicit cfg : AxiConfig) : RPack = {
    val dataLen = data.bitLength
    val dataSize = (1 << (cfg.fullSize + 3))
    val actData = if (dataLen > dataSize) {
      SpinalWarning(s"Data width ${dataLen} exceeds interface width ${dataSize}")
      data & ((BigInt(1) << dataSize) - 1)
    } else {
      data
    }

    val actLast = if (!cfg.hasBurst && !last) {
      SpinalWarning(s"Non-last transfers not supported, thus forced to last")
      true
    } else {
      last
    }

    val actId = if (id > cfg.maxId) {
      SpinalWarning(s"Id ${id} above interface maximum ${cfg.maxId}, thus truncated to ${id & cfg.maxId}")
      id & cfg.maxId
    } else {
      id
    }

    new RPack(actData, resp, actLast, actId, cfg)
  }

  def drive(payload : TAxiRPayload, value : RPack) : Unit = {
    payload.data #= value.data
    payload.resp #= value.resp.id
    if (payload.cfg.hasId) {
      payload.id #= value.id
    }
    if (payload.cfg.hasBurst) {
      payload.last #= value.last
    }
  }

  def read(payload : TAxiRPayload) : RPack = {
    val data = payload.data.toBigInt

    val rawResp = payload.resp.toInt
    val resp = if (rawResp >= Resp.maxId) {
      SpinalWarning(s"Reading Invalid resp signal 0x${rawResp.toHexString}, force SlvErr")
      Resp.SlvErr
    } else {
      Resp(rawResp)
    }

    val id = if (payload.cfg.hasId) {
      payload.id.toLong
    } else {
      0L
    }

    val last = if (payload.cfg.hasBurst) {
      payload.last.toBoolean
    } else {
      true
    }

    apply(data, resp, last, id)(payload.cfg)
  }
}

