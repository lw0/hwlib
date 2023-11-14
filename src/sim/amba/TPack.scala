package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiStreamConfig, TAxiTPayload}



class TPack(
  val data : BigInt,
  val keep : BigInt,
  val strb : BigInt,
  val id   : Long,
  val dest : Long,
  val last : Boolean,
  val cfg : AxiStreamConfig) {
  def identity = (dest << 32) | id

  override def toString : String = {
    s"TPack(0x${data.toString(16)} | 0b${(keep & strb).toString(2)} @${id}>${dest}${if(last) " last" else ""})"
  }
}


object TPack {

  def apply(data : BigInt, keep : BigInt, strb : BigInt, id : Long, dest : Long, last : Boolean)(implicit cfg : AxiStreamConfig) : TPack = {

    val dataLen = data.bitLength
    val dataSize = cfg.dataBytes * 8
    val actData = if (dataLen > dataSize) {
      SpinalWarning(s"Data width ${dataLen} exceeds interface width ${dataSize}")
      data & ((BigInt(1) << dataSize) - 1)
    } else {
      data
    }

    val keepLen = keep.bitLength
    val maskSize = cfg.dataBytes
    val maskFull = (BigInt(1) << maskSize) - 1
    val actKeep = if (!cfg.hasKeep && (~keep & maskFull) != 0) {
      SpinalWarning(s"Keep ${keep.toString(2)} not supported, thus set to neutral. Null bytes become position or data bytes!")
      maskFull
    } else if (keepLen > maskSize) {
      SpinalWarning(s"Keep width ${keepLen} exceeds interface width ${maskSize}")
      keep & maskFull
    } else {
      keep
    }

    val strbLen = strb.bitLength
    if ((strb & ~actKeep & maskFull) != 0) {
      SpinalWarning(s"Strb ${strb.toString(2)} overlaps not-keep ${actKeep.toString(2)} which is reserved in AXI4. Reverting to null-bytes.")
    }
    val actStrb = if (!cfg.hasStrb && (~strb & actKeep & maskFull) != 0) {
      SpinalWarning(s"Strb ${strb.toString(2)} not supported, thus set to neutral. Position bytes become data bytes!")
      actKeep
    } else if (strbLen > maskSize) {
      SpinalWarning(s"Strb width ${strbLen} exceeds interface width $maskSize}")
      strb & maskFull & actKeep
    } else {
      strb & actKeep
    }

    val actId = if (id > cfg.maxId) {
      SpinalWarning(s"Id ${id} above interface maximum ${cfg.maxId}, thus truncated to ${id & cfg.maxId}")
      id & cfg.maxId
    } else {
      id
    }

    val actDest = if (dest > cfg.maxDest) {
      SpinalWarning(s"Dest ${dest} above interface maximum ${cfg.maxDest}, thus truncated to ${dest & cfg.maxDest}")
      dest & cfg.maxDest
    } else {
      dest
    }

    new TPack(actData, actKeep, actStrb, actId, actDest, last, cfg)
  }

  def drive(payload : TAxiTPayload, value : TPack) : Unit = {
    payload.data #= value.data
    if (payload.cfg.hasKeep) {
      payload.keep #= value.keep
    }
    if (payload.cfg.hasStrb) {
      payload.strb #= value.strb
    }
    if (payload.cfg.hasId) {
      payload.id #= value.id
    }
    if (payload.cfg.hasDest) {
      payload.dest #= value.dest
    }
    payload.last #= value.last
  }

  def read(payload : TAxiTPayload) : TPack = {
    val data = payload.data.toBigInt
    val keep = if (payload.cfg.hasKeep) {
      payload.keep.toBigInt
    } else {
      (BigInt(1) << payload.cfg.dataBytes) - 1
    }
    val strb = if (payload.cfg.hasStrb) {
      payload.strb.toBigInt
    } else {
      keep
    }
    val id = if (payload.cfg.hasId) {
      payload.id.toLong
    } else {
      0L
    }
    val dest = if (payload.cfg.hasDest) {
      payload.dest.toLong
    } else {
      0L
    }
    val last = payload.last.toBoolean

    apply(data, keep, strb, id, dest, last)(payload.cfg)
  }
}

