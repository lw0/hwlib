package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxiWPayload}



class WPack(
  val data : BigInt,
  val strb : BigInt,
  val last : Boolean,
  val cfg : AxiConfig) {
  override def toString : String = {
    s"WPack(0x${data.toString(16)} | 0b${strb.toString(2)}${if(last) " last" else ""})"
  }
}

object WPack {

  def apply(data : BigInt, strb : BigInt, last : Boolean)(implicit cfg : AxiConfig) : WPack = {
    val dataLen = data.bitLength
    val dataSize = cfg.dataBytes * 8
    val actData = if (dataLen > dataSize) {
      SpinalWarning(s"Data width ${dataLen} exceeds interface width ${dataSize}")
      data & ((BigInt(1) << dataSize) - 1)
    } else {
      data
    }

    val strbLen = strb.bitLength
    val strbSize = cfg.dataBytes
    val strbFull = (BigInt(1) << strbSize) - 1
    val actStrb = if (!cfg.hasStrb && (~strb & strbFull) != 0) {
      SpinalWarning(s"Strb ${strb.toString(2)} not supported, thus set to neutral. Partial writes become full writes!")
      strbFull
    } else if (strbLen > strbSize) {
      SpinalWarning(s"Strb width ${strbLen} exceeds interface width ${strbSize}")
      strb & strbFull
    } else {
      strb
    }

    val actLast = if (!cfg.hasBurst && !last) {
      SpinalWarning(s"Non-last transfers not supported, thus forced to last")
      true
    } else {
      last
    }

    new WPack(actData, actStrb, actLast, cfg)
  }

  def drive(payload : TAxiWPayload, value : WPack) : Unit = {
    payload.data #= value.data
    if (payload.cfg.hasStrb) {
      payload.strb #= value.strb
    }
    if (payload.cfg.hasBurst) {
      payload.last #= value.last
    }
  }

  def read(payload : TAxiWPayload) : WPack = {
    val data = payload.data.toBigInt

    val strb = if (payload.cfg.hasStrb) {
      payload.strb.toBigInt
    } else {
      (BigInt(1) << payload.cfg.dataBytes) - 1
    }

    val last = if (payload.cfg.hasBurst) {
      payload.last.toBoolean
    } else {
      true
    }

    apply(data, strb, last)(payload.cfg)
  }
}

