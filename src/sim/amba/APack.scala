package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxiAPayload}

import Burst.Burst
import Size.Size



class APack(
  val addr : Long,
  val len : Int,
  val size : Size,
  val burst : Burst,
  val id : Long,
  val cfg : AxiConfig) {

  def genBurst(call : (Int, Long, Int, Int, Int, Boolean) => Unit) : Unit = {
    val bytesPerBus = 1 << cfg.fullSize
    val bytesPerBeat = 1 << size.id
    val bytesPerTrn = bytesPerBeat * len
    val baseAddr = addr & ~(bytesPerBeat - 1)
    val wrapAddr = addr & ~(bytesPerTrn - 1)
    // var dpos = 0
    for (idx <- 0 until len) {
      val curAddr = burst match {
        case Burst.Fixed => addr
        case Burst.Incr => if (idx == 0) {addr} else {baseAddr + idx * bytesPerBeat}
        case Burst.Wrap => wrapAddr + ((baseAddr + idx * bytesPerBeat) & (bytesPerTrn - 1))
      }
      val bpos = curAddr.toInt & (bytesPerBus - 1)
      val cnt  = bytesPerBeat - (bpos & (bytesPerBeat - 1))
      val dpos = burst match {
        case Burst.Fixed => idx * cnt
        case Burst.Incr => (curAddr - addr).toInt
        case Burst.Wrap => (curAddr - wrapAddr).toInt
      }
      call(idx, curAddr, dpos, bpos, cnt, idx == (len - 1))
      // dpos += cnt
    }
  }

  def base = burst match {
    case Burst.Fixed => addr
    case Burst.Incr => addr
    case Burst.Wrap => {
      val bytesPerBeat = 1 << size.id
      val bytesPerTrn = bytesPerBeat * len
      addr & ~(bytesPerTrn - 1)
    }
  }
  def bytes = len * (1 << size.id)

  override def toString : String = {
    s"APack(0x${addr.toHexString} ${burst}(${len} x ${size}) @${id})"
  }

  def split(data : BigInt, strb : BigInt)(call : (Int, BigInt, BigInt, Boolean) => Unit) : Unit = {
    genBurst{
      (idx : Int, addr : Long, dpos : Int, bpos : Int, cnt : Int, last : Boolean) =>
        val bdata = ((data >> (dpos * 8)) & ((BigInt(1) << (cnt * 8)) - 1)) << (bpos * 8)
        val bstrb = ((strb >> dpos) & ((BigInt(1) << cnt) - 1)) << bpos
        call(idx, bdata, bstrb, last)
    }
  }

  def split(data : BigInt)(call : (Int, BigInt, Boolean) => Unit) : Unit = {
    genBurst{
      (idx : Int, addr : Long, dpos : Int, bpos : Int, cnt : Int, last : Boolean) =>
        val bdata = ((data >> (dpos * 8)) & ((BigInt(1) << (cnt * 8)) - 1)) << (bpos * 8)
        call(idx, bdata, last)
    }
  }

  def join(call : (Int, Boolean) => (BigInt, BigInt)) : (BigInt, BigInt) = {
    var data, strb = BigInt(0)
    genBurst{
      (idx : Int, addr : Long, dpos : Int, bpos : Int, cnt : Int, last : Boolean) =>
        val tup = call(idx, last)
        data |= ((tup._1 >> (bpos * 8)) & ((BigInt(1) << (cnt * 8)) - 1)) << (dpos * 8)
        strb |= ((tup._2 >> bpos) & ((BigInt(1) << cnt) - 1)) << dpos
    }
    (data, strb)
  }

  def join(call : (Int, Boolean) => BigInt) : BigInt = {
    var data = BigInt(0)
    genBurst{
      (idx : Int, addr : Long, dpos : Int, bpos : Int, cnt : Int, last : Boolean) =>
        val bdata = call(idx, last)
        data |= ((bdata >> (bpos * 8)) & ((BigInt(1) << (cnt * 8)) - 1)) << (dpos * 8)
    }
    data
  }
}


object APack {

  def apply(addr : Long, len : Int, size : Size, burst : Burst, id : Long)(implicit cfg : AxiConfig) : APack = {
    // TODO-lw implement permissive option, warning about but not correcting protocol violations
    //  ?are Rd/Wr Masters robust against inconsistent transactions?
    //  BTW- are WPacks corrected accordingly, or does this checking introduce inconsistent transactions?!
    val actId = if (id > cfg.maxId) {
      SpinalWarning(s"Id ${id} above interface maximum ${cfg.maxId}, thus truncated to ${id & cfg.maxId}")
      id & cfg.maxId
    } else {
      id
    }

    val actBurst = if (!cfg.hasBurst && burst != Burst.Fixed) {
      SpinalWarning(s"Burst mode ${burst} not supported, thus set Fixed")
      Burst.Fixed
    } else {
      burst
    }

    val actSize = if (!cfg.hasBurst && size.id != cfg.fullSize) {
      SpinalWarning(s"Burst size ${size} not supported, thus set to bus size ${Size(cfg.fullSize)}")
      Size(cfg.fullSize)
    } else if (size.id > cfg.fullSize) {
      SpinalWarning(s"Burst size ${size} above, thus clamped to bus size ${Size(cfg.fullSize)}.")
      Size(cfg.fullSize)
    } else {
      size
    }

    val alignMask = ((1L << actSize.id) - 1)
    val actAddr = if (actBurst == Burst.Wrap && (addr & alignMask) != 0L) {
      SpinalWarning(s"Wrapping Burst addr ${addr.toHexString} unaligned to ${actSize}, thus realigned")
      addr & ~alignMask
    } else {
      addr
    }

    val bound = (1 << (12 - actSize.id))
    val maxlen = bound - ((actAddr >> actSize.id).toInt & (bound - 1))
    val actLen = if (!cfg.hasBurst && len != 1) {
      SpinalWarning(s"Burst length ${len} not supported, thus set to 1")
      1
    } else if (actBurst == Burst.Wrap && len < 2) {
      SpinalWarning(s"Wrapping Burst length ${len} invalid, thus corrected to 2")
      2
    } else if (actBurst == Burst.Wrap && 2 < len && len < 4) {
      SpinalWarning(s"Wrapping Burst length ${len} invalid, thus corrected to 4")
      4
    } else if (actBurst == Burst.Wrap && 4 < len && len < 8) {
      SpinalWarning(s"Wrapping Burst length ${len} invalid, thus corrected to 8")
      8
    } else if (actBurst == Burst.Wrap && (8 < len && len < 16 || 16 < len)) {
      SpinalWarning(s"Wrapping Burst length ${len} invalid, thus corrected to 16")
      16
    } else if (len < 1) {
      SpinalWarning(s"Burst length ${len} below, thus clamped to, minimum 1")
      1
    } else if (actBurst == Burst.Fixed && len > 16) {
      SpinalWarning(s"Fixed Burst length ${len} above, thus clamped to, maximum 16")
      16
    } else if (actBurst == Burst.Incr && len > maxlen) {
      SpinalWarning(s"Incr Burst length ${len} above, thus clamped to, 4K boundary")
      maxlen
    } else if (len > 256) {
      SpinalWarning(s"Burst length ${len} above, thus clamped to, maximum 256")
      256
    } else {
      len
    }

    new APack(actAddr, actLen, actSize, actBurst, actId, cfg)
  }

  def drive(payload : TAxiAPayload, value : APack) : Unit = {
    payload.addr #= value.addr
    if (payload.cfg.hasBurst) {
      payload.len #= value.len - 1
      payload.size #= value.size.id
      payload.burst #= value.burst.id
    }
    if (payload.cfg.hasId) {
      payload.id #= value.id
    }
  }

  def read(payload : TAxiAPayload) : APack = {
    val addr = payload.addr.toLong

    val len = if (payload.cfg.hasBurst) {
      payload.len.toInt + 1
    } else {
      1
    }

    val size = if (payload.cfg.hasBurst) {
      // Size accepts 0..7 matching payload.size/UInt(3 bits) range, no check needed
      Size(payload.size.toInt)
    } else {
      Size(payload.cfg.fullSize)
    }

    val burst = if (payload.cfg.hasBurst) {
      val rawBurst = payload.burst.toInt
      if (rawBurst > Burst.maxId) {
        SpinalWarning(s"Invalid burst signal 0x${rawBurst.toHexString}, force Fixed")
        Burst.Fixed
      } else {
        Burst(rawBurst)
      }
    } else {
      Burst.Fixed
    }

    val id = if (payload.cfg.hasId) {
      payload.id.toLong
    } else {
      0L
    }

    apply(addr, len, size, burst, id)(payload.cfg)
  }
}

