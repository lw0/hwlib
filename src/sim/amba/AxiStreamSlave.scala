package hwlib.sim.amba

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiStreamConfig, TAxiStream}
import hwlib.sim.{ClockEnv, DelayQueue, Take, Touch, Leave}



class AxiStreamPacket(tpacks : Buffer[TPack]) {
  val id = tpacks(0).id
  val dest = tpacks(0).dest
  val data = Buffer[Option[Byte]]()

  for (tpack <- tpacks) {
    val rawData = tpack.data.toByteArray.reverse.padTo(tpack.cfg.dataBytes, 0.toByte)
    for (idx <- 0 until tpack.cfg.dataBytes) {
      if (tpack.keep.testBit(idx)) {
        if (tpack.strb.testBit(idx)) {
          data.append(Some(rawData(idx)))
        } else {
          data.append(None)
        }
      }
    }
  }

  override def toString() : String = {
    val dataStr = data.map((opt) => opt.map((byte) => f"${byte}%02x").getOrElse("--")).mkString(" ")
    s"Packet(>${dest}@${id} ${dataStr})"
  }

  def onlyData = data.collect{case Some(b) => b}

  def asWords(wordBytes : Int, bigEndian : Boolean = false) : Array[Option[BigInt]] = {
    val wordCount = (((data.length - 1) / wordBytes) + 1)
    (0 until wordCount).map {
      (idx) =>
        val rawPart = data.slice(idx * wordBytes, (idx + 1) * wordBytes)
                       .padTo(wordBytes, None)
        if (rawPart.forall(_.isDefined)) {
          val part = rawPart.collect{case Some(b) => b}
          Some(BigInt(1, (if (bigEndian) part else part.reverse).toArray))
        } else {
          None
        }
    }.toArray
  }
  def onlyWords(wordBytes : Int, bigEndian : Boolean = false) = asWords(wordBytes, bigEndian).collect{case Some(w) => w}

  def asShorts(bigEndian : Boolean = false) = asWords(2, bigEndian).map(_.map(_.toShort))
  def onlyShorts(bigEndian : Boolean = false) = asShorts(bigEndian).collect{case Some(s) => s}

  def asInts(bigEndian : Boolean = false) = asWords(4, bigEndian).map(_.map(_.toInt))
  def onlyInts(bigEndian : Boolean = false) = asInts(bigEndian).collect{case Some(i) => i}

  def asLongs(bigEndian : Boolean = false) = asWords(8, bigEndian).map(_.map(_.toLong))
  def onlyLongs(bigEndian : Boolean = false) = asLongs(bigEndian).collect{case Some(l) => l}
}

class AxiStreamSlave(val stm : TAxiStream, val mCfg : AxiModelConfig)(implicit val env : ClockEnv) {
  implicit val cfg : AxiStreamConfig = stm.cfg


  val tRecv = new ChannelReceiver(stm.t, mCfg.dRecvCfg)(TPack.read)

  val pending = Map[Long, Buffer[TPack]]()

  val destHandlers = Map[Long, (AxiStreamPacket) => Unit]()
  var defaultHandler : Option[(AxiStreamPacket) => Unit] = None

  env.dbg.addChild(this, tRecv,  "tR")

  tRecv.onGet(None) {
    (tpack, _i) =>
      val ident = tpack.identity
      if (!pending.contains(ident)) {
        pending(ident) = Buffer[TPack]()
      }
      val tpacks = pending(ident)
      tpacks.append(tpack)
      if (tpack.last) {
        pending.remove(ident)
        val pkg = new AxiStreamPacket(tpacks)
        if (destHandlers.contains(pkg.dest)) {
          destHandlers(pkg.dest)(pkg)
        } else if (defaultHandler.isDefined) {
          defaultHandler.get(pkg)
        } else {
          SpinalWarning(s"Discarding received Packet(${pkg.data.length} bytes, dest=${pkg.dest} id=${pkg.id}) without handler")
        }
      }
      Take
  }

  def onRecvFrom(dest : Long)(call : (AxiStreamPacket) => Unit) = {
    if (destHandlers.contains(dest)) {
      SpinalWarning(s"Replacing existing receive handler for destination ${dest}")
    }
    destHandlers(dest) = call
  }

  def onRecv(call : (AxiStreamPacket) => Unit) = {
    if (defaultHandler.isDefined) {
      SpinalWarning(s"Replacing existing default receive handler")
    }
    defaultHandler = Some(call)
  }

}

