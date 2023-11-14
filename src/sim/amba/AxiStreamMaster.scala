package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiStreamConfig, TAxiStream}
import hwlib.sim.{ClockEnv, DelayQueue, Take, Touch, Leave, PoissonDist}



class AxiStreamMaster(val stm : TAxiStream, val mCfg : AxiModelConfig)(implicit val env : ClockEnv) {
  implicit val cfg : AxiStreamConfig = stm.cfg

  class Transaction(dest : Long,
                    id : Long,
                    data : Array[Option[Option[Byte]]]) {

    val tpackCount = (((data.length - 1) / cfg.dataBytes) + 1)
    val tpacks = (0 until tpackCount).map {
      (idx) =>
        val part = data.slice(idx * cfg.dataBytes, (idx + 1) * cfg.dataBytes)
                       .padTo(cfg.dataBytes, None)
        val partData = part.map((item) => item.map(_.getOrElse(0.toByte)).getOrElse(0.toByte))
        val partIsNull = part.map((item) => !item.isDefined)
        val partStrb = part.map((item) => item.map(_.isDefined).getOrElse(false))
        val partKeep = part.map((item) => item.isDefined)
        val tdata = BigInt(1, partData.reverse)
        val tstrb = BigInt(partStrb.reverse.map((b) => if (b) "1" else "0").mkString(""), 2)
        val tkeep = BigInt(partKeep.reverse.map((b) => if (b) "1" else "0").mkString(""), 2)
        TPack(tdata, tkeep, tstrb, id, dest, idx == (tpackCount - 1))
    }

    var tpackIdx = 0
    def takeTPack() : TPack = {
      val tpack = tpacks(tpackIdx)
      if (tpackIdx < tpacks.indices.last) {
        tpackIdx += 1
      }
      tpack
    }

    def identifier = (dest << 32) | id
  }

  val tSend = new ChannelSender(stm.t, mCfg.dSendCfg)(TPack.drive)

  val qIssue = new DelayQueue[Transaction](mCfg.qIssCfg)

  env.dbg.addChild(this, tSend,  "tS")
  env.dbg.addChild(this, qIssue, "qIss")

  tSend.onPut {
    () =>
      var action : Option[(TPack, Long)] = None
      qIssue.getWith(None) {
        // TODO-lw check why issuePrio seems not to work...
        (trn, _i) =>
          val tpack = trn.takeTPack()
          action = Some((tpack, trn.identifier))
          if (tpack.last) {
            Take
          } else {
            Touch
          }
      }
      action
  }

  def sendPosNull(data : Array[Option[Option[Byte]]],
           dest : Long = 0L,
           id : Long = 0L) : Unit = {
    val trn = new Transaction(dest, id, data)
    env.dbg.printForTag(this, "AxiStreamMaster", s"Initiate ${trn}")
    qIssue.put(trn, trn.identifier)
  }

  def sendPos(data : Array[Option[Byte]],
           nullBytes : BytePattern = Contiguous(),
           dest : Long = 0L,
           id : Long = 0L) : Unit = {
    sendPosNull(nullBytes.expand(data), dest, id)
  }

  def send(data : Array[Byte],
           nullBytes : BytePattern = Contiguous(),
           posBytes : BytePattern = Contiguous(),
           dest : Long = 0L,
           id : Long = 0L) : Unit = {
    sendPosNull(nullBytes.expand(posBytes.expand(data)), dest, id)
  }

  def sendWords(words : Array[BigInt],
                wordBytes : Int,
                bigEndian : Boolean = false,
                nullBytes : BytePattern = Contiguous(),
                posBytes : BytePattern = Contiguous(),
                dest : Long = 0L,
                id : Long = 0L) : Unit = {
    val data = words.map {
        (word) =>
          val wordData = if (bigEndian) { word.toByteArray } else { word.toByteArray.reverse }
          wordData.padTo(wordBytes, 0.toByte).slice(0, wordBytes)
      }.flatten.toArray
    send(data, nullBytes, posBytes, dest, id)
  }

  def sendShorts(shorts : Array[Short],
                 bigEndian : Boolean = false,
                 nullBytes : BytePattern = Contiguous(),
                 posBytes : BytePattern = Contiguous(),
                 dest : Long = 0L,
                 id : Long = 0L) : Unit = {
    sendWords(shorts.map(BigInt(_)), 2, bigEndian, nullBytes, posBytes, dest, id)
  }

  def sendInts(ints : Array[Int],
               bigEndian : Boolean = false,
               nullBytes : BytePattern = Contiguous(),
               posBytes : BytePattern = Contiguous(),
               dest : Long = 0L,
               id : Long = 0L) : Unit = {
    sendWords(ints.map(BigInt(_)), 4, bigEndian, nullBytes, posBytes, dest, id)
  }

  def sendLongs(longs : Array[Long],
                bigEndian : Boolean = false,
                nullBytes : BytePattern = Contiguous(),
                posBytes : BytePattern = Contiguous(),
                dest : Long = 0L,
                id : Long = 0L) : Unit = {
    sendWords(longs.map(BigInt(_)), 8, bigEndian, nullBytes, posBytes, dest, id)
  }

}


abstract class BytePattern {

  def expand(data : Array[Option[Byte]]) : Array[Option[Option[Byte]]] = {
    var idx = 0
    pattern(data.length).map {
      (valid) =>
        if (valid) {
          val item = data(idx)
          idx += 1
          Some(item)
        } else {
          None.asInstanceOf[Option[Option[Byte]]]
        }
    }
  }

  def expand(data : Array[Byte]) : Array[Option[Byte]] = {
    var idx = 0
    pattern(data.length).map {
      (valid) =>
        if (valid) {
          val item = data(idx)
          idx += 1
          Some(item)
        } else {
          None.asInstanceOf[Option[Byte]]
        }
    }
  }

  def pattern(count : Int) : Array[Boolean]

  def sequence(count : Int, value : Boolean) : Array[Boolean] = (0 until count).map((_i) => value).toArray

  def partition(count : Int, nth : Int)(inter : (Int) => Int) : Array[Boolean] = {
    val div = count / nth
    val mod = count % nth
    if (div == 0) {
      sequence(mod, true)
    } else if (mod == 0) {
      (0 until (div - 1)).map((idx) => Seq(sequence(nth, true), sequence(inter(idx), false))).flatten.flatten.toArray ++ sequence(nth, true)
    } else {
      (0 until div).map((idx) => Seq(sequence(nth, true), sequence(inter(idx), false))).flatten.flatten.toArray ++ sequence(mod, true)
    }
  }

}

case class Contiguous(initial : Int = 0) extends BytePattern {
  override def pattern(count : Int) : Array[Boolean] = {
    sequence(initial, false) ++ sequence(count, true)
  }
}

case class FixedDist(dist : Int, nth : Int = 1, initial : Int = 0) extends BytePattern {
  override def pattern(count : Int) : Array[Boolean] = {
    sequence(initial, false) ++ partition(count, nth)((idx) => dist)
  }
}

case class FixedRotate(dist : Int, nth : Int = 1, initial : Int = 0) extends BytePattern {
  def period(idx : Int) = (idx % (dist / nth)) == ((dist / nth) - 1)
  override def pattern(count : Int) : Array[Boolean] = {
    sequence(initial, false) ++ partition(count, nth)((idx) => (if (period(idx)) 0 else dist))
  }
}

case class AverageDist(distAvg : Double, nth : Int = 1, initial : Int = 0) extends BytePattern {
  val dist = PoissonDist(0.0 max distAvg)
  override def pattern(count : Int) : Array[Boolean] = {
    sequence(initial, false) ++ partition(count, nth)((idx) => dist().toInt)
  }
}

case class AverageRate(rateAvg : Double, nth : Int = 1, initial : Int = 0) extends BytePattern {
  val rateClamped = Double.MinPositiveValue max rateAvg min 1.0
  val dist = PoissonDist((1.0 - rateClamped) / rateClamped * nth)
  override def pattern(count : Int) : Array[Boolean] = {
    sequence(initial, false) ++ partition(count, nth)((idx) => dist().toInt)
  }
}


