package hwlib.sim.amba

import scala.collection.mutable.Buffer

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxi}
import hwlib.sim.{ClockEnv, Future, FutureSet, DelayQueue, Take, Touch, Leave}

import Resp.Resp
import Burst.Burst
import Size.Size


case class AxiWrResult(resp : Resp)

class AxiWrMaster(val axi : TAxi, val mCfg : AxiModelConfig)(implicit val env : ClockEnv) {
  require(axi.cfg.hasWr && !axi.isMasterInterface)
  implicit val cfg : AxiConfig = axi.cfg

  class Transaction(
    addr : Long,
    data : BigInt,
    strb : BigInt,
    len : Int,
    size : Size,
    burst : Burst,
    id : Long)(implicit cfg : AxiConfig) {

    val apack = APack(addr, len, size, burst, id)

    val wpacks = Buffer[WPack]()
    apack.split(data, strb) {
      (idx, bdata, bstrb, last) =>
        wpacks.append(WPack(bdata, bstrb, last))
    }

    val future = Future[AxiWrResult]()

    def resolve(bpack : BPack) : Unit = {
      future.resolve(AxiWrResult(bpack.resp))
    }

    override def toString : String = {
      s"AxiWrMaster.Trn(${apack} ${wpacks.size} WPacks)"
    }
  }

  val awSend = new ChannelSender(axi.aw, mCfg.aSendCfg)(APack.drive)
  val wSend = new ChannelSender(axi.w, mCfg.dSendCfg)(WPack.drive)
  val bRecv = new ChannelReceiver(axi.b, mCfg.bRecvCfg)(BPack.read)

  val qIssue = new DelayQueue[Transaction](mCfg.qIssCfg)
  val qResolve = new DelayQueue[Transaction](mCfg.qResCfg)

  var blockIssue = FutureSet.All.get()

  env.dbg.addChild(this, awSend,   "awS")
  env.dbg.addChild(this, wSend,    "wS")
  env.dbg.addChild(this, bRecv,    "bR")
  env.dbg.addChild(this, qIssue,   "qIss")
  env.dbg.addChild(this, qResolve, "qRes")

  qIssue.onGet(None) {
    (trn, _i) =>
      if (blockIssue.isResolved) {
        blockIssue = FutureSet.All
          .add(qResolve.put(trn, trn.apack.id))
          .add(awSend.put(trn.apack))
          .add(trn.wpacks.map(wSend.put(_)))
          .get()
        Take
      } else {
        Leave
      }
  }

  bRecv.onGet(None){
    (bpack, _i) =>
      qResolve.getWith(Some(bpack.id)){
        (trn, _ii) =>
          trn.resolve(bpack)
          Take // take trn
      } // take bpack if trn was found
  }

  def writeStrbOn(id : Long,
                  addr : Long,
                  data : BigInt,
                  strb : BigInt,
                  len : Int,
                  size : Size,
                  burst : Burst) : Future[AxiWrResult] = {
    val trn = new Transaction(addr, data, strb, len, size, burst, id)
    env.dbg.printForTag(this, "AxiWrMaster", s"Initiate ${trn}")
    qIssue.put(trn, id)
    trn.future
  }

  def writeStrbOn(id : Long,
                  addr : Long,
                  data : BigInt,
                  strb : BigInt,
                  len : Int,
                  burst : Burst = Burst.Incr) : Future[AxiWrResult] = {
    writeStrbOn(id, addr, data, strb, len, Size(cfg.fullSize), burst)
  }

  def writeStrbOn(id : Long,
                  addr : Long,
                  data : BigInt,
                  strb : BigInt) : Future[AxiWrResult] = {
    writeStrbOn(id, addr, data, strb, 1, Size(cfg.fullSize), Burst.Fixed)
  }

  def writeOn(id : Long,
              addr : Long,
              data : BigInt,
              len : Int,
              size : Size,
              burst : Burst) : Future[AxiWrResult] = {
    val strb = (BigInt(1) << ((1 << size.id) * len)) - 1
    writeStrbOn(id, addr, data, strb, len, size, burst)
  }

  def writeOn(id : Long,
              addr : Long,
              data : BigInt,
              len : Int,
              burst : Burst = Burst.Incr) : Future[AxiWrResult] = {
    writeOn(id, addr, data, len, Size(cfg.fullSize), burst)
  }

  def writeOn(id : Long,
              addr : Long,
              data : BigInt) : Future[AxiWrResult] = {
    writeOn(id, addr, data, 1, Size(cfg.fullSize), Burst.Fixed)
  }

  def writeStrb(addr : Long,
                data : BigInt,
                strb : BigInt,
                len : Int,
                size : Size,
                burst : Burst) : Future[AxiWrResult] = {
    writeStrbOn(0L, addr, data, strb, len, size, burst)
  }

  def writeStrb(addr : Long,
                data : BigInt,
                strb : BigInt,
                len : Int,
                burst : Burst = Burst.Incr) : Future[AxiWrResult] = {
    writeStrbOn(0L, addr, data, strb, len, burst)
  }

  def writeStrb(addr : Long,
                data : BigInt,
                strb : BigInt) : Future[AxiWrResult] = {
    writeStrbOn(0L, addr, data, strb)
  }

  def write(addr : Long,
            data : BigInt,
            len : Int,
            size : Size,
            burst : Burst) : Future[AxiWrResult] = {
    writeOn(0L, addr, data, len, size, burst)
  }

  def write(addr : Long,
            data : BigInt,
            len : Int,
            burst : Burst = Burst.Incr) : Future[AxiWrResult] = {
    writeOn(0L, addr, data, len, burst)
  }

  def write(addr : Long,
            data : BigInt) : Future[AxiWrResult] = {
    writeOn(0L, addr, data)
  }

  def bwriteStrbOn(id : Long,
                  addr : Long,
                  data : BigInt,
                  strb : BigInt,
                  len : Int,
                  size : Size,
                  burst : Burst) : AxiWrResult = {
    writeStrbOn(id, addr, data, strb, len, size, burst).blockValue()
  }

  def bwriteStrbOn(id : Long,
                  addr : Long,
                  data : BigInt,
                  strb : BigInt,
                  len : Int,
                  burst : Burst = Burst.Incr) : AxiWrResult = {
    writeStrbOn(id, addr, data, strb, len, burst).blockValue()
  }

  def bwriteStrbOn(id : Long,
                  addr : Long,
                  data : BigInt,
                  strb : BigInt) : AxiWrResult = {
    writeStrbOn(id, addr, data, strb).blockValue()
  }

  def bwriteOn(id : Long,
              addr : Long,
              data : BigInt,
              len : Int,
              size : Size,
              burst : Burst) : AxiWrResult = {
    writeOn(id, addr, data, len, size, burst).blockValue()
  }

  def bwriteOn(id : Long,
              addr : Long,
              data : BigInt,
              len : Int,
              burst : Burst = Burst.Incr) : AxiWrResult = {
    writeOn(id, addr, data, len, burst).blockValue()
  }

  def bwriteOn(id : Long,
              addr : Long,
              data : BigInt) : AxiWrResult = {
    writeOn(id, addr, data).blockValue()
  }

  def bwriteStrb(addr : Long,
                data : BigInt,
                strb : BigInt,
                len : Int,
                size : Size,
                burst : Burst) : AxiWrResult = {
    writeStrb(addr, data, strb, len, size, burst).blockValue()
  }

  def bwriteStrb(addr : Long,
                data : BigInt,
                strb : BigInt,
                len : Int,
                burst : Burst = Burst.Incr) : AxiWrResult = {
    writeStrb(addr, data, strb, len, burst).blockValue()
  }

  def bwriteStrb(addr : Long,
                data : BigInt,
                strb : BigInt) : AxiWrResult = {
    writeStrb(addr, data, strb).blockValue()
  }

  def bwrite(addr : Long,
            data : BigInt,
            len : Int,
            size : Size,
            burst : Burst) : AxiWrResult = {
    write(addr, data, len, size, burst).blockValue()
  }

  def bwrite(addr : Long,
            data : BigInt,
            len : Int,
            burst : Burst = Burst.Incr) : AxiWrResult = {
    write(addr, data, len, burst).blockValue()
  }

  def bwrite(addr : Long,
            data : BigInt) : AxiWrResult = {
    write(addr, data).blockValue()
  }

}

