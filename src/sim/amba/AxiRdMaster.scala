package hwlib.sim.amba

import scala.collection.mutable.Buffer

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.base.HexString
import hwlib.amba.{AxiConfig, TAxi}
import hwlib.sim.{ClockEnv, Future, FutureSet, DelayQueue, GetAction, Take, Touch, Leave}

import Resp.Resp
import Burst.Burst
import Size.Size


case class AxiRdResult(resp : Resp, data : BigInt, cfg : AxiConfig) {

  override def toString = s"AxiRdResult(${resp} ${data.bitLength} bits: ${HexString(data, unit=cfg.dataBytes * 2)})"

}

class AxiRdMaster(val axi : TAxi, val mCfg : AxiModelConfig)(implicit val env : ClockEnv) {
  require(axi.cfg.hasRd && !axi.isMasterInterface)
  implicit val cfg : AxiConfig = axi.cfg

  class Transaction(
    addr : Long,
    len : Int,
    size : Size,
    burst : Burst,
    id : Long)(implicit cfg : AxiConfig) {

    val apack = APack(addr, len, size, burst, id)

    var rpacks = Buffer[RPack]()

    val future = Future[AxiRdResult]()

    def addRPack(rpack : RPack) : Boolean = {
      rpacks.append(rpack)
      if (rpack.last && rpacks.length < apack.len) {
        SpinalWarning(s"Read burst #${rpacks.length} terminates before #${apack.len})")
      } else if (!rpack.last && rpacks.length >= apack.len) {
        SpinalWarning(s"Read burst #${rpacks.length} continues after #{apack.len}")
      }
      rpack.last
    }

    def resolve() : Unit = {
      var resp = Resp.Okay
      val data = apack.join {
        (idx, last) =>
          if (rpacks.indices.contains(idx)) {
            resp = Resp.max(resp, rpacks(idx).resp)
            rpacks(idx).data
          } else {
            BigInt(0)
          }
      }
      future.resolve(AxiRdResult(resp, data, cfg))
    }

    override def toString : String = {
      s"AxiRdMaster.Trn(${apack} ${rpacks.size} RPacks)"
    }
  }

  val arSend = new ChannelSender(axi.ar, mCfg.aSendCfg)(APack.drive)
  val rRecv = new ChannelReceiver(axi.r, mCfg.dRecvCfg)(RPack.read)

  val qIssue = new DelayQueue[Transaction](mCfg.qIssCfg)
  val qResolve = new DelayQueue[Transaction](mCfg.qResCfg)

  var blockIssue = FutureSet.All.get()

  env.dbg.addChild(this, arSend,   "arS")
  env.dbg.addChild(this, rRecv,    "rR")
  env.dbg.addChild(this, qIssue,   "qIss")
  env.dbg.addChild(this, qResolve, "qRes")

  qIssue.onGet(None) {
    (trn, _i) =>
      if (blockIssue.isResolved) {
        blockIssue = FutureSet.All
          .add(qResolve.put(trn, trn.apack.id))
          .add(arSend.put(trn.apack))
          .get()
        Take
      } else {
        Leave
      }
  }

  rRecv.onGet(None){
    (rpack, _i) =>
      var action : GetAction = Leave
      qResolve.getWith(Some(rpack.id)){
        (trn, _ii) =>
          action = Take
          if (trn.addRPack(rpack)) {
            trn.resolve()
            Take
          } else {
            Leave
          }
      }
      action
  }

  def readOn(id : Long,
             addr : Long,
             len : Int,
             size : Size,
             burst : Burst) : Future[AxiRdResult] = {
    val trn = new Transaction(addr, len, size, burst, id)
    env.dbg.printForTag(this, "AxiRdMaster", s"Initiate ${trn}")
    qIssue.put(trn, id)
    trn.future
  }

  def readOn(id : Long,
             addr : Long,
             len : Int,
             burst : Burst = Burst.Incr) : Future[AxiRdResult] = {
    readOn(id, addr, len, Size(cfg.fullSize), burst)
  }

  def readOn(id : Long,
             addr : Long) : Future[AxiRdResult] = {
    readOn(id, addr, 1, Size(cfg.fullSize), Burst.Fixed)
  }

  def read(addr : Long,
           len : Int,
           size : Size,
           burst : Burst) : Future[AxiRdResult] = {
    readOn(0L, addr, len, size, burst)
  }

  def read(addr : Long,
           len : Int,
           burst : Burst = Burst.Incr) : Future[AxiRdResult] = {
    readOn(0L, addr, len, burst)
  }

  def read(addr : Long) : Future[AxiRdResult] = {
    readOn(0L, addr)
  }


  def breadOn(id : Long,
             addr : Long,
             len : Int,
             size : Size,
             burst : Burst) : AxiRdResult = {
    readOn(id, addr, len, size, burst).blockValue()
  }

  def breadOn(id : Long,
             addr : Long,
             len : Int,
             burst : Burst = Burst.Incr) : AxiRdResult = {
    readOn(id, addr, len, burst).blockValue()
  }

  def breadOn(id : Long,
             addr : Long) : AxiRdResult = {
    readOn(id, addr).blockValue()
  }

  def bread(addr : Long,
           len : Int,
           size : Size,
           burst : Burst) : AxiRdResult = {
    read(addr, len, size, burst).blockValue()
  }

  def bread(addr : Long,
           len : Int,
           burst : Burst = Burst.Incr) : AxiRdResult = {
    read(addr, len, burst).blockValue()
  }

  def bread(addr : Long) : AxiRdResult = {
    read(addr).blockValue()
  }

}

