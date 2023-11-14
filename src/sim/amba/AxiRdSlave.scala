package hwlib.sim.amba

import scala.collection.mutable.Buffer

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxi}
import hwlib.sim.{ClockEnv, Future, FutureSet, DelayQueue, Take, Touch, Leave, RangeMap}

import Resp.Resp



class AxiRdSlave(val axi : TAxi, val mCfg : AxiModelConfig)(implicit val env : ClockEnv) {
  require(axi.cfg.hasRd && axi.isMasterInterface)
  implicit val cfg : AxiConfig = axi.cfg

  class Transaction(val apack : APack)(implicit val cfg : AxiConfig) {

    def process(call : (Long, Int) => (Resp, BigInt)) : Seq[RPack] = {
      val tup = call(apack.base, apack.bytes)
      val resp = tup._1
      val data = tup._2
      val rpacks = Buffer[RPack]()
      apack.split(data) {
        (idx, bdata, last) =>
          rpacks.append(RPack(bdata, resp, last, apack.id))
      }
      rpacks.toSeq
    }

    def process(resp : Resp) : Seq[RPack] = {
      val rpacks = Buffer[RPack]()
      apack.genBurst {
        (idx, baddr, dpos, bpos, bcnt, last) =>
          rpacks.append(RPack(BigInt(0), resp, last, apack.id))
      }
      rpacks.toSeq
    }

    override def toString : String = {
      s"AxiRdSlave.Trn(${apack})"
    }
  }

  val handlers = new RangeMap[(Long, Int) => (Resp, BigInt)]()

  val arRecv = new ChannelReceiver(axi.ar, mCfg.aRecvCfg)(APack.read)
  val rSend = new ChannelSender(axi.r, mCfg.dSendCfg)(RPack.drive)

  val qProcess = new DelayQueue[Transaction](mCfg.qProcCfg)

  var blockProcess = FutureSet.All.get()

  env.dbg.addChild(this, arRecv,   "arR")
  env.dbg.addChild(this, rSend,    "rS")
  env.dbg.addChild(this, qProcess, "qProc")

  arRecv.onGet(None) {
    (apack, _i) =>
      val didPut = qProcess.putWith{
        () =>
          Some((new Transaction(apack), apack.id))
      }
      if (didPut) { Take } else { Leave }
  }

  qProcess.onGet(None) {
    (trn, _i) =>
      if (blockProcess.isResolved) {
        val handler = handlers.get(trn.apack.addr)
        val rpacks = if (handler.isDefined) {
          env.dbg.printForTag(this, "AxiRdSlave", s"Process ${trn}")
          trn.process(handler.get)
        } else {
          env.dbg.printForTag(this, "AxiRdSlave", s"Reject ${trn}")
          trn.process(Resp.DecErr)
        }
        blockProcess = FutureSet.All
          .add(rpacks.map(rSend.put(_)))
          .get()
        Take
      } else {
        Leave
      }
  }

  def onRead(base : Long, count : Long)(call : (Long, Int) => (Resp, BigInt)) : Unit = {
    if (!handlers.add(base, count, call)) {
      SpinalWarning(s"Could not add AxiRdSlave handler for range (${base} + ${count})")
    }
  }

}

