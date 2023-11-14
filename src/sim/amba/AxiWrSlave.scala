package hwlib.sim.amba

import scala.collection.mutable.Buffer

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{AxiConfig, TAxi}
import hwlib.sim.{ClockEnv, Future, FutureSet, DelayQueue, GetAction, Take, Touch, Leave, RangeMap}

import Resp.Resp



class AxiWrSlave(val axi : TAxi, val mCfg : AxiModelConfig)(implicit val env : ClockEnv) {
  require(axi.cfg.hasWr && axi.isMasterInterface)
  implicit val cfg : AxiConfig = axi.cfg

  case class Transaction(val apack : APack)(implicit cfg : AxiConfig) {
    val wpacks = Buffer[WPack]()

    def addWPack(wpack : WPack) : Boolean = {
      wpacks.append(wpack)
      if (wpack.last && wpacks.length < apack.len) {
        SpinalWarning(s"Write burst #${wpacks.length} terminates before #${apack.len})")
      } else if (!wpack.last && wpacks.length >= apack.len) {
        SpinalWarning(s"Write burst #${wpacks.length} continues after #{apack.len}")
      }
      wpack.last
    }


    def process(call : (Long, Int, BigInt, BigInt) => Resp) : BPack = {
      val tup = apack.join{
        (idx, last) =>
          if (wpacks.isDefinedAt(idx)) {
            (wpacks(idx).data, wpacks(idx).strb)
          } else {
            (BigInt(0), BigInt(0))
          }
      }
      val data = tup._1
      val strb = tup._2
      val resp = call(apack.base, apack.bytes, data, strb)
      BPack(resp, apack.id)
    }

    def process(resp : Resp) : BPack = {
      BPack(resp, apack.id)
    }

    override def toString : String = {
      s"AxiWrSlave.Trn(${apack} ${wpacks.size} WPacks)"
    }
  }

  val handlers = new RangeMap[(Long, Int, BigInt, BigInt) => Resp]()

  val awRecv = new ChannelReceiver(axi.aw, mCfg.aRecvCfg)(APack.read)
  val wRecv = new ChannelReceiver(axi.w, mCfg.dRecvCfg)(WPack.read)
  val bSend = new ChannelSender(axi.b, mCfg.bSendCfg)(BPack.drive)

  val qProcess = new DelayQueue[Transaction](mCfg.qProcCfg)

  env.dbg.addChild(this, awRecv,   "awR")
  env.dbg.addChild(this, wRecv,    "wR")
  env.dbg.addChild(this, bSend,    "bS")
  env.dbg.addChild(this, qProcess, "qProc")

  var current : Option[Transaction] = None
  var blockProcess = FutureSet.All.get()

  awRecv.onGet(None) {
    (apack, _i) =>
      if (current.isEmpty) {
        current = Some(Transaction(apack))
        Take
      } else {
        Leave
      }
  }
  // TODO-lw handle race condition between awRecv and wRecv...
  wRecv.onGet(None) {
    (wpack, _i) =>
      var action : GetAction = Leave
      qProcess.putWith {
        () =>
          if (current.isDefined) {
            val trn = current.get
            action = Take
            if (trn.addWPack(wpack)) {
              current = None
              Some((trn, trn.apack.id))
            } else {
              None
            }
          } else {
            None
          }
      }
      action
  }

  qProcess.onGet(None) {
    (trn, _i) =>
      if (blockProcess.isResolved) {
        val handler = handlers.get(trn.apack.addr)
        val bpack = if (handler.isDefined) {
          env.dbg.printForTag(this, "AxiWrSlave", s"Process ${trn}")
          trn.process(handler.get)
        } else {
          env.dbg.printForTag(this, "AxiWrSlave", s"Reject ${trn}")
          trn.process(Resp.DecErr)
        }
        blockProcess = FutureSet.All
          .add(bSend.put(bpack))
          .get()
        Take
      } else {
        Leave
      }
  }

  def onWrite(base : Long, count : Long)(call : (Long, Int, BigInt, BigInt) => Resp) : Unit = {
    if (!handlers.add(base, count, call)) {
      SpinalWarning(s"Could not add AxiWriteSlave handler for range (${base} + ${count})")
    }
  }

}

