package hwlib.sim.amba

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.amba.{ApbConfig, TApb}
import hwlib.sim.{Future, ClockEnv, DelayQueue, DelayQueueConfig, Take, Touch, Leave, PoissonDist}


case class ApbResult(error : Boolean, data : BigInt) {
  def isWrite = (data == null)
}

class ApbMaster(val apb : TApb, val qCfg : DelayQueueConfig)(implicit val env : ClockEnv) {
  implicit val cfg : ApbConfig = apb.cfg

  class Transaction(val addr : Long,
                    val data : BigInt,
                    val strb : BigInt,
                    val prot : Int) {
    def this(addr : Long, prot : Int) = this(addr, null, null, prot)

    val future = Future[ApbResult]()

    def isWrite = (data != null)

    override def toString() =  if (isWrite) {
      f"ApbWrite(0x$addr%x, ${data.toString(16)}, mask=${strb.toString(16)}, prot=$prot)"
    } else {
      f"ApbRead(0x$addr%x, prot=$prot)"
    }

  }

  val queue = new DelayQueue[Transaction](qCfg)
  env.dbg.addChild(this, queue, "qT")

  var state = 0
  var current : Option[Transaction] = None
  apb.sel #= false
  apb.enable #= false

  env.onClk { () =>
    if (state == 0) {
      val item = queue.rawGet()
      if (item.isDefined) {
        apb.addr #= item.get.addr
        if (cfg.hasProt) {
          apb.prot #= item.get.prot
        }
        if (item.get.isWrite) {
          apb.wdata #= item.get.data
          if (cfg.hasStrb) {
            apb.strb #= item.get.strb
          }
        }
        apb.write #= item.get.isWrite
        apb.sel #= true
        apb.enable #= false
        state = 1
        current = item
      }
    } else if (state == 1) {
      apb.sel #= true
      apb.enable #= true
      state = 2
    } else {
      if (apb.ready.toBoolean) {
        val error = if (cfg.hasErr) {
          apb.slverr.toBoolean
        } else {
          false
        }
        if (current.get.isWrite) {
          current.get.future.resolve(ApbResult(error, null))
        } else {
          current.get.future.resolve(ApbResult(error, apb.rdata.toBigInt))
        }
        apb.sel #= false
        apb.enable #= false
        state = 0
        current = None
      }
    }
  }

  def read(addr : Long, prot : Int = 0) : Future[ApbResult] = {
    val trn = new Transaction(addr, prot)
    env.dbg.printForTag(this, "ApbMaster", s"Initiate ${trn}")
    queue.put(trn)
    trn.future
  }

  def write(addr : Long, data : BigInt, prot : Int = 0) : Future[ApbResult] = {
    val trn = new Transaction(addr, data, (BigInt(1) << cfg.dataBytes) - 1, prot)
    env.dbg.printForTag(this, "ApbMaster", s"Initiate ${trn}")
    queue.put(trn)
    trn.future
  }

  def writeMask(addr : Long, data : BigInt, mask : BigInt, prot : Int = 0) : Future[ApbResult] = {
    val trn = new Transaction(addr, data, mask, prot)
    env.dbg.printForTag(this, "ApbMaster", s"Initiate ${trn}")
    queue.put(trn)
    trn.future
  }

  def bread(addr : Long, prot : Int = 0) : ApbResult = {
    read(addr, prot).blockValue()
  }

  def bwrite(addr : Long, data : BigInt, prot : Int = 0) : ApbResult = {
    write(addr, data, prot).blockValue()
  }

  def bwriteMask(addr : Long, data : BigInt, mask : BigInt, prot : Int = 0) : ApbResult = {
    writeMask(addr, data, mask, prot).blockValue()
  }

}


