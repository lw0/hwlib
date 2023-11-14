package hwlib.sim

import scala.collection.mutable
import scala.util.Random

import spinal.core._
import spinal.core.sim._

import hwlib.base.TMemoryPort
import hwlib.sim._


class MemoryModel(dataBits : Int)(implicit val env : ClockEnv) {

  case class Operation(policy : ReadUnderWritePolicy, isWrite : Boolean, index : Long, mask : BigInt, shift : Int, data : BigInt = 0)

  case class PortInfo(portBits  : Int,
                      maskBits  : Int,
                      policy    : ReadUnderWritePolicy,
                      getAddr   : () => Long,
                      getWrite  : () => Boolean,
                      getWData  : () => BigInt,
                      getWMask  : () => BigInt,
                      getReq    : () => Boolean,
                      setRData  : (BigInt) => Unit,
                      setReady  : (Boolean) => Unit,
                      setError  : (Boolean) => Unit,
                      genDelay  : () => Int,
                      genError  : () => Boolean) {
    require(portBits > 0)
    require(dataBits % portBits == 0 && isPow2(dataBits / portBits))
    require(maskBits > 0)
    require(portBits % maskBits == 0)

    val subAddrBits = log2Up(dataBits / portBits)
    val subAddrMask = (1L << subAddrBits) - 1
    val maskUnitBits = portBits / maskBits
    val maskUnitMask = (BigInt(1) << maskUnitBits) - 1
    def makeBitMask(mask : BigInt, shift : Int) = {
      var bitMask = BigInt(0)
      for (idx <- 0 until maskBits) {
        if (mask.testBit(idx)) {
          bitMask |= maskUnitMask << (idx * maskUnitBits)
        }
      }
      bitMask << shift
    }

    var delay : Int = 0;

    // init outputs
    setRData(0)
    setReady(false)
    setError(false)

    def preUpdate() : Option[Operation] = {
      val op = if (delay > 0) {
        delay = delay - 1
        None
      } else if (getReq()) {
        delay = genDelay()
        val addr = getAddr()
        val index = addr >> subAddrBits
        val sub = (addr & subAddrMask).toInt
        val shift = sub * portBits
        var wmask = getWMask()
        val mask = makeBitMask(wmask, shift)
        Some(Operation(policy, getWrite(), index, mask, shift, getWData()))
      } else {
        None
      }
      setRData(BigInt(dataBits, Random.self))
      setError(Random.nextBoolean())
      setReady(delay == 0)
      op
    }

    def postUpdate(op : Operation, data : BigInt) {
      setRData((data & op.mask) >> op.shift)
      setError(genError())
    }
  }


  val contents = mutable.Map[Long, BigInt]()
  val ports = mutable.ArrayBuffer[PortInfo]()


  def connect(mPort : TMemoryPort,
              delayGen : => Int = 0,
              errorGen : => Boolean = false) {
    val portBits = mPort.cfg.dataBits
    val maskBits : Int = if (mPort.cfg.hasMask) { mPort.cfg.maskBits } else { 1 }
    val policy = mPort.cfg.policy

    val getAddr = () => mPort.addr.toLong
    val getWrite = () => if (mPort.cfg.isBoth) { mPort.write.toBoolean } else { mPort.cfg.isWrite }
    val getWData = () => if (mPort.cfg.canWrite) { mPort.wdata.toBigInt } else { BigInt(0) }
    val getWMask = () => if (mPort.cfg.hasMask) { mPort.wmask.toBigInt } else { BigInt(1) }
    val getReq = () => mPort.req.toBoolean

    val setRData = (v : BigInt)  => if (mPort.cfg.canRead) { mPort.rdata #= v }
    val setGrant = (v : Boolean) => if (mPort.cfg.hasGrant) { mPort.grant #= v }
    val setError = (v : Boolean) => if (mPort.cfg.hasError) { mPort.error #= v }

    ports += PortInfo(portBits  = mPort.cfg.dataBits,
                      maskBits  = if (mPort.cfg.hasMask) { mPort.cfg.maskBits } else { 1 },
                      policy    = mPort.cfg.policy,
                      getAddr   = () => mPort.addr.toLong,
                      getWrite  = () => if (mPort.cfg.isBoth) { mPort.write.toBoolean } else { mPort.cfg.isWrite },
                      getWData  = () => if (mPort.cfg.canWrite) { mPort.wdata.toBigInt } else { BigInt(0) },
                      getWMask  = () => if (mPort.cfg.hasMask) { mPort.wmask.toBigInt } else { BigInt(1) },
                      getReq    = () => mPort.req.toBoolean,
                      setRData  = (v : BigInt)  => if (mPort.cfg.canRead) { mPort.rdata #= v },
                      setReady  = (v : Boolean) => if (mPort.cfg.hasGrant) { mPort.grant #= v },
                      setError  = (v : Boolean) => if (mPort.cfg.hasError) { mPort.error #= v },
                      genDelay  = () => delayGen,
                      genError  = () => errorGen)
  }

  def clear() = contents.clear()


  env.onClk { () =>
    // collect operations
    val writes = mutable.Map[Long, mutable.ArrayBuffer[Operation]]()
    val reads = mutable.ArrayBuffer[(PortInfo, Operation)]()
    for (port <- ports) {
      port.preUpdate().map { op =>
        reads.append((port, op))
        if (op.isWrite) {
          writes.getOrElseUpdate(op.index, mutable.ArrayBuffer[Operation]()).append(op)
        }
      }
    }

    // detect and resolve write collisions
    val updateValues = mutable.Map[Long, BigInt]()
    val updateMasks = mutable.Map[Long, BigInt]()
    for ((index, ops) <- writes) {
      var value = contents.getOrElseUpdate(index, BigInt(dataBits, Random.self))
      var updateMask = BigInt(0)
      var collisionMask = BigInt(0)
      for (i <- 0 until ops.length) {
        val op = ops(i)
        value = value & ~op.mask | op.data & op.mask
        collisionMask |= updateMask & op.mask
        updateMask |= op.mask
      }
      if (collisionMask != 0) {
        env.dbg.warn(s"Write Collision at 0x${index}%x bits 0x${collisionMask.toString(16)}")
        value = value & ~collisionMask | BigInt(dataBits, Random.self) & collisionMask
      }
      updateValues += (index -> value)
      updateMasks += (index -> updateMask)
    }

    // complete read operations
    for ((port, op) <- reads) {
      val orig = contents.getOrElseUpdate(op.index, BigInt(dataBits, Random.self))
      val data = if (op.policy == readFirst || op.policy == eitherFirst && Random.nextBoolean()) {
        // return original value regardless if updated
        orig
      } else if (op.policy == writeFirst || op.policy == eitherFirst) {
        // return new value if updated
        updateValues.getOrElse(op.index, orig)
      } else { // op.policy == dontCare
        // randomize updated bits
        val updateMask = updateMasks.getOrElse(op.index, BigInt(0))
        orig & ~updateMask | BigInt(dataBits, Random.self) & updateMask
      }
      port.postUpdate(op, data)
    }

    contents ++= updateValues
  }

}

