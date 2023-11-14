package hwlib.base

import spinal.core._

import hwlib.xilinx.tools.XilinxInterfaceType
import hwlib.bundle._


sealed trait MemoryPortKind
case object MemoryPortRead extends MemoryPortKind
case object MemoryPortWrite extends MemoryPortKind
case object MemoryPortBoth extends MemoryPortKind

case class MemoryPortConfig(
    kind : MemoryPortKind,
    addrBits : Int,
    dataBits : Int,
    hasReq   : Boolean = true,
    hasGrant : Boolean = true,
    hasError : Boolean = false,
    maskBits : Int = 0,
    policy   : ReadUnderWritePolicy = dontCare) {
  require(addrBits > 0)
  require(dataBits > 0)
  require(maskBits >= 0)
  require(maskBits == 0 || dataBits % maskBits == 0)

  val isRead = kind == MemoryPortRead
  val isWrite = kind == MemoryPortWrite
  val isBoth = kind == MemoryPortBoth
  val canWrite = isWrite || isBoth
  val canRead = isRead || isBoth
  val hasMask = canWrite && maskBits > 0

  def TAddr = UInt(addrBits bits)
  def TData = Bits(dataBits bits)
  def TMask = hasMask generate Bits(maskBits bits)
}

class TMemoryPort(val cfg : MemoryPortConfig) extends AutoBundleBidir/* with XilinxInterfaceType*/ {
  val addr  = cfg.TAddr
  val wdata = cfg.canWrite generate cfg.TData
  val wmask = cfg.hasMask generate cfg.TMask
  val write = cfg.isBoth generate Bool
  val req   = cfg.hasReq generate Bool
  val grant = cfg.hasGrant generate Bool
  val rdata = cfg.canRead generate cfg.TData
  val error = cfg.hasError generate Bool

  element("addr", false)
  element("wdata", false)
  element("wmask", false)
  element("write", false)
  element("req", false)
  element("grant", true)
  element("rdata", true)
  element("error", true)

  def getReq = if (cfg.hasReq) { req } else { True }
  def setReq(v : Bool) = if (cfg.hasReq) { req := v }

  def getGrant = if (cfg.hasGrant) { grant } else { True }
  def setGrant(v : Bool) = if (cfg.hasGrant) { grant := v }

  def getError = if (cfg.hasError) { error } else { False }
  def setError(v : Bool) = if (cfg.hasError) { error := v }

/*
 * X_INTERFACE_INFO n = "xilinx.com:interface:bram:1.0 <port> <name>"
 *  EN?, DOUT?, DIN?, WE?, ADDR, CLK, RST
 * X_INTERFACE_PARAMETER
 *  MASTER_TYPE(string, blank),
    MEM_ECC(string, blank),
    MEM_WIDTH(long),
    MEM_SIZE(long),
    READ_WRITE_MODE("READ_WRITE", "READ_ONLY", "WRITE_ONLY")
  def defineXilinxInterface() : String = {
    addr.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:bram:1.0 ${interface} ADDR"))
    irq.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
          s"SENSITIVITY ${sensitivity}"))
    if (cfg.hasId) {
      SpinalWarning("Interrupt id signals not supported by xilinx interface, leaving ${id} without annotation!")
    }
    if (cfg.hasAck) {
      SpinalWarning("Interrupt ack signals not supported by xilinx interface, leaving ${ack} without annotation!")
    }

    interface
  }

  override def isInterface = true
*/
}

object TMemoryPort {
  def apply(cfg : MemoryPortConfig) = new TMemoryPort(cfg)
}

