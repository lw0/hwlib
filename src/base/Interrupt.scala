package hwlib.base

import spinal.core._

import hwlib.xilinx.tools.XilinxInterfaceType
import hwlib.bundle._


sealed trait IrqSensitivity
case object IrqHigh extends IrqSensitivity
case object IrqLow extends IrqSensitivity
case object IrqRising extends IrqSensitivity
case object IrqFalling extends IrqSensitivity

case class IrqConfig(
    sense : IrqSensitivity = IrqHigh,
    hasAck : Boolean = false,
    idBits : Int = 0) {
  def hasId = idBits > 0
}

case class TIrq(cfg : IrqConfig) extends AutoBundleBidir with XilinxInterfaceType {
  val irq = Bool
  val id  = ifGen(cfg.hasId) { UInt(cfg.idBits bits) }
  val ack = ifGen(cfg.hasAck) { Bool }

  element("irq", false)
  element("id",  false)
  element("ack", true)

  irq.setPartialName("")

  def defineXilinxInterface() : String = {
    val sensitivity = cfg.sense match {
      case IrqHigh => "LEVEL_HIGH"
      case IrqLow => "LEVEL_LOW"
      case IrqRising => "EDGE_RISING"
      case IrqFalling => "EDGE_FALLING"
    }
    val interface = irq.getName()

    irq.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:signal:interrupt:1.0 ${interface} INTERRUPT"))
    irq.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
          s"SENSITIVITY ${sensitivity}"))
    if (cfg.hasId) {
      SpinalWarning("Irq id signals not supported by xilinx interface, leaving ${id} without annotation!")
    }
    if (cfg.hasAck) {
      SpinalWarning("Irq ack signals not supported by xilinx interface, leaving ${ack} without annotation!")
    }

    interface
  }

  override def isInterface = false
}


