package hwlib.serial

import spinal.core._

import hwlib.bundle._
import hwlib.amba.Channel
import hwlib.xilinx.tools.XilinxInterfaceType


case class SerialConfig(
  hasData : Boolean = true,
  hasHandshake : Boolean = false,
  hasPresence : Boolean = false,
  hasModem : Boolean = false,
  negData : Boolean = false,
  negHandshake : Boolean = false,
  negPresence : Boolean = false,
  negCarrier : Boolean = false,
  negRing : Boolean = false)


case class TSerial(cfg : SerialConfig) extends AutoBundleBidir with XilinxInterfaceType {
  /*  +--DTE--+ m     s +--DCE--+
   *  |     o >---tx----> i     |
   *  |     i <---rx----< o     |
   *  |     i <---cts---< o     |
   *  |     o >---rts---> i     |
   *  |     o >---dtr---> i     |
   *  |     i <---dsr---< o     |
   *  |     i <---dcd---< o     |
   *  |     i <---ri----< o     |
   *  +-------+         +-------+
   */
  // master is DTE, slave is DCE
  val tx  = ifGen(cfg.hasData) { Bool default Bool(!cfg.negData)}
  val rx  = ifGen(cfg.hasData) { Bool default Bool(!cfg.negData)}
  val cts = ifGen(cfg.hasHandshake) { Bool default Bool(cfg.negHandshake)}
  val rts = ifGen(cfg.hasHandshake) { Bool default Bool(cfg.negHandshake)}
  val dtr = ifGen(cfg.hasPresence) { Bool default Bool(cfg.negPresence)}
  val dsr = ifGen(cfg.hasPresence) { Bool default Bool(cfg.negPresence)}
  val dcd = ifGen(cfg.hasModem) { Bool default Bool(cfg.negCarrier)}
  val ri  = ifGen(cfg.hasModem) { Bool default Bool(cfg.negRing)}

  element("tx", false)
  element("rx", true)
  element("cts", true)
  element("rts", false)
  element("dtr", false)
  element("dsr", true)
  element("dcd", true)
  element("ri", true)

  if (cfg.hasData && cfg.negData) {
    tx.setPartialName("txn")
    rx.setPartialName("rxn")
  }
  if (cfg.hasHandshake && cfg.negHandshake) {
    cts.setPartialName("ctsn")
    rts.setPartialName("rtsn")
  }
  if (cfg.hasPresence && cfg.negPresence) {
    dtr.setPartialName("dtrn")
    dsr.setPartialName("dsrn")
  }
  if (cfg.hasModem && cfg.negCarrier) {
    dcd.setPartialName("dcdn")
  }
  if (cfg.hasModem && cfg.negRing) {
    ri.setPartialName("rin")
  }

  def defineXilinxInterface() : String = {
    val interface = getName()

    tx.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:uart:1.0 ${interface} TxD"))
    rx.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:uart:1.0 ${interface} RxD"))
    if (cfg.hasHandshake) {
      if (cfg.negHandshake) {
        rts.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:uart:1.0 ${interface} RTSn"))
        cts.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:uart:1.0 ${interface} CTSn"))
      } else {
        SpinalWarning(s"Serial non-inverted handshake signals not supported by xilinx interface, leaving ${cts} and ${rts} without attributes!")
      }
    }
    if (cfg.hasPresence) {
      if (cfg.negPresence) {
        dtr.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:uart:1.0 ${interface} DTRn"))
        dsr.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:uart:1.0 ${interface} DSRn"))
      } else {
        SpinalWarning(s"Serial non-inverted presence signals not supported by xilinx interface, leaving ${dtr} and ${dsr} without attributes!")
      }
    }
    if (cfg.hasModem) {
      if (cfg.negCarrier) {
        dcd.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:uart:1.0 ${interface} DCDn"))
      } else {
        SpinalWarning(s"Serial non-inverted carrier detect signal not supported by xilinx interface, leaving ${dcd} without attributes!")
      }
      if (cfg.negRing) {
        ri.addAttribute(new AttributeString("X_INTERFACE_INFO",
              s"xilinx.com:interface:uart:1.0 ${interface} RI"))
      } else {
        SpinalWarning(s"Serial non-inverted ring-indicator signal not supported by xilinx interface, leaving ${ri} without attributes!")
      }
    }

    interface
  }

  override def isInterface = true
}


case class TCharStreamPayload(charWidth : Int = 8, hasError : Boolean = false) extends AutoBundle {
  require(charWidth > 0)

  val char = Bits(charWidth bits)
  val eovr = ifGen(hasError) { Bool default False }
  val efrm = ifGen(hasError) { Bool default False }
  val epar = ifGen(hasError) { Bool default False }

  def setErr(ovr : Bool = False, frm : Bool = False, par : Bool = False) = if (hasError) {
    eovr := ovr
    efrm := frm
    epar := par
  }

  def getEOvr() = if (hasError) { eovr } else { False }
  def getEFrm() = if (hasError) { efrm } else { False }
  def getEPar() = if (hasError) { epar } else { False }
  def getEAny() = if (hasError) { eovr | efrm | epar } else { False }

  element("char")
  element("eovr")
  element("efrm")
  element("epar")
}


case class TCharStream(charWidth : Int = 8, hasError : Boolean = false) extends Channel(TCharStreamPayload(charWidth, hasError)) {
  def char = payload.char
  def eovr = payload.eovr
  def efrm = payload.efrm
  def epar = payload.epar

  def setErr(ovr : Bool = False, frm : Bool = False, par : Bool = False) = payload.setErr(ovr, frm, par)

  def getEOvr() = payload.getEOvr()
  def getEFrm() = payload.getEFrm()
  def getEPar() = payload.getEPar()
  def getEAny() = payload.getEAny()
}

