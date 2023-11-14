package hwlib.amba

import spinal.core._

import hwlib.bundle._
import hwlib.xilinx.tools.XilinxInterfaceType



case class AxiStreamConfig(
  dataBytes      : Int,
  hasKeep        : Boolean = false,
  hasStrb        : Boolean = false,
  idBits         : Int = 0,
  destBits       : Int = 0,
  userGlobalBits : Int = 0,
  userByteBits   : Int = 0,
  defId          : UInt = null,
  defDest        : UInt = null,
  defUserGlobal  : Bits = null,
  defUserByte    : Bits = null) {
  require(idBits <= 32)
  require(destBits <= 32)

  // additional properties
  val hasId = (idBits > 0)
  val maxId = (1 << idBits) - 1

  val hasDest = (destBits > 0)
  val maxDest = (1 << destBits) - 1

  val hasUserGlobal = (userGlobalBits > 0)
  val hasUserByte = (userByteBits > 0)
  val userBits = userGlobalBits + userByteBits * dataBytes
  val hasUser = (userBits > 0)
  val userGlobalOffset = userByteBits * dataBytes

  // types
  def TData()       = Bits(dataBytes * 8 bits)
  def TMask()       = Bits(dataBytes bits)
  def TId()         = ifGen(hasId)         { UInt(idBits bits) }
  def TDest()       = ifGen(hasDest)       { UInt(destBits bits) }
  def TUser()       = ifGen(hasUser)       { Bits(userBits bits) }
  def TUserGlobal() = ifGen(hasUserGlobal) { Bits(userGlobalBits bits) }
  def TUserByte()   = ifGen(hasUserByte)   { Bits(userByteBits bits) }

  // constants
  def cMaskNone = TMask.getZero
  def cMaskAll = ~cMaskNone

  // defaults:
  def cIdDefault   = ifGen(hasId)   { (if (defId   != null)  defId    else U(0)).resize(widthOf(TId())) }
  def cDestDefault = ifGen(hasDest) { (if (defDest != null)  defDest  else U(0)).resize(widthOf(TDest())) }
  def cUserGlobalDefault = ifGen(hasUserGlobal) { (if (defUserGlobal != null)  defUserGlobal  else B(0)).resize(widthOf(TUserGlobal())) }
  def cUserByteDefault = ifGen(hasUserByte) { (if (defUserByte != null)  defUserByte  else B(0)).resize(widthOf(TUserByte())) }
  def cUserDefault = if (hasUserByte) {
      val cUserBytesDefault = B((0 until dataBytes).map((idx) => cUserByteDefault))
      if (hasUserGlobal) { cUserGlobalDefault ## cUserBytesDefault } else { cUserBytesDefault }
    } else {
      if (hasUserGlobal) { cUserGlobalDefault } else { null }
    }
}


case class TAxiTPayload(val cfg : AxiStreamConfig) extends AutoBundle {
  val data = cfg.TData()
  val keep = ifGen(cfg.hasKeep) { cfg.TMask() }
  val strb = ifGen(cfg.hasStrb) { cfg.TMask() }
  val id   = ifGen(cfg.hasId)   { cfg.TId() }
  val dest = ifGen(cfg.hasDest) { cfg.TDest() }
  val user = ifGen(cfg.hasUser) { cfg.TUser() }
  val last = Bool()

  element("data")
  element("keep", WarnDefault(cfg.cMaskAll), WarnDrop)
  element("strb", WarnDefault(if (cfg.hasKeep) keep else cfg.cMaskAll), WarnDrop)
  element("id",   WarnDefault(cfg.cIdDefault), WarnDrop)
  element("dest", WarnDefault(cfg.cDestDefault), WarnDrop)
  element("user", WarnDefault(cfg.cUserDefault), WarnDrop)
  element("last")

  def userGlobal = ifGen(cfg.hasUserGlobal) { user(cfg.userGlobalOffset, cfg.userGlobalBits bits) }
  def userByte(idx : Int) = ifGen(cfg.hasUserByte && (idx < cfg.dataBytes)) { user(cfg.userByteBits * idx, cfg.userByteBits bits) }
}

case class TAxiT(val cfg : AxiStreamConfig) extends Channel(TAxiTPayload(cfg)) {
  def data  = payload.data
  def keep  = payload.keep
  def strb  = payload.strb
  def id    = payload.id
  def dest  = payload.dest
  def user  = payload.user
  def last  = payload.last

  def userGlobal = payload.userGlobal
  def userByte(idx : Int) = payload.userByte(idx)
}

case class TAxiStream(val cfg : AxiStreamConfig) extends AutoBundleBidir with XilinxInterfaceType {
  val t = TAxiT(cfg)

  element("t",  false, UsePrefix)

  def data  = t.data
  def keep  = t.keep
  def strb  = t.strb
  def id    = t.id
  def dest  = t.dest
  def user  = t.user
  def last  = t.last
  def valid = t.valid
  def ready = t.ready

  def userGlobal = t.userGlobal
  def userByte(idx : Int) = t.userByte(idx)


  def defineXilinxInterface() : String = {
    val interface = getName()

    t.data.addAttribute(new AttributeString("X_INTERFACE_PARAMETER",
          s"TDATA_NUM_BYTES ${cfg.dataBytes}"))
    t.data.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:aximm:1.0 ${interface} TDATA"))
    if (cfg.hasKeep) {
      t.keep.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} TKEEP"))
    }
    if (cfg.hasStrb) {
      t.strb.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} TSTRB"))
    }
    if (cfg.hasId) {
      t.id.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} TID"))
    }
    if (cfg.hasDest) {
      t.dest.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} TDEST"))
    }
    if (cfg.hasUser) {
      t.user.addAttribute(new AttributeString("X_INTERFACE_INFO",
            s"xilinx.com:interface:aximm:1.0 ${interface} TUSER"))
    }
    t.last.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:aximm:1.0 ${interface} TLAST"))
    t.valid.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:aximm:1.0 ${interface} TVALID"))
    t.ready.addAttribute(new AttributeString("X_INTERFACE_INFO",
          s"xilinx.com:interface:aximm:1.0 ${interface} TREADY"))

    interface
  }

  override def isInterface = true
  override def associateClock = true
}

