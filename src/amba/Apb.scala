package hwlib.amba

import spinal.core._

import hwlib.bundle._



case class ApbConfig(
  addrBits  : Int,
  dataBytes : Int,
  hasStrb   : Boolean = true,
  hasProt   : Boolean = false,
  hasErr    : Boolean = true,
  defProt   : UInt = null) {
  require(addrBits <= 32)
  require(Seq(1, 2, 4).contains(dataBytes))

  // additional properties
  val fullSize = log2Up(dataBytes)

  // types
  def TAddr   = UInt(addrBits bits)
  def TData   = Bits(dataBytes * 8 bits)
  def TMask   = Bits(dataBytes bits)

  // constants
  def cMaskNone = TMask.getZero
  def cMaskAll = ~cMaskNone

  // defaults:
  def cProtDefault = ifGen(hasProt) { (if (defProt != null) defProt else Amba.cProtNone).resize(widthOf(Amba.TProt) bits) }
}
object ApbConfig {
  def fromAxi(axiCfg : AxiConfig) = ApbConfig(
      addrBits = axiCfg.waddrBits,
      dataBytes = axiCfg.dataBytes,
      hasStrb = axiCfg.hasStrb,
      hasProt = axiCfg.hasProt,
      hasErr = true)
}


case class TApbMPayload(val cfg : ApbConfig) extends AutoBundle {
  val addr   = cfg.TAddr
  val prot   = ifGen(cfg.hasProt) { Amba.TProt }
  val write  = Bool()
  val wdata  = cfg.TData
  val strb   = ifGen(cfg.hasStrb) { cfg.TMask }

  element("addr",   MayUpsize, WarnDownsize)
  element("prot",   UseDefault(cfg.cProtDefault), MayDrop)
  element("write")
  element("wdata")
  element("strb",   WarnDefault(cfg.cMaskAll), WarnDrop)
}


case class TApbSPayload(val cfg : ApbConfig) extends AutoBundle {
  val rdata  = cfg.TData
  val slverr = ifGen(cfg.hasErr) { Bool() }

  element("rdata")
  element("slverr", WarnDefault(False), WarnDrop)
}


case class TApb(val cfg : ApbConfig) extends AutoBundleBidir {
  val mpayload = TApbMPayload(cfg)
  val spayload = TApbSPayload(cfg)
  val sel      = Bool()
  val enable   = Bool()
  val ready    = Bool()

  def addr   = mpayload.addr
  def prot   = mpayload.prot
  def write  = mpayload.write
  def wdata  = mpayload.wdata
  def strb   = mpayload.strb
  def rdata  = spayload.rdata
  def slverr = spayload.slverr

  element("mpayload", false, UsePrefix("p"))
  element("spayload", true,  UsePrefix("p"))
  element("sel",      false, UsePrefix("p"))
  element("enable",   false, UsePrefix("p"))
  element("ready",    true,  UsePrefix("p"))
}


case class TApbMux(val cfg : ApbConfig, val selCount : Int) extends AutoBundleBidir {
  val mpayload = TApbMPayload(cfg)
  val spayload = TApbSPayload(cfg)
  val sel      = Bits(selCount bits)
  val enable   = Bool()
  val ready    = Bool()

  def addr   = mpayload.addr
  def prot   = mpayload.prot
  def write  = mpayload.write
  def wdata  = mpayload.wdata
  def strb   = mpayload.strb
  def rdata  = spayload.rdata
  def slverr = spayload.slverr

  element("mpayload", false, UsePrefix("p"))
  element("spayload", true,  UsePrefix("p"))
  element("sel",      false, UsePrefix("p"))
  element("enable",   false, UsePrefix("p"))
  element("ready",    true,  UsePrefix("p"))

}

