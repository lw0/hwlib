package hwlib.comp.streamer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.base.{MultiFifo, ChannelFanOut}
import hwlib.bundle.{AutoBundle, AutoBundleBidir}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiT, Amba, Channel, TAxiTPayload}



case class AddrGenPort(val cfg : AxiConfig) extends AutoBundleBidir {
  val addr   = cfg.TWAddr
  val len    = cfg.TLen
  val last   = Bool
  val valid  = Bool
  val ready  = Bool
  val idle   = Bool
  val reset  = Bool

  element("addr",   false)
  element("len",    false)
  element("last",   false)
  element("valid",  false)
  element("ready",  true)
  element("idle",   false)
  element("reset",  true)
}


case class AddrGenPack(val cfg : AxiConfig) extends AutoBundle {
  val addr   = cfg.TWAddr
  val len    = cfg.TLen

  element("addr")
  element("len")
}


case class AddrGenChannel(val cfg : AxiConfig) extends Channel(AddrGenPack(cfg)) {
  def addr   = payload.addr
  def len    = payload.len
}


