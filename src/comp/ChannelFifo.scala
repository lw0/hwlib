package hwlib.comp

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import hwlib.base.Fifo
import hwlib.bundle.AutoBundle
import hwlib.amba.Channel



class ChannelFifo[T <: AutoBundle](tPayload : HardType[T], logDepth : Int, syncRead : Boolean = true) extends Component {
  val io = new Bundle {
    val sPut   =  slave(new Channel(tPayload()))
    val mGet   = master(new Channel(tPayload()))

    val oCount =    out(UInt((logDepth + 1) bits))
    val oSpace =    out(UInt((logDepth + 1) bits))
    val oFull  =    out(Bool)
    val oEmpty =    out(Bool)
    val oPut   =    out(Bool)
    val oGet   =    out(Bool)
  }

  val iFifo = new Fifo(tPayload(), logDepth, syncRead)
  iFifo.io.iPutData  << io.sPut.payload
  iFifo.io.iPutValid := io.sPut.valid
  io.sPut.ready      := iFifo.io.oPutReady

  io.mGet.payload    << iFifo.io.oGetData
  io.mGet.valid      := iFifo.io.oGetValid
  iFifo.io.iGetReady := io.mGet.ready

  io.oCount := iFifo.io.oCount
  io.oSpace := iFifo.io.oSpace
  io.oFull  := iFifo.io.oFull
  io.oEmpty := iFifo.io.oEmpty
  io.oPut   := io.sPut.valid && iFifo.io.oPutReady
  io.oGet   := iFifo.io.oGetValid && io.mGet.ready
}
