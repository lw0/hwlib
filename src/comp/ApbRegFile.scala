package hwlib.comp

import spinal.core._
import spinal.lib._

import hwlib.amba.{ApbConfig, TApb}



class ApbRegFile(val cfg : ApbConfig) extends Component {

  val RegCount = 1 << widthOf(cfg.TAddr)

  val io = new Bundle {
    val sApb    =   slave(TApb(cfg))

    val iReg    = Vec( in(cfg.TData), RegCount)
    val oReg    = Vec(out(cfg.TData), RegCount)
    val oRegWr  = Vec(out(Bool), RegCount)
    val oRegRd  = Vec(out(Bool), RegCount)
  }

  def byteMux(sMask : Bits, sData0 : Bits, sData1 : Bits) = new Composite(sMask, "byteMux") {
    val sData = cfg.TData
    for (idx <- 0 until widthOf(sMask)) {
      sData(8 * idx, 8 bits) := Mux(sMask(idx), sData1(8 * idx , 8 bits), sData0(8 * idx, 8 bits))
    }
  }.sData

  io.sApb.rdata := B(0)
  io.sApb.slverr := False
  io.sApb.ready := True

  for (idx <- 0 until RegCount) {
    io.oReg(idx).setAsReg().init(B(0))
    io.oRegWr(idx).setAsReg().init(False)
    io.oRegRd(idx).setAsReg().init(False)
    io.oRegWr(idx) := False
    io.oRegRd(idx) := False
    when (io.sApb.sel && io.sApb.enable && io.sApb.addr === idx) {
      when (io.sApb.write) {
        if (cfg.hasStrb) {
          io.oRegWr(idx) := io.sApb.strb.orR
          io.oReg(idx) := byteMux(io.sApb.strb, io.oReg(idx), io.sApb.wdata)
        } else {
          io.oRegWr(idx) := True
          io.oReg(idx) := io.sApb.wdata
        }
      } otherwise {
        io.oRegRd(idx) := True
        io.sApb.rdata := io.iReg(idx)
      }
    }
  }
}

