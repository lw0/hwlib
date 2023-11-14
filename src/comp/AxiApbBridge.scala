package hwlib.comp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.amba.{AxiConfig, TAxi, ApbConfig, TApb, TApbMux, Amba}



class AxiApbBridge(val cfgAxi : AxiConfig, val regions : (Long, Long)*) extends Component {
  require(cfgAxi.isLite)

  val cfg = ApbConfig.fromAxi(cfgAxi)
  val selCount = regions.length

  val io = new Bundle {
    val sAxi  =      slave(TAxi(cfgAxi))
    val mApbs = Vec(master(TApb(cfg)), selCount)
  }


  def AddrDecoder(sAxiWAddr : UInt) = new Area {
    val sMatch  = Bits(selCount bits)
    val sSelect = Bits(selCount bits)
    val sRelAddr = cloneOf(sAxiWAddr)

    sSelect := OHMasking.last(sMatch)
    sRelAddr := U(0)
    for (idx <- 0 until selCount) {
      val sOffset = U(regions(idx)._1)
      val sLength = U(regions(idx)._2)
      val sRelative = sAxiWAddr - sOffset
      sMatch(idx) := sAxiWAddr >= sOffset && sRelative < sLength
      when (sSelect(idx)) {
        sRelAddr := sRelative
      }
    }
  }

  val iWrDec  = AddrDecoder(io.sAxi.aw.waddr)
  val iRdDec  = AddrDecoder(io.sAxi.ar.waddr)

  val rAddr   = Reg(cfg.TAddr)
  val rSelect = Reg(Bits(selCount bits))
  val rId     = ifGen(cfgAxi.hasId) { Reg(cfgAxi.TId) }
  val rProt   = ifGen(cfgAxi.hasProt) { Reg(cfgAxi.TProt) }
  val rWrite  = Reg(Bool)
  val rWrData = Reg(cfg.TData)
  val rWrMask = ifGen(cfg.hasStrb) { Reg(cfg.TMask) }

  val rTimeout = Counter(1024) // Use a fixed timeout to cancel Enable state

  val rWrResp = Reg(cfgAxi.TResp)
  val rRdResp = Reg(cfgAxi.TResp)
  val rRdData = Reg(cfg.TData)

  val sApbMux = TApbMux(cfg, selCount)

  val iFsm = new StateMachine {
    io.sAxi.aw.ready := False
    io.sAxi.ar.ready := False
    io.sAxi.w.ready := False
    io.sAxi.b.valid := False
    io.sAxi.r.valid := False
    sApbMux.sel := B(0)
    sApbMux.enable := False

    val Init : State = new State with EntryPoint {
      whenIsActive {
        when (io.sAxi.ar.valid) {
          goto(WaitRd)
        } otherwise {
          goto(WaitWr)
        }
      }
    }

    val WaitWr : State = new State {
      whenIsActive {
        io.sAxi.aw.ready := True
        when (io.sAxi.aw.valid) {
          if (cfgAxi.hasId) {
            rId := io.sAxi.aw.id
          }
          if (cfgAxi.hasProt) {
            rProt := io.sAxi.aw.prot
          }
          rAddr := iWrDec.sRelAddr
          rSelect := iWrDec.sSelect
          rWrite := True
          goto(DataWr)
        } elsewhen (io.sAxi.ar.valid) {
          goto(WaitRd)
        }
      }
    }

    val WaitRd : State = new State {
      whenIsActive {
        io.sAxi.ar.ready := True
        when (io.sAxi.ar.valid) {
          if (cfgAxi.hasId) {
            rId := io.sAxi.ar.id
          }
          if (cfgAxi.hasProt) {
            rProt := io.sAxi.ar.prot
          }
          rAddr := iRdDec.sRelAddr
          rSelect := iRdDec.sSelect
          rWrite := False
          when (iRdDec.sSelect.orR) {
            goto(Select)
          } otherwise {
            rRdResp := cfgAxi.cRespDecErr
            goto(RespRd)
          }
        } elsewhen (io.sAxi.aw.valid) {
          goto(WaitWr)
        }
      }
    }

    val DataWr : State = new State {
      whenIsActive {
        io.sAxi.w.ready := True
        when (io.sAxi.w.valid) {
          rWrData := io.sAxi.w.data
          if (cfg.hasStrb) {
            rWrMask := io.sAxi.w.strb
          }
          when (rSelect.orR) {
            goto(Select)
          } otherwise {
            rWrResp := cfgAxi.cRespDecErr
            goto(RespWr)
          }
        }
      }
    }

    val Select : State = new State {
      whenIsActive {
        sApbMux.sel := rSelect
        rTimeout.clear()
        goto(Enable)
      }
    }

    val Enable : State = new State {
      whenIsActive {
        rTimeout.increment()
        sApbMux.sel := rSelect
        sApbMux.enable := True
        when (sApbMux.ready) {
          when (rWrite) {
            rWrResp := Mux(sApbMux.slverr, cfgAxi.cRespSlvErr, cfgAxi.cRespOkay)
            goto(RespWr)
          } otherwise {
            rRdData := sApbMux.rdata
            rRdResp := Mux(sApbMux.slverr, cfgAxi.cRespSlvErr, cfgAxi.cRespOkay)
            goto(RespRd)
          }
        } elsewhen (rTimeout.willOverflow) {
          when (rWrite) {
            rWrResp := cfgAxi.cRespSlvErr
            goto(RespWr)
          } otherwise {
            rRdResp := cfgAxi.cRespSlvErr
            goto(RespRd)
          }
        }
      }
    }

    val RespWr : State = new State {
      whenIsActive {
        io.sAxi.b.valid := True
        when (io.sAxi.b.ready) {
          when (io.sAxi.aw.valid && !io.sAxi.ar.valid) {
            goto(WaitWr)
          } otherwise {
            goto(WaitRd)
          }
        }
      }
    }

    val RespRd : State = new State {
      whenIsActive {
        io.sAxi.r.valid := True
        when (io.sAxi.r.ready) {
          when (io.sAxi.ar.valid && !io.sAxi.aw.valid) {
            goto(WaitRd)
          } otherwise {
            goto(WaitWr)
          }
        }
      }
    }

  }

  io.sAxi.b.payload.assign(
      "resp"  -> rWrResp,
      "id"    -> rId)

  io.sAxi.r.payload.assign(
      "resp"  -> rRdResp,
      "data"  -> rRdData,
      "id"    -> rId)

  sApbMux.mpayload.assign(
      "addr"  -> rAddr,
      "prot"  -> rProt,
      "write" -> rWrite,
      "wdata" -> rWrData,
      "strb"  -> rWrMask)


  sApbMux.spayload.assign()
  sApbMux.ready := False
  for (idx <- 0 until selCount) {
    io.mApbs(idx).mpayload.connect(sApbMux.mpayload)
    io.mApbs(idx).sel := sApbMux.sel(idx)
    io.mApbs(idx).enable := sApbMux.enable
    when (sApbMux.sel(idx)) {
      sApbMux.spayload.connect(io.mApbs(idx).spayload)
      sApbMux.ready := io.mApbs(idx).ready
    }
  }

}


