package hwlib.comp.streamer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.amba.{AxiConfig, Amba}


class AddrGen_Sequential(val cfg : AxiConfig) extends Component {
  val io = new Bundle {
    val iBase  =     in(cfg.TWAddr)
    val iCount =     in(cfg.TWAddr)
    val iUnit  =     in(cfg.TLen)

    val mGen   = master(AddrGenPort(cfg))
  }

  val sNextLenSpan  = UInt(cfg.wBound bits)
  val sNextLenCount = cfg.TWAddr
  val sNextBase     = cfg.TWAddr
  val sNextCount    = cfg.TWAddr
  val sNextLen      = cfg.TLen
  val sNextLast     = Bool

  val rBase    = Reg(cfg.TWAddr).init(U(0))
  val rCount   = Reg(cfg.TWAddr).init(U(0))
  val rUnit    = Reg(cfg.TLen).init(U(0))
  val rLen     = Reg(cfg.TLen).init(U(0))
  val rLast    = Reg(Bool).init(False)

  sNextLenCount := sNextCount - 1
  sNextLenSpan := ~sNextBase((cfg.wBound - 1) downto 0)
  when (sNextLenCount <= sNextLenSpan && sNextLenCount <= rUnit) {
    sNextLen := sNextLenCount.resize(widthOf(cfg.TLen))
    sNextLast := True
  } elsewhen (sNextLenSpan < rUnit) {
    sNextLen := sNextLenSpan.resize(widthOf(cfg.TLen))
    sNextLast := False
  } otherwise {
    sNextLen := rUnit
    sNextLast := False
  }
  sNextBase := rBase + rLen + 1
  sNextCount := rCount - rLen - 1

  when (io.mGen.reset) {
    rBase := io.iBase
    rCount := io.iCount
    rUnit := io.iUnit
  }

  val iFsm = new StateMachine {
    io.mGen.valid := False
    io.mGen.idle  := False

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        io.mGen.idle := True
        when (io.mGen.reset) {
          goto(Init)
        }
      }
    }

    val Init : State = new State {
      whenIsActive {
        when (!io.mGen.reset) {
          when (rCount === 0) {
            goto(Idle)
          } otherwise {
            rLen := sNextLen
            rLast := sNextLast
            goto(Active)
          }
        }
      }
    }

    val Active : State = new State {
      whenIsActive {
        io.mGen.valid := True
        when (io.mGen.reset) {
          goto(Init)
        } elsewhen (io.mGen.ready) {
          when (rLast) {
            goto(Idle)
          } otherwise {
            rBase := sNextBase
            rCount := sNextCount
            rLen := sNextLen
            rLast := sNextLast
          }
        }
      }
    }
  }

  io.mGen.addr := rBase
  io.mGen.len  := rLen
  io.mGen.last := rLast
}

