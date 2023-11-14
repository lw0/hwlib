package hwlib.comp.streamer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.base.{Fifo, ChannelFanOut}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, Amba, Channel, TAxiTPayload}



class ReaderSlice(val cfg : AxiConfig, val logCap : Int, val logQueue : Int = 5) extends Component {
  val cap = 1 << logCap
  val cCredMax  = U(cap, widthOf(cfg.TWAddr) bits)

  val io = new Bundle {
    val iStart    =     in(Bool)
    val oError    =    out(Bool)
    val oIdle     =    out(Bool)

    val sGen      =  slave(AddrGenPort(cfg))

    val mGen      = master(AddrGenChannel(cfg))

    val iRChData  =     in(cfg.TData)
    val iRChLast  =     in(Bool)
    val iRChResp  =     in(cfg.TResp)
    val iRChValid =     in(Bool)
    val oRChReady =    out(Bool)

    val oStmData  =    out(cfg.TData)
    val oStmStrb  =    out(cfg.TMask)
    val oStmLast  =    out(Bool)
    val oStmValid =    out(Bool)
    val iStmReady =     in(Bool)
  }

  val sInit = Bool
  val rError   = Reg(Bool).init(False)

  val rPending = Reg(cfg.TWAddr).init(U(0))
  val sNonePending = Bool

  val rCredits = Reg(cfg.TWAddr).init(cCredMax)
  val sCredLen = cfg.TLen
  val sCredSub = Bool

  val rAddr    = Reg(cfg.TWAddr).init(U(0))
  val rAddrLen     = Reg(cfg.TLen).init(U(0))
  val rAddrLast    = Reg(Bool).init(False)
  val sAddrValid = Bool
  val sAddrReady = Bool

  val rDataLast = Reg(Bool).init(False)
  val sDataGet = Bool

  val sResp = cfg.TResp
  val sRespAct = Bool
  val sRespDone = Bool

  when (sInit) {
    rError := False
  } elsewhen (sResp(1) && sRespAct) { // resp=1- means SlvErr or DecErr
    rError := True
  }
  io.oError := rError

  when(sInit) {
    rPending := 0
  } elsewhen((io.mGen.valid && io.mGen.ready) && !sRespDone) {
    rPending := rPending + 1
  } elsewhen(!(io.mGen.valid && io.mGen.ready) && sRespDone) {
    rPending := rPending - 1
  }
  sNonePending := rPending === 0

  when(sCredSub && sDataGet) {
    rCredits := rCredits - sCredLen
  } elsewhen(sCredSub && !sDataGet) {
    rCredits := rCredits - sCredLen - 1
  } elsewhen(!sCredSub && sDataGet) {
    rCredits := rCredits + 1
  }

  /////////////////////////////////////////////////////////////////////////////
  // Address Generation
  val iAddrFsm = new StateMachine {
    sAddrValid := False
    io.sGen.ready := False
    io.sGen.reset := False
    io.oIdle      := False
    sInit := False
    sCredLen := U(0)
    sCredSub := False

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        io.sGen.reset := io.iStart
        io.oIdle      := True
        sInit         := io.iStart
        when(io.iStart) {
          goto(Check)
        }
      }
    }

    val Check : State = new State {
      whenIsActive {
        when (io.sGen.idle) {
          goto(Idle)
        } otherwise {
          goto(Fetch)
        }
      }
    }

    val Fetch : State = new State {
      whenIsActive {
        when (io.sGen.valid) {
          when (rCredits > io.sGen.len) {
            io.sGen.ready := True
            rAddr := io.sGen.addr
            rAddrLen := io.sGen.len
            rAddrLast := io.sGen.last
            sCredLen := io.sGen.len
            sCredSub := True
            goto(Valid)
          }
        }
      }
    }

    val Valid : State = new State {
      whenIsActive {
        sAddrValid := True
        when (sAddrReady) {
          when (rAddrLast) {
            goto(Stop)
          } otherwise {
            goto(Fetch)
          }
        }
      }
    }

    val Stop : State = new State {
      whenIsActive {
        when (sNonePending) {
          goto(Idle)
        }
      }
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Address Info Output and Queue Ingress
  val iBarrier = new ChannelFanOut(2)
  iBarrier.io.iValid := sAddrValid
  sAddrReady := iBarrier.io.oReady

  val iQueue = new Fifo(Bool, logQueue, false)
  iQueue.io.iPutData := rAddrLast
  iQueue.io.iPutValid := iBarrier.io.oValids(0)
  iBarrier.io.iReadys(0) := iQueue.io.oPutReady

  io.mGen.addr := rAddr
  io.mGen.len := rAddrLen
  io.mGen.valid := iBarrier.io.oValids(1)
  iBarrier.io.iReadys(1) := io.mGen.ready


  /////////////////////////////////////////////////////////////////////////////
  // Data Buffer Ingress
  case class BufferPack() extends Bundle {
    val data = cfg.TData
    val last = Bool
  }
  val iBuffer = new Fifo(BufferPack(), logCap, true)


  iBuffer.io.iPutData.data := io.iRChData
  iBuffer.io.iPutData.last := io.iRChLast && rDataLast
  val iDataFsm = new StateMachine {
    iBuffer.io.iPutValid := False
    io.oRChReady := False
    iQueue.io.iGetReady := False

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        iQueue.io.iGetReady := True
        when (iQueue.io.oGetValid) {
          rDataLast := iQueue.io.oGetData
          goto(Pass)
        }
      }
    }

    val Pass : State = new State {
      whenIsActive {
        iBuffer.io.iPutValid := io.iRChValid
        io.oRChReady := iBuffer.io.oPutReady
        when (io.iRChLast && io.iRChValid && iBuffer.io.oPutReady) {
          iQueue.io.iGetReady := True
          when (iQueue.io.oGetValid) {
            rDataLast := iQueue.io.oGetData
          } otherwise {
            goto(Idle)
          }
        }
      }
    }
  }

  sResp := io.iRChResp
  sRespAct := io.iRChValid && io.oRChReady
  sRespDone := io.iRChLast && io.iRChValid && io.oRChReady

  /////////////////////////////////////////////////////////////////////////////
  // Data Buffer Egress
  io.oStmData := iBuffer.io.oGetData.data
  io.oStmStrb := cfg.cMaskAll
  io.oStmLast := iBuffer.io.oGetData.last
  io.oStmValid := iBuffer.io.oGetValid
  iBuffer.io.iGetReady := io.iStmReady
  sDataGet := iBuffer.io.oGetValid && io.iStmReady
}


