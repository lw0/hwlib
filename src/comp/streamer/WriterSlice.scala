package hwlib.comp.streamer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.base.{Fifo, ChannelFanOut}
import hwlib.amba.{AxiConfig, Amba}



class WriterSlice(val cfg : AxiConfig, val logCap : Int, logQueue : Int = 5) extends Component {
  val cap = 1 << logCap
  val cCredMax   = U(cap, widthOf(cfg.TWAddr) bits)

  val io = new Bundle {
    val iStart     =     in(Bool)
    val oOver      =    out(Bool)
    val oError     =    out(Bool)
    val oIdle      =    out(Bool)

    val sGen       =  slave(AddrGenPort(cfg))
    val mGen       = master(AddrGenChannel(cfg))

    val iStmData   =     in(cfg.TData)
    val iStmStrb   =     in(cfg.TMask)
    val iStmLast   =     in(Bool)
    val iStmValid  =     in(Bool)
    val oStmReady  =    out(Bool)

    val oWChData   =    out(cfg.TData)
    val oWChStrb   =    out(cfg.TMask)
    val oWChLast   =    out(Bool)
    val oWChValid  =    out(Bool)
    val iWChReady  =     in(Bool)

    val iResp      =     in(cfg.TResp)
    val iRespAct   =     in(Bool)
  }

  val sInit        = Bool
  val rError       = Reg(Bool).init(False)

  val rPending     = Reg(cfg.TWAddr).init(U(0))
  val sNonePending = Bool

  val rCredits     = Reg(cfg.TWAddr).init(U(0))
  val sCredLen     = cfg.TLen
  val sCredSub     = Bool

  val rAddr        = Reg(cfg.TWAddr).init(U(0))
  val rAddrLen     = Reg(cfg.TLen).init(U(0))
  val rAddrLast    = Reg(Bool).init(False)
  val sAddrValid   = Bool
  val sAddrReady   = Bool

  val rDataActive  = Reg(Bool).init(False)
  val rDataLen     = Reg(cfg.TLen).init(U(0))
  val sDataStart   = Bool
  val sDataPut     = Bool


  /////////////////////////////////////////////////////////////////////////////
  // Status Registers
  when (sInit) {
    rError := False
  } elsewhen (io.iResp(1) && io.iRespAct) { // resp=1- means SlvErr or DecErr
    rError := True
  }
  io.oError := rError

  when(sInit) {
    rPending := 0
  } elsewhen(io.mGen.valid && io.mGen.ready && !io.iRespAct) {
    rPending := rPending + 1
  } elsewhen(!(io.mGen.valid && io.mGen.ready) && io.iRespAct) {
    rPending := rPending - 1
  }
  sNonePending := rPending === 0

  when (sCredSub && sDataPut) {
    rCredits := rCredits - sCredLen
  } elsewhen(sCredSub && !sDataPut) {
    rCredits := rCredits - sCredLen - 1
  } elsewhen(!sCredSub && sDataPut) {
    rCredits := rCredits + 1
  }

  /////////////////////////////////////////////////////////////////////////////
  // Address Generation
  val iAddrFsm = new StateMachine {
    sAddrValid    := False
    io.sGen.ready := False
    io.sGen.reset := False
    io.oIdle      := False
    io.oOver      := False
    sInit         := False
    sDataStart    := False
    sCredLen      := U(0)
    sCredSub      := False

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        io.oIdle      := True
        io.oOver      := False
        io.sGen.reset := io.iStart
        sInit         := io.iStart
        sDataStart    := io.iStart
        when(io.iStart) {
          goto(Check)
        }
      }
    }

    val Over : State = new State {
      whenIsActive {
        io.oIdle      := True
        io.oOver      := True
        io.sGen.reset := io.iStart
        sInit         := io.iStart
        sDataStart    := False
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
          } elsewhen (!rDataActive) {
            when (rCredits > 0) {
              io.sGen.ready := True
              rAddr := io.sGen.addr
              rAddrLen := (rCredits - 1).resize(widthOf(rAddrLen))
              rAddrLast := True
              sCredLen := (rCredits - 1).resize(widthOf(rAddrLen))
              sCredSub := True
              goto(Valid)
            } otherwise {
              goto(Stop)
            }
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
          when (rCredits === 0) {
            goto(Idle)
          } otherwise {
            goto(Over)
          }
        }
      }
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Address Info Queue and Barrier
  val iBarrier = new ChannelFanOut(2)
  iBarrier.io.iValid := sAddrValid
  sAddrReady := iBarrier.io.oReady

  val iQueue = new Fifo(cfg.TLen, logQueue, false)
  iQueue.io.iPutData := rAddrLen
  iQueue.io.iPutValid := iBarrier.io.oValids(0)
  iBarrier.io.iReadys(0) := iQueue.io.oPutReady

  io.mGen.addr := rAddr
  io.mGen.len  := rAddrLen
  io.mGen.valid := iBarrier.io.oValids(1)
  iBarrier.io.iReadys(1) := io.mGen.ready


  /////////////////////////////////////////////////////////////////////////////
  // Data Buffer Ingress
  case class BufferPack() extends Bundle {
    val data = cfg.TData
    val strb = cfg.TMask
  }
  val iBuffer = new Fifo(BufferPack(), logCap, true)

  when (sDataStart) {
    rDataActive := True
  } elsewhen (io.iStmLast && io.iStmValid && iBuffer.io.oPutReady) {
    rDataActive := False
  }
  iBuffer.io.iPutData.data := io.iStmData
  iBuffer.io.iPutData.strb := io.iStmStrb
  when (rDataActive) {
    iBuffer.io.iPutValid := io.iStmValid
    io.oStmReady := iBuffer.io.oPutReady
    sDataPut := io.iStmValid && iBuffer.io.oPutReady
  } otherwise {
    iBuffer.io.iPutValid := False
    io.oStmReady := False
    sDataPut := False
  }

  /////////////////////////////////////////////////////////////////////////////
  // Data Buffer Egress
  io.oWChData := iBuffer.io.oGetData.data
  io.oWChStrb := iBuffer.io.oGetData.strb
  io.oWChLast := (rDataLen === 0)
  val iDataFsm = new StateMachine {
    io.oWChValid := False
    iBuffer.io.iGetReady := False
    iQueue.io.iGetReady := False

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        iQueue.io.iGetReady := True
        when (iQueue.io.oGetValid) {
          rDataLen := iQueue.io.oGetData
          goto(Pass)
        }
      }
    }

    val Pass : State = new State {
      whenIsActive {
        io.oWChValid := iBuffer.io.oGetValid
        iBuffer.io.iGetReady := io.iWChReady
        when (iBuffer.io.oGetValid && io.iWChReady) {
          rDataLen := rDataLen - 1
          when (rDataLen === 0) {
            iQueue.io.iGetReady := True
            when (iQueue.io.oGetValid) {
              rDataLen := iQueue.io.oGetData
            } otherwise {
              goto(Idle)
            }
          }
        }
      }
    }
  }

}


