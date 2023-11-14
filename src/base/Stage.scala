package hwlib.base

import spinal.core._
import spinal.lib.fsm._



class Stage[T <: Data](tPayload : HardType[T]) extends Component {

  val io = new Bundle {
    val iPutData  =  in(tPayload())
    val iPutValid =  in(Bool)
    val oPutReady = out(Bool)

    val oGetData  = out(tPayload())
    val oGetValid = out(Bool)
    val iGetReady =  in(Bool)
  }

  val rData = Reg(tPayload())
  val rOver = Reg(tPayload())

  io.oGetData := rData

  io.oPutReady := False
  io.oGetValid := False
  val iFsm = new StateMachine {

    val Empty : State = new State with EntryPoint {
      whenIsActive {
        io.oPutReady := True
        when (io.iPutValid) {
          rData := io.iPutData
          goto(Full)
        }
      }
    }

    val Full : State = new State {
      whenIsActive {
        io.oPutReady := True
        io.oGetValid := True
        when (io.iPutValid && io.iGetReady) {
          rData := io.iPutData
        } elsewhen (io.iPutValid) {
          rOver := io.iPutData
          goto(Over)
        } elsewhen (io.iGetReady) {
          goto(Empty)
        }
      }
    }

    val Over : State = new State {
      whenIsActive {
        io.oGetValid := True
        when (io.iGetReady) {
          rData := rOver
          goto(Full)
        }
      }
    }

  }
}

