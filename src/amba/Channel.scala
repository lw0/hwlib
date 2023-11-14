package hwlib.amba

import spinal.core._
import spinal.lib.fsm.{StateMachine, State, EntryPoint}

import hwlib.bundle._



class Channel[T <: AutoBundle](tPayload : HardType[T]) extends AutoBundleBidir {
  val payload = tPayload()
  val valid = Bool()
  val ready = Bool()

  element("payload", false)
  element("valid",   false)
  element("ready",   true)

  def sendFsm() = new StateMachine {
    val sPayload = cloneOf(payload)
    valid := False
    payload.asRegInit()
    val Valid = new State with EntryPoint {
      whenIsNext {
        payload := sPayload
      }
      whenIsActive {
        valid := True
        when (ready) {
          exit()
        }
      }
    }
  }

  def receiveFsm() = new StateMachine {
    val rPayload = payload.cloneRegInit()
    ready := False
    val Ready = new State with EntryPoint {
      whenIsActive {
        ready := True
        when (valid) {
          rPayload := payload
          exit()
        }
      }
    }
  }

  def bufferToFsm(mTo : Channel[T]) = new StateMachine {
    val rBuffer = payload.cloneRegInit()
    mTo.payload.asRegInit()
    ready := False
    mTo.valid := False

    val Empty : State = new State with EntryPoint {
      whenIsActive {
        ready := True
        mTo.valid := False
        when (valid) {
          mTo.payload := payload
          goto(Normal)
        }
      }
    }
    val Normal : State = new State {
      whenIsActive {
        ready := True
        mTo.valid := True
        when (valid && mTo.ready) {
          mTo.payload := payload
        } elsewhen (valid && !mTo.ready) {
          rBuffer := payload
          goto(Full)
        } elsewhen (!valid && mTo.ready) {
          goto(Empty)
        }
      }
    }
    val Full : State = new State {
      whenIsActive {
        ready := False
        mTo.valid := True
        when (mTo.ready) {
          mTo.payload := rBuffer
          goto(Normal)
        }
      }
    }

  }
}

