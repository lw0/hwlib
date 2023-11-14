package hwlib.serial

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._



class Receiver(val frameBits : Int = 9, val fracBits : Int = 4) extends Component {
  require(frameBits > 0)
  require(fracBits >= 2)

  val countBits = log2Up(frameBits)

  def MaxDataBits = U(frameBits - 1, countBits bits)
  def FracHalf = U(1 << fracBits - 1, fracBits bits)
  def FracFull = U((1 << fracBits) - 1, fracBits bits)


  val io = new Bundle {
    val iEnable   =  in(Bool) default True
    val iBaudTick =  in(Bool)

    val iLine     =  in(Bool)
    val oAccept   = out(Bool)

    val iDataBits =  in(UInt(countBits bits)) default MaxDataBits

    val oData     = out(Bits(frameBits bits))
    val oFrame    = out(Bool)
    val oOver     = out(Bool)
    val oValid    = out(Bool)
    val iReady    =  in(Bool) default True

    val oBreak    = out(Bool)
  }


  val rFrac = Reg(UInt(fracBits bits)) init 0
  val rCount = Reg(UInt(countBits bits)) init 0

  val rSampleA = Reg(Bool) init False
  val rSampleB = Reg(Bool) init False
  val sSample = MajorityVote(io.iLine ## rSampleA ## rSampleB)

  val rShift = Reg(Bits(frameBits bits)) init 0
  val sStrobe = Bool
  val sActive = Bool



  when (rFrac === 1 && io.iBaudTick) {
    rSampleA := io.iLine
  }
  when (rFrac === 2 && io.iBaudTick) {
    rSampleB := io.iLine
  }

  val iSampler = new StateMachine {
    io.oBreak  := False
    sStrobe  := False
    sActive := False

    val Disabled : State = new State with EntryPoint {
      whenIsActive {
        rOver  := False
        when (io.iEnable && io.iLine) {
          goto(Idle)
        }
      }
    }

    val Idle : State = new State {
      whenIsActive {
        when (!io.iEnable) {
          goto(Disabled)
        } elsewhen (io.iBaudTick && !io.iLine) {
          rFrac := FracHalf
          goto(Start)
        }
      }
    }

    val Start : State = new State {
      whenIsActive {
        sActive := True
        when (io.iBaudTick) {
          when (rFrac === 0) {
            when (sSample) {
              goto(Idle) // Spurious Start
            } elsewhen (io.iDataBits > MaxDataBits) {
              goto(Idle) // Invalid Data Bit Count
            } otherwise {
              rFrac  := FracFull
              rShift := 0
              rCount := io.iDataBits
              goto(Data)
            }
          } otherwise {
            rFrac := rFrac - 1
          }
        }
      }
    }

    val Data : State = new State {
      whenIsActive {
        sActive := True
        when (io.iBaudTick) {
          when (rFrac === 0) {
            rFrac := FracFull
            rShift := sSample ## rShift(frameBits-1 downto 1)
            when (rCount === 0) {
              goto(Stop)
            } otherwise {
              rCount := rCount - 1
            }
          } otherwise {
            rFrac := rFrac - 1
          }
        }
      }
    }

    val Stop : State = new State {
      whenIsActive {
        sActive := True
        when (io.iBaudTick) {
          when (rFrac === 0) {
            rFrac := FracFull
            when (!sSample && rShift === 0) {
              goto(Break)
            } elsewhen (sSample) {
              sStrobe := True
              goto(Idle)
            } otherwise {
              // use rCount to indicate Framing Error
              rCount := 1
            }
          } otherwise {
            rFrac := rFrac - 1
          }
        }
      }
    }

    val Break : State = new State {
      whenIsActive {
        io.oBreak := True
        when (io.iBaudTick) {
          when (rFrac === 0) {
            rFrac := FracFull
            when (sSample) {
              goto(Idle) // Break is not data, so no strobe generated
            }
          } otherwise {
            rFrac := rFrac - 1
          }
        }
      }
    }
  }


  val rData = Reg(Bits(frameBits bits))
  val rFrame = Reg(Bool)
  val rOver = Reg(Bool)
  val rBufData = Reg(Bits(frameBits bits))
  val rBufFrame = Reg(Bool)
  val rBufOver = Reg(Bool)

  io.oData := rData
  io.oFrame := rFrame
  io.oOver := rOver

  val iBuffer = new StateMachine {
    io.oAccept := False
    io.oValid := False

    val Empty : State = new State with EntryPoint {
      whenIsActive {
        io.oAccept := True
        when (sStrobe) {
          rData := rShift
          rFrame := rCount =/= 0
          goto(Norm)
        }
      }
    }

    val Norm : State = new State {
      whenIsActive {
        io.oAccept := !sActive
        io.oValid := True
        when (io.iReady && sStrobe) {
          rData := rShift
          rFrame := rCount =/= 0
          rOver := False
        } elsewhen (io.iReady) {
          goto(Empty)
        } elsewhen (sStrobe) {
          rBufData := rShift
          rBufFrame := rCount =/= 0
          rBufOver := False
          goto(Full)
        }
      }
    }

    val Full : State = new State {
      whenIsActive {
        io.oAccept := False
        io.oValid := True
        when (io.iReady && sStrobe) {
          rData := rBufData
          rFrame := rBufFrame
          rOver := rBufOver
          rBufData := rShift
          rBufFrame := rCount =/= 0
          rBufOver := False
        } elsewhen (io.iReady) {
          rData := rBufData
          rFrame := rBufFrame
          rOver := rBufOver
          goto(Norm)
        } elsewhen (sStrobe) {
          rBufData := rShift
          rBufFrame := rCount =/= 0
          rBufOver := True
          // discard older in favour of newer data
        }
      }
    }

  }
}

