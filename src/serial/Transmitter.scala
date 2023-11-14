package hwlib.serial

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._



class Transmitter(val frameBits : Int = 9, val fracBits : Int = 4) extends Component {
  require(frameBits > 0)
  require(fracBits >= 2)

  val countBits = log2Up(frameBits)

  def OneStopBit = U(0, countBits bits)
  def TwoStopBits = U(1, countBits bits)
  def StopBits(secondStop : Boolean) = if (secondStop) { TwoStopBits } else { OneStopBit }

  def MaxDataBits = U(frameBits - 1, countBits bits)
  def FracFull = U((1 << fracBits) - 1, fracBits bits)


  val io = new Bundle {
    val iEnable   =  in(Bool) default True
    val iBaudTick =  in(Bool)

    val oLine     = out(Bool)
    val iAccept   =  in(Bool) default True

    val iStopBits =  in(UInt(countBits bits)) default OneStopBit
    val iDataBits =  in(UInt(countBits bits)) default MaxDataBits

    val iData     =  in(Bits(frameBits bits))
    val iValid    =  in(Bool)
    val oReady    = out(Bool)

    val iBreak    =  in(Bool) default False
  }


  val rFrac = Reg(UInt(fracBits bits))
  val rCount = Reg(UInt(countBits bits))

  val rShift = Reg(Bits(frameBits bits))

  val iShifter = new StateMachine {
    io.oReady := False
    io.oLine := True

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        when (io.iBreak) {
          rCount := io.iDataBits
          goto(Break)
        } elsewhen (io.iEnable && io.iAccept) {
          io.oReady := True
          when (io.iValid && io.iDataBits <= MaxDataBits) {
            // Skip data if iDataBits is invalid
            rShift := io.iData
            rCount := io.iDataBits
            rFrac := FracFull
            goto(Start)
          }
        }
      }
    }

    val Start : State = new State {
      whenIsActive {
        io.oLine := False
        when(io.iBaudTick) {
          when (rFrac === 0) {
            rFrac := FracFull
            goto(Data)
          } otherwise {
            rFrac := rFrac - 1
          }
        }
      }
    }

    val Data : State = new State {
      whenIsActive {
        io.oLine := rShift(0)
        when(io.iBaudTick) {
          when (rFrac === 0) {
            rFrac := FracFull
            rShift := False ## rShift(frameBits-1 downto 1)
            when (rCount === 0) {
              rCount := io.iStopBits
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
        io.oLine := True
        when(io.iBaudTick) {
          when (rFrac === 0) {
            rFrac := FracFull
            when (rCount === 0) {
              goto(Idle)
            } otherwise {
              rCount := rCount - 1
            }
          } otherwise {
            rFrac := rFrac - 1
          }
        }
      }
    }

    val BreakStart : State = new State {
      whenIsActive {
        io.oLine := False
        when (rFrac === 0) {
          rFrac := FracFull
          goto(Break)
        } otherwise {
          rFrac := rFrac - 1
        }
      }
    }

    val Break : State = new State {
      whenIsActive {
        io.oLine := False
        when (rFrac === 0) {
          rFrac := FracFull
          when (rCount === 0) {
            goto(BreakStop)
          } otherwise {
            rCount := rCount - 1
          }
        } otherwise {
          rFrac := rFrac - 1
        }
      }
    }

    val BreakStop : State = new State {
      whenIsActive {
        io.oLine := False
        when (rFrac === 0) {
          rFrac := FracFull
          when (!io.iBreak) {
            goto(Idle)
          }
        } otherwise {
          rFrac := rFrac - 1
        }
      }
    }

  }
}

