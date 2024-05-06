package hwlib.test

import scala.util.Random

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.xilinx.blackbox._
import hwlib.sim._
import hwlib.HwMain



class CdcComponents(dataBits : Int = 4, timeBits : Int = 8) extends Component {
  require(dataBits > 0)
  require(timeBits > 0)

  def TData = Bits(dataBits bits)
  def TTime = UInt(timeBits bits)

  case class TPacket() extends Bundle {
    val data = TData
    val trig = Bool
    val time = TTime
    val over = Bool
    val last = Bool
  }
  val packetBits = widthOf(TPacket())

  /*object TControl extends Bundle {
    val active = Bool
    val time = TTime
  }

  object TStatus extends Bundle {
    val skewed = Bool
    val time = TTime
  }*/

  val dHeadClock = ClockDomain.external("head", frequency=ClockDomain.FixedFrequency(250 MHz))

  val io = new Bundle {
    // head clock domain
    val iData     = in(TData)
    val iTrigger  = in(Bool)

    // default clock domain
    val iStart    =  in(Bool)
    val iStop     =  in(Bool)
    val oStarting = out(Bool)
    val oStopping = out(Bool)
    val oActive   = out(Bool)
    val oSkewed   = out(Bool)

    val oData     = out(TData)
    val oTrig     = out(Bool)
    val oTime     = out(TTime)
    val oOver     = out(Bool)
    val oLast     = out(Bool)
    val oValid    = out(Bool)
    val iReady    =  in(Bool)
  }

  val iCdcActivate = new CdcHandshake(CdcHandshakeConfig(1, extern=true), dstClock=dHeadClock)
  val iCdcSkewed   = new CdcIndividual(CdcIndividualConfig(None), srcClock=dHeadClock)
  val iCdcFifo     = new FifoMacro(FifoMacroConfig(16, packetBits, packetBits), putClock=dHeadClock)


  ///// Pipeline Head (head clock domain) /////

  val iHead = new ClockingArea(dHeadClock) {
    val rTime         = Reg(TTime)
    val rNextTime     = Reg(TTime)
    val rLastTime     = Reg(TTime)
    val sRelTime      = TTime
    val sHold         = Bool
    val sUpdate       = Bool

    val rSkewed       = Reg(Bool)
    val rOver         = Reg(Bool)

    val sInit         = Bool
    val sActive       = Bool
    val sLastReq      = Bool
    val sLastAck      = Bool

    val sActivate      = Bool
    val sActivateReq   = Bool
    val sActivateAck   = Bool

    val sPacket       = TPacket()
    val sPacketStrobe = Bool
    val sPacketReady  = Bool


    /// Timestamp Logic ///

    sRelTime := rTime - rLastTime
    sHold := rNextTime === rLastTime
    sUpdate := sRelTime(widthOf(TTime) - 1) // is 1/2 threshold crossed?

    sPacket.data := io.iData
    sPacket.trig := io.iTrigger
    sPacket.time := sRelTime
    sPacket.over := rOver
    sPacket.last := sLastReq

    sLastAck := False
    sPacketStrobe := False
    when (sInit) {
      rTime := 0
      rNextTime := 1
      rLastTime := 0
      rSkewed := False
    } elsewhen (sActive) {
      when (sHold) {
        rSkewed := True
      } otherwise {
        rTime := rNextTime
        rNextTime := rNextTime + 1
      }
      when (io.iTrigger) {
        when (!sPacketReady) { // ?|| sHold?
          rOver := True
        } otherwise {
          rOver := False
          rLastTime := rTime
          sPacketStrobe := True
        }
      } elsewhen (sUpdate) {
        when (sPacketReady) {
          rOver := False
          rLastTime := rTime
          sPacketStrobe := True
        }
      }
    } elsewhen (sLastReq) {
      when (sPacketReady) {
        rOver := False
        rLastTime := rTime
        sPacketStrobe := True
        sLastAck := True
      }
    }


    /// Handshake State Machine ///

    val iFsm = new StateMachine {
      sActivateAck := False
      sInit := False
      sActive := False
      sLastReq := False

      val IdleAck : State = new State {
        whenIsActive {
          sActivateAck := True
          when (!sActivateReq) {
            goto(Idle)
          }
        }
      }

      val Idle : State = new State with EntryPoint {
        whenIsActive {
          when (sActivateReq) {
            when (sActivate) {
              goto(Init)
            } otherwise {
              goto(IdleAck)
            }
          }
        }
      }

      val Init : State = new State {
        whenIsActive {
          sInit := True
          sActivateAck := True
          when (!sActivateReq) {
            goto(Active)
          }
        }
      }

      val Active : State = new State {
        whenIsActive {
          sActive := True
          when (sActivateReq) {
            when (sActivate) {
              goto(ActiveAck)
            } otherwise {
              goto(Last)
            }
          }
        }
      }

      val ActiveAck : State = new State {
        whenIsActive {
          sActive := True
          sActivateAck := True
          when (!sActivateReq) {
            goto(Active)
          }
        }
      }

      val Last : State = new State {
        whenIsActive {
          sLastReq := True
          when (sLastAck) {
            goto(IdleAck)
          }
        }
      }

    }


    /// Tail Connections ///

    sActivate := iCdcActivate.io.oDstPayload(0)
    sActivateReq := iCdcActivate.io.oDstReq
    iCdcActivate.io.iDstAck := sActivateAck

    iCdcSkewed.io.iSrcSignal := rSkewed

    iCdcFifo.io.iPutData := sPacket.asBits
    iCdcFifo.io.iPutEnable := sPacketStrobe
    sPacketReady := !iCdcFifo.io.oPutFull && !iCdcFifo.io.oPutInReset
  }


  ///// Pipeline Tail (default clock domain) /////

  val iTail = new Area {
    val sActivate    = Bool
    val sActivateReq = Bool
    val sActivateAck = Bool

    val sPacket      = TPacket()
    val sPacketValid = Bool
    val sPacketReady = Bool


    /// Fifo Egress Logic ///

    io.oData := sPacket.data
    io.oTrig := sPacket.trig
    io.oTime := sPacket.time
    io.oOver := sPacket.over
    io.oLast := sPacket.last
    io.oValid := sPacketValid
    sPacketReady := io.iReady


    /// Handshake State Machine ///


    val iFsm = new StateMachine {
      io.oActive   := False
      io.oStarting := False
      io.oStopping := False
      sActivate    := False
      sActivateReq := False

      val Idle : State = new State with EntryPoint {
        whenIsActive {
          when (io.iStart) {
            goto(Starting)
          }
        }
      }

      val Starting : State = new State {
        whenIsActive {
          io.oStarting := True
          sActivate := True
          sActivateReq := True
          when (sActivateAck) {
            goto(Started)
          }
        }
      }

      val Started : State = new State {
        whenIsActive {
          io.oActive := True
          sActivate := True
          when (!sActivateAck) {
            goto(Active)
          }
        }
      }

      val Active : State = new State {
        whenIsActive {
          io.oActive := True
          sActivate := True
          when (io.iStop) {
            goto(Stopping)
          }
        }
      }

      val Stopping : State = new State {
        whenIsActive {
          io.oActive := True
          io.oStopping := True
          sActivate := False
          sActivateReq := True
          when (sActivateAck) {
            goto(Stopped)
          }
        }
      }

      val Stopped : State = new State {
        whenIsActive {
          io.oStopping := True
          sActivate := False
          when (!sActivateAck) {
            goto(Idle)
          }
        }
      }

    }


    /// Head Connections ///

    iCdcActivate.io.iSrcPayload(0) := sActivate
    iCdcActivate.io.iSrcReq := sActivateReq
    sActivateAck := iCdcActivate.io.oSrcAck

    io.oSkewed := iCdcSkewed.io.oDstSignal

    sPacket.assignFromBits(iCdcFifo.io.oGetData)
    sPacketValid := !iCdcFifo.io.oGetInReset && !iCdcFifo.io.oGetEmpty
    iCdcFifo.io.iGetEnable := sPacketValid && sPacketReady

  }

}

object CdcComponents extends HwMain[CdcComponents] {

  simWith(new CdcComponents)
  clockAt(150 MHz)

  simRun{ dut =>
    val env = new ClockEnv(dut.clockDomain)
    val envBeta = new ClockEnv(dut.dHeadClock)

    env.timeout(10000)

    env.onRawCycle(100) { () =>
     env.setRst(false)
     envBeta.setRst(false)
    }

    fork {
      dut.io.iData #= 0
      dut.io.iTrigger #= false
      envBeta.waitFor(4)
      while (true) {
        dut.io.iData #= Random.between(0, 16)
        dut.io.iTrigger #= true
        envBeta.waitFor(1)
        dut.io.iTrigger #= false
        randomWait(envBeta, 16, 0.01, 600)
      }
    }

    fork {
      dut.io.iReady #= false
      env.waitFor(4)
      while (true) {
        dut.io.iReady #= true
        env.waitNextWhen(dut.io.oValid.toBoolean)
        dut.io.iReady #= false
        randomWait(env, 4, 0.02, 150)
      }
    }

    dut.io.iStart #= false
    dut.io.iStop  #= false
    env.waitFor(4)

    for (rep <- 0 until 4) {
      dut.io.iStart #= true
      env.waitNextWhen(dut.io.oStarting.toBoolean)
      dut.io.iStart #= false
      env.waitNextWhen(dut.io.oActive.toBoolean)

      env.waitFor(1024)

      dut.io.iStop #= true
      env.waitNextWhen(dut.io.oStopping.toBoolean)
      dut.io.iStop #= false
      env.waitNextWhen(!dut.io.oActive.toBoolean)

      env.waitFor(256)
    }
  }

  def randomWait(env : ClockEnv, normal : Int, longRate : Double = 0.0, long : Int = 1000) {
    if (Random.nextDouble() < longRate) {
      env.waitFor(long)
    } else {
      val delay = Random.between(0, normal)
      if (delay > 0) {
        env.waitFor(delay)
      }
    }
  }
}
