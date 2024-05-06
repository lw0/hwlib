package hwlib.comp

import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import hwlib.base.{MemoryPortConfig, TMemoryPort, MemoryPortRead, MemoryPortWrite, MemoryPortBoth, MaskMux}
import hwlib.amba.{AxiConfig, TAxi, AxiStreamConfig, TAxiStream, ApbConfig, TApb}
import hwlib.HwMain
import hwlib.sim._
import hwlib.sim.amba._


class AxiMemInterface(val cfg : AxiConfig,
                      val denyRead : Boolean = false,
                      val denyWrite : Boolean = false,
                      val combined : Boolean = false,
                      val hasReq : Boolean = true,
                      val hasGrant : Boolean = true,
                      val hasError : Boolean = false) extends Component {
  require(!cfg.isAxi3)

  val canRead  = cfg.hasRd && !denyRead
  val canWrite = cfg.hasWr && !denyWrite
  val hasCPort = combined && canRead && canWrite
  val hasRPort = canRead && !hasCPort
  val hasWPort = canWrite && !hasCPort

  val cfgRPort = MemoryPortConfig(MemoryPortRead, cfg.waddrBits, cfg.dataBits, hasReq, hasGrant, hasError)
  val cfgWPort = MemoryPortConfig(MemoryPortWrite, cfg.waddrBits, cfg.dataBits, hasReq, hasGrant, hasError, cfg.dataBytes)
  val cfgCPort = MemoryPortConfig(MemoryPortBoth, cfg.waddrBits, cfg.dataBits, hasReq, hasGrant, hasError,cfg.dataBytes)

  ///// Interface /////
  val io = new Bundle {
    val sAxi    = slave(TAxi(cfg))

    val mPortRd = hasRPort generate master(TMemoryPort(cfgRPort))
    val mPortWr = hasWPort generate master(TMemoryPort(cfgWPort))
    val mPort   = hasCPort generate master(TMemoryPort(cfgCPort))
  }


  ///// Read Operations /////

  val iReadPlug = (cfg.hasRd && denyRead) generate io.sAxi.rdPlugFsm()

  val iReadLogic = canRead generate new Area {
    val sGenActive = Bool
    val sGenAddr   = cfg.TWAddr
    val sGenStep   = Bool
    val sGenLast   = Bool
    val sGenError  = Bool
    val sGenNext   = Bool

    val sPortAddr  = cfg.TWAddr
    val sPortReq   = Bool
    val sPortData  = cfg.TData
    val sPortError = Bool
    val sPortGrant = Bool

    /// Read Address Channel ///
    val rGenId = cfg.hasId generate RegNextWhen(io.sAxi.ar.id, io.sAxi.ar.valid && !sGenActive)

    val iAddrGen = if (cfg.hasBurst) new Area {
      val iSequencer = new AxiSequencer(cfg)
      iSequencer.io.iAddr  := io.sAxi.ar.addr
      iSequencer.io.iLen   := io.sAxi.ar.len
      iSequencer.io.iSize  := io.sAxi.ar.size
      iSequencer.io.iBurst := io.sAxi.ar.burst
      iSequencer.io.iStart := io.sAxi.ar.valid
      io.sAxi.ar.ready := !iSequencer.io.oActive
      sGenActive := iSequencer.io.oActive
      sGenAddr   := iSequencer.io.oWAddr
      sGenStep   := iSequencer.io.oBegin
      sGenLast   := iSequencer.io.oLast
      sGenError  := iSequencer.io.oError
      iSequencer.io.iNext := sGenNext
    } else new Area {
      val rActive = Reg(Bool) init False
      val rAddr = Reg(cfg.TWAddr)
      when (!rActive && io.sAxi.ar.valid) {
        rAddr := io.sAxi.ar.waddr
        rActive := True
      } elsewhen (rActive & sGenNext) {
        rActive := False
      }
      io.sAxi.ar.ready := !rActive
      sGenActive := rActive
      sGenAddr   := rAddr
      sGenStep   := True
      sGenLast   := True
      sGenError  := False
    }

    /// Read Memory Port ///
    val rOutId = cfg.hasId generate RegNextWhen(rGenId, sGenActive)
    val sOutData = cfg.TData
    val sOutStep = Bool
    val sOutGenError = Bool
    val sOutDataError = Bool
    val sOutLast = Bool
    val sOutStrobe = Bool
    val sOutHold = Bool

    sGenNext := sGenActive && (!sGenStep || sPortGrant) && !sOutHold

    sPortAddr := sGenAddr
    sPortReq := sGenActive && sGenStep && !sOutHold

    sOutData := sPortData
    sOutDataError := sPortError
    sOutStep := RegNext(sGenStep)
    sOutGenError := RegNext(sGenError)
    sOutLast := RegNext(sGenLast)
    sOutStrobe := RegNext(sGenNext)

    /// Read Data Channel ///

    val rData = Reg(cfg.TData)
    val rError = Reg(Bool)
    val rLast = Reg(Bool)
    val rId = cfg.hasId generate Reg(cfg.TId)
    val rBufData = Reg(cfg.TData)
    val rBufError = Reg(Bool)
    val rBufLast = Reg(Bool)
    val rBufId = cfg.hasId generate Reg(cfg.TId)

    // TODO-lw consider adding a triple buffer, so hold can remain disabled in Full state

    val iFsm = new StateMachine {
      sOutHold := False
      io.sAxi.r.valid := False

      val Empty : State = new State with EntryPoint {
        whenIsActive {
          when (sOutStrobe) {
            if (cfg.hasId) {
              rId := rOutId
            }
            when (sOutGenError) {
              rError := True
            } elsewhen (sOutStep) {
              // !sOutStep maintains last rData & rError value
              rData := sOutData
              rError := sOutDataError
            }
            rLast := sOutLast
            goto(Full)
          }
        }
      }

      val Full : State = new State {
        whenIsActive {
          sOutHold := !io.sAxi.r.ready
          io.sAxi.r.valid := True
          when (io.sAxi.r.ready && sOutStrobe) {
            if (cfg.hasId) {
              rId := rOutId
            }
            when (sOutGenError) {
              rError := True
            } elsewhen (sOutStep) {
              // !sOutStep maintains last rData & rError value
              rData := sOutData
              rError := sOutDataError
            }
            rLast := sOutLast
          } elsewhen (io.sAxi.r.ready) {
            goto(Empty)
          } elsewhen (sOutStrobe) {
            if (cfg.hasId) {
              rBufId := rOutId
            }
            when (sOutGenError) {
              rBufError := True
            } elsewhen (sOutStep) {
              // !sOutStep maintains last rData & rError value
              rBufData := sOutData
              rBufError := sOutDataError
            } otherwise {
              rBufData := rData
              rBufError := rError
            }
            rBufLast := sOutLast
            goto(Over)
          }
        }
      }

      val Over : State = new State {
        whenIsActive {
          sOutHold := True
          io.sAxi.r.valid := True
          // TODO-lw ensure that sOutStrobe can not occur here
          when (io.sAxi.r.ready) {
            if (cfg.hasId) {
              rId := rBufId
            }
            rData := rBufData
            rError := rBufError
            rLast := rBufLast
            goto(Full)
          }
        }
      }
    }

    io.sAxi.r.data := rData
    io.sAxi.r.resp := Mux(rError, cfg.cRespSlvErr, cfg.cRespOkay)
    io.sAxi.r.last := rLast
    if (cfg.hasId) {
      io.sAxi.r.id := rId
    }
  }


  ///// Write Operations /////

  val iWritePlug = (cfg.hasWr && denyWrite) generate io.sAxi.wrPlugFsm()

  val iWriteLogic = canWrite generate new Area {
    val sPortAddr = cfg.TWAddr
    val sPortData = cfg.TData
    val sPortMask = cfg.TMask
    val sPortReq = Bool
    val sPortError = Bool
    val sPortGrant = Bool

    val sGenActive = Bool
    val sGenAddr  = cfg.TWAddr
    val sGenMask  = cfg.TMask
    val sGenFirst  = Bool
    val sGenBeg  = Bool
    val sGenEnd  = Bool
    val sGenLast  = Bool
    val sGenError = Bool
    val sGenNext  = Bool

    val sWriteClear = Bool
    val sWriteUse = Bool
    val sWriteMerge = Bool
    val sWriteSend = Bool
    val sWriteHold = Bool
    val sWriteError = Bool

    /// Address Generator: Axi AW --> Gen* ///
    val rGenId = cfg.hasId generate RegNextWhen(io.sAxi.aw.id, io.sAxi.aw.valid && !sGenActive)

    val iAddrGen = if (cfg.hasBurst) new Area {
      val iSequencer = new AxiSequencer(cfg)
      iSequencer.io.iAddr  := io.sAxi.aw.addr
      iSequencer.io.iLen   := io.sAxi.aw.len
      iSequencer.io.iSize  := io.sAxi.aw.size
      iSequencer.io.iBurst := io.sAxi.aw.burst
      iSequencer.io.iStart := io.sAxi.aw.valid
      io.sAxi.aw.ready := !iSequencer.io.oActive
      sGenActive := iSequencer.io.oActive
      sGenAddr  := iSequencer.io.oWAddr
      sGenMask  := iSequencer.io.oMask
      sGenBeg  := iSequencer.io.oEnd
      sGenEnd  := iSequencer.io.oEnd
      sGenLast  := iSequencer.io.oLast
      sGenError := iSequencer.io.oError
      iSequencer.io.iNext := sGenNext
    } else new Area {
      val rActive = Reg(Bool) init False
      val rAddr = Reg(cfg.TWAddr)
      when (!rActive && io.sAxi.aw.valid) {
        rAddr := io.sAxi.aw.waddr
        rActive := True
      } elsewhen (rActive & sGenNext) {
        rActive := False
      }
      io.sAxi.aw.ready := !rActive

      sGenActive := rActive
      sGenAddr := rAddr
      sGenMask := cfg.cMaskAll
      sGenBeg := True
      sGenEnd := True
      sGenLast := True
      sGenError := False
    }

    /* sGenAddr
     * sGenMask
     * sGenFirst
     * sGenBeg
     * sGenEnd
     * sGenLast
     * sGenError
     * sGenActive
     *             sGenNext
     */


    /// Data Path: Axi W --> Port ///
    val sAxiData = cfg.TData
    val sAxiMask = cfg.TMask
    val sAxiLast = Bool
    val sAxiValid = Bool
    val sAxiReady = Bool

    val rPortData = Reg(cfg.TData)
    val rPortMask = Reg(cfg.TMask)
    val rPortAddr = Reg(cfg.TWAddr)
    val rPortReq = Reg(Bool)

    sAxiData := io.sAxi.w.data
    sAxiMask := (if (cfg.hasStrb) {
      io.sAxi.w.strb
    } else {
      cfg.cMaskAll
    })
    sAxiLast := (if (cfg.hasBurst) {
      io.sAxi.w.last
    } else {
      True
    })
    sAxiValid := io.sAxi.w.valid
    io.sAxi.w.ready := sAxiReady

    sWriteHold := True
    when (!rPortReq || sPortGrant) {
      sWriteHold := False
      when (sWriteClear) {
        rPortData := 0
        rPortMask := cfg.cMaskNone
      } elsewhen (sWriteUse) {
        when (sWriteMerge) {
          rPortData := MaskMux(sGenMask, sAxiData, rPortData)
          rPortMask := MaskMux(sGenMask, sAxiMask, rPortMask)
        } otherwise {
          rPortData := MaskMux(sGenMask, sAxiData, 0)
          rPortMask := MaskMux(sGenMask, sAxiMask, 0)
        }
      }
      when (sWriteSend) {
        rPortAddr := sGenAddr
        rPortReq := True
      } otherwise {
        rPortReq := False
      }
    }
    sWriteError  := sPortError && sPortGrant && rPortReq

    sPortAddr := rPortAddr
    sPortData := rPortData
    sPortMask := rPortMask
    sPortReq  := rPortReq


    /* sAxiLast
     * sAxiValid
     *              sAxiReady
     *              sWriteClear
     *              sWriteUse
     *              sWriteMerge
     *              sWriteSend
     * sWriteHold
     * sWriteError
     */


    /// Write Core Logic: * --> Axi B ///
    val sError = Bool

    val rRspId = cfg.hasId generate RegNextWhen(rGenId, sGenActive)
    val rRspError = Reg(Bool)
    val rRspValid = Reg(Bool)

    val sRspSend = Bool
    val sRspHold = Bool

    sRspHold := True
    when (!rRspValid || io.sAxi.b.ready) {
      sRspHold := False
      when (sRspSend) {
        rRspError := sError
        rRspValid := True
      } otherwise {
        rRspValid := False
      }
    }
    io.sAxi.b.resp := Mux(rRspError, cfg.cRespSlvErr, cfg.cRespOkay)
    if (cfg.hasId) {
      io.sAxi.b.id := rRspId
    }
    io.sAxi.b.valid := rRspValid


    val rError = Reg(Bool)
    val sFrameError = Bool
    val sErrorUse = Bool
    val sErrorMerge = Bool

    when (sErrorUse) {
      when (sErrorMerge) {
        sError := sWriteError || sGenError || sFrameError || rError
      } otherwise {
        sError := sWriteError || sGenError || sFrameError
      }
      rError := sError
    } otherwise {
      sError := rError
    }

    /* sFrameError
     * sErrorUse
     * sErrorMerge
     */


    val iFsm = new StateMachine {
      // sGenFirst
      // sGenBeg
      // sGenEnd
      // sGenLast
      // sGenError
      // sGenActive
      sGenNext := False

      // sAxiLast
      // sAxiValid
      sAxiReady := False
      sWriteClear := False
      sWriteUse := False
      sWriteMerge := False
      sWriteSend := False
      // sWriteHold

      sRspSend := False
      // sRspHold

      sFrameError := False
      sErrorUse := False
      sErrorMerge := False

      val Normal : State = new State with EntryPoint { whenIsActive {
        sAxiReady := sGenActive && !sWriteHold
      } }
      val GenDrain : State = new State with EntryPoint { whenIsActive {
        sAxiReady := sGenActive
        when (sGenActive & sAxiValid) {
        }
      } }

      val Error : State = new State { whenIsActive {
      } }
    }

  }


  /// Local Memory Ports ///

  val iCPort = hasCPort generate new Area {
    io.mPort.wdata := iWriteLogic.sPortData
    io.mPort.wmask := iWriteLogic.sPortMask
    iReadLogic.sPortData := io.mPort.rdata
    iReadLogic.sPortError := io.mPort.getError
    iWriteLogic.sPortError := io.mPort.getError

    val iFsm = new StateMachine {
      io.mPort.addr := iReadLogic.sPortAddr
      io.mPort.write := False
      io.mPort.setReq(iReadLogic.sPortReq)
      iReadLogic.sPortGrant := False
      iWriteLogic.sPortGrant := False

      val Read : State = new State with EntryPoint {
        whenIsActive {
          io.mPort.addr := iReadLogic.sPortAddr
          io.mPort.write := False
          io.mPort.setReq(iReadLogic.sPortReq)
          iReadLogic.sPortGrant := io.mPort.getGrant
          when ((!iReadLogic.sPortReq || io.mPort.getGrant) && iWriteLogic.sPortReq) {
            goto(Write)
          }
        }
      }
      val Write : State = new State {
        whenIsActive {
          io.mPort.addr := iWriteLogic.sPortAddr
          io.mPort.write := True
          io.mPort.setReq(iWriteLogic.sPortReq)
          iWriteLogic.sPortGrant := io.mPort.getGrant
          when ((!iWriteLogic.sPortReq || io.mPort.getGrant) && iReadLogic.sPortReq) {
            goto(Read)
          }
        }
      }
    }
  }

  if (hasRPort) {
    io.mPortRd.addr := iReadLogic.sPortAddr
    io.mPortRd.setReq(iReadLogic.sPortReq)
    iReadLogic.sPortData  := io.mPortRd.rdata
    iReadLogic.sPortError := io.mPortRd.getError
    iReadLogic.sPortGrant := io.mPortRd.getGrant
  }

  if (hasWPort) {
    io.mPortWr.addr := iWriteLogic.sPortAddr
    io.mPortWr.wdata := iWriteLogic.sPortData
    io.mPortWr.wmask := iWriteLogic.sPortMask
    io.mPortWr.setReq(iWriteLogic.sPortReq)
    iWriteLogic.sPortError := io.mPortWr.getError
    iWriteLogic.sPortGrant := io.mPortWr.getGrant
  }
}


object AxiMemInterface extends HwMain[AxiMemInterface] {

  clockAt(200 MHz)

  simSuiteWith("ReadOnly")(new AxiMemInterface(AxiConfig(addrBits=16, dataBytes=16, idBits=4), denyWrite=true))
  simSuiteRun("ReadOnly") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)
    env.timeout(65536)
    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val iMemory = new MemoryModel(dataBits = dut.cfg.dataBits)
    iMemory.connect(dut.io.mPortRd, delayGen = Random.between(0, 4), errorGen = Random.nextDouble() < 0.0)

    val axiModelCfg = AxiModelConfig(dataRate=RateGen(0.6))
    val iAxiRd = new AxiRdMaster(dut.io.sAxi, axiModelCfg)

    env.waitFor()

    val tA = iAxiRd.readOn(0xA, 0xC000, 16, Size.S8B, Burst.Wrap)
    val tB = iAxiRd.readOn(0xB, 0xC004)
    val tC = iAxiRd.readOn(0xC, 0x210, 5, Size.S16B, Burst.Incr)
    val tD = iAxiRd.readOn(0xD, 0xFF0, 50, Size.S4B, Burst.Incr) // cross 4K boundary to simulate error
    val tE = iAxiRd.readOn(0xE, 0x0C0, 256, Size.S4B, Burst.Incr)
    tA.blockValue()
    tB.blockValue()
    tC.blockValue()
    tD.blockValue()
    tE.blockValue()

    env.waitFor(108)
  }

  simSuiteWith("WriteOnly")(new AxiMemInterface(AxiConfig(addrBits=16, dataBytes=16, idBits=4), denyRead=true))
  simSuiteRun("WriteOnly") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)
    env.timeout(65536)
    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val iMemory = new MemoryModel(dataBits = dut.cfg.dataBits)
    iMemory.connect(dut.io.mPortRd, delayGen = Random.between(0, 4), errorGen = Random.nextDouble() < 0.0)

    val axiModelCfg = AxiModelConfig(dataRate=RateGen(0.6))
    val iAxiRd = new AxiWrMaster(dut.io.sAxi, axiModelCfg)

    env.waitFor()

    val tA = iAxiRd.writeOn(0xA, 0xC000, BigInt("A7A6A5A4A3A2A1A0B7B6B5B4B3B2B1"), 16, Size.S8B, Burst.Wrap)
    val tB = iAxiRd.writeOn(0xB, 0xC004, BigInt("D3D2D1D0"))
    val tC = iAxiRd.writeOn(0xC, 0x210, 0x6c06c, 5, Size.S16B, Burst.Incr)
    val tD = iAxiRd.writeOn(0xD, 0xFF0, 0x6c06c, 50, Size.S4B, Burst.Incr) // cross 4K boundary to simulate error
    val tE = iAxiRd.writeOn(0xE, 0x0C0, 0x6c06c, 256, Size.S4B, Burst.Incr)
    tA.blockValue()
    tB.blockValue()
    tC.blockValue()
    tD.blockValue()
    tE.blockValue()

    env.waitFor(108)
  }

  simSuiteWith("Combined")(new AxiMemInterface(AxiConfig(addrBits=16, dataBytes=16), combined=true))
  simSuiteRun("Combined") { dut =>
    implicit val env = new ClockEnv(dut.clockDomain)
    env.timeout(65536)
    env.onRawCycle(2) { () =>
     env.setRst(false)
    }

    val iMemory = new MemoryModel(dataBits = dut.cfg.dataBits)
    iMemory.connect(dut.io.mPort, delayGen = Random.between(0, 4), errorGen = Random.nextDouble() < 0.1)

    val axiModelCfg = AxiModelConfig(dataRate=RateGen(0.75))
    val iAxiWr = new AxiWrMaster(dut.io.sAxi, axiModelCfg)
    val iAxiRd = new AxiRdMaster(dut.io.sAxi, axiModelCfg)

    env.waitFor()

    val wrDone = Future[Unit]()
    fork {
      // TODO-lw implement
      wrDone.resolve()
    }

    val rdDone = Future[Unit]()
    fork {
      // TODO-lw implement
      rdDone.resolve()
    }

    wrDone.blockValue()
    rdDone.blockValue()
    env.waitFor(108)
  }
}
