package hwlib.test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import hwlib.HwMain
import hwlib.amba.{AxiConfig, TAxi, ApbConfig, TApb, Amba}
import hwlib.comp.{AxiApbBridge, ApbRegFile}
import hwlib.base.{StaticDivider, Debouncer, IrqConfig, TIrq, IrqHigh}
import hwlib.serial.{TSerial, SerialConfig, StaticTransceiver, StaticTransceiverConfig}
import hwlib.xilinx.board.ZyboZ720
import hwlib.xilinx.tools.{BoardRefPhase, XilinxInterfacePhase, Zynq7Phase, ProjectGenerator, Zynq7Port, BoardRef}
import hwlib.sim._
import hwlib.sim.amba._


class Demo extends Component {
  val cfgSerial = Zynq7Port.CfgSerial
  val cfgReg  = AxiConfig(addrBits=12, dataBytes=4,  idBits=0,
                          hasBurst=false, hasProt=true)
  val cfgMem  = Zynq7Port.CfgSAxiHP
  val cfgApb8 = ApbConfig.fromAxi(cfgReg).copy(addrBits=3)
  val cfgApb4 = ApbConfig.fromAxi(cfgReg).copy(addrBits=2)
  val cfgIrq = IrqConfig(sense=IrqHigh)

  val io = new Bundle {
    val sReg     =  slave(TAxi(cfgReg))       addTag(Zynq7Port.MAxiGP(0)) addTag(Zynq7Port.CurrentClock)
    val mMem     = master(TAxi(cfgMem))       addTag(Zynq7Port.SAxiHP(0)) addTag(Zynq7Port.CurrentClock)
    val iButton  =     in(Bits(4 bits))       addTag(BoardRef("BUTTON"))
    val oLeds    =    out(Bits(4 bits))       addTag(BoardRef("LED"))
    val mEvent0  = master(TIrq(cfgIrq))       addTag(Zynq7Port.Irq(0))
    val mEvent1  = master(TIrq(cfgIrq))       addTag(Zynq7Port.Irq(1))
    val mEvent2  = master(TIrq(cfgIrq))       addTag(Zynq7Port.Irq(8))
    val mEvent3  = master(TIrq(cfgIrq))       addTag(Zynq7Port.Irq(9))
    val sDebug   =  slave(TSerial(cfgSerial)) addTag(Zynq7Port.Uart(0, fullSignals=true))
  }
  //io.sReg.nameThis()
  //io.mMem.nameThis()
  //noIoPrefix()


  val iClock100Hz = new StaticDivider(100 Hz)
  val iDebouncer = new Debouncer(4, 3)
  iDebouncer.io.iTick := iClock100Hz.io.oTick
  iDebouncer.io.iLine := io.iButton

  val iBridge = new AxiApbBridge(cfgReg,
      0x000L -> 4L,
      0x008L -> 8L,
      0x010L -> 8L)
  iBridge.io.sAxi << io.sReg

  // Regs Test @0x000
  /* Map:
     ADDR - R                 | W
     +00  - free counter      |  -
     +04  - last written data | write data
     +08  - count 04 writes   |  -
     +0C  - count 04 read     |  -
  */
  val iRegsTest = new ApbRegFile(cfgApb4)
  iRegsTest.io.sApb << iBridge.io.mApbs(0)
  val rTestCount = Counter(32 bits)
  val rWrite1Count = Counter(32 bits)
  val rRead1Count = Counter(32 bits)
  iRegsTest.io.iReg(0) := rTestCount.asBits
  iRegsTest.io.iReg(1) := iRegsTest.io.oReg(1)
  iRegsTest.io.iReg(2) := rWrite1Count.asBits
  iRegsTest.io.iReg(3) := rRead1Count.asBits
  rTestCount.increment()
  when (iRegsTest.io.oRegWr(1)) {
    rWrite1Count.increment()
  }
  when (iRegsTest.io.oRegRd(1)) {
    rRead1Count.increment()
  }


  // Regs Mem @0x020
  /* Map:
      ADDR - R                 | W
      +00  - RdDone            | Clear RdDone
      +04  - RdReady           | RdAddr (initiate)
      +08  - WrDone            | Clear WrDone
      +0C  - WrReady           | WrAddr (initiate)
      +10  - RdData( 31 ..  0) | WrData( 31 ..  0)
      +14  - RdData( 63 .. 32) | WrData( 63 .. 32)
      +18  - RdData( 95 .. 64) | WrData( 95 .. 64)
      +1C  - RdData(127 .. 96) | WrData(127 .. 96)
  */
  val iRegsMem = new ApbRegFile(cfgApb8)
  iRegsMem.io.sApb << iBridge.io.mApbs(1)
  val rRdResp  = Reg(cfgMem.TResp)
  val rRdData  = Reg(cfgMem.TData)
  val sRdAddr  = cfgReg.TData
  val sRdReady = Bool
  val sRdStrb  = Bool
  val sRdDone  = Bool
  val sRdAck   = Bool
  val rWrResp  = Reg(cfgMem.TResp)
  val sWrData  = cfgMem.TData
  val sWrAddr  = cfgReg.TData
  val sWrReady = Bool
  val sWrStrb  = Bool
  val sWrDone  = Bool
  val sWrAck   = Bool

  iRegsMem.io.iReg(0) := sRdDone.asBits.resized
  sRdAck  := iRegsMem.io.oRegWr(0)
  iRegsMem.io.iReg(1) := sRdReady.asBits.resized
  sRdAddr := iRegsMem.io.oReg(1)
  sRdStrb := iRegsMem.io.oRegWr(1)
  iRegsMem.io.iReg(4) := rRdData( 0, 32 bits)
  // iRegsMem.io.iReg(5) := rRdData(32, 32 bits)
  // iRegsMem.io.iReg(6) := rRdData(64, 32 bits)
  // iRegsMem.io.iReg(7) := rRdData(96, 32 bits)
  iRegsMem.io.iReg(5) := 0
  iRegsMem.io.iReg(6) := 0
  iRegsMem.io.iReg(7) := 0

  iRegsMem.io.iReg(2) := sWrDone.asBits.resized
  sWrAck  := iRegsMem.io.oRegWr(2)
  iRegsMem.io.iReg(3) := sWrReady.asBits.resized
  sWrAddr := iRegsMem.io.oReg(3)
  sWrStrb := iRegsMem.io.oRegWr(3)
  sWrData( 0, 32 bits) := iRegsMem.io.oReg(4)
  //sWrData(32, 32 bits) := iRegsMem.io.oReg(5)
  //sWrData(64, 32 bits) := iRegsMem.io.oReg(6)
  //sWrData(96, 32 bits) := iRegsMem.io.oReg(7)

  val iMemFsm = new StateMachine {
    sRdReady := False
    sRdDone  := False
    sWrReady := False
    sWrDone  := False
    io.mMem.aw.valid := False
    io.mMem.w.valid := False
    io.mMem.b.ready := False
    io.mMem.ar.valid := False
    io.mMem.r.ready := False

    val Idle : State = new State with EntryPoint {
      whenIsActive {
        sRdReady := True
        sWrReady := True
        // TODO-lw continue
        when (sWrStrb) {
          goto(WriteWaitAW)
        } elsewhen (sRdStrb) {
          goto(ReadWaitA)
        }
      }
    }

    val WriteWaitAW : State = new State {
      whenIsActive {
        io.mMem.aw.valid := True
        io.mMem.w.valid := True
        when (io.mMem.aw.ready && io.mMem.w.ready) {
          goto(WriteWaitB)
        } elsewhen (io.mMem.aw.ready) {
          goto(WriteWaitW)
        } elsewhen (io.mMem.w.ready) {
          goto(WriteWaitA)
        }
      }
    }
    val WriteWaitA : State = new State {
      whenIsActive {
        io.mMem.aw.valid := True
        when (io.mMem.aw.ready) {
          goto(WriteWaitB)
        }
      }
    }
    val WriteWaitW : State = new State {
      whenIsActive {
        io.mMem.w.valid := True
        when (io.mMem.w.ready) {
          goto(WriteWaitB)
        }
      }
    }
    val WriteWaitB : State = new State {
      whenIsActive {
        io.mMem.b.ready := True
        when (io.mMem.b.valid) {
          rWrResp := io.mMem.b.resp
          goto(WriteDone)
        }
      }
    }
    val WriteDone : State = new State {
      whenIsActive {
        sWrDone := True
        when (sWrAck) {
          goto(Idle)
        }
      }
    }

    val ReadWaitA : State = new State {
      whenIsActive {
        io.mMem.ar.valid := True
        when (io.mMem.ar.ready) {
          goto(ReadWaitR)
        }
      }
    }
    val ReadWaitR : State = new State {
      whenIsActive {
        io.mMem.r.ready := True
        when (io.mMem.r.valid) {
          rRdResp := io.mMem.r.resp
          rRdData := io.mMem.r.data
          goto(ReadDone)
        }
      }
    }
    val ReadDone : State = new State {
      whenIsActive {
        sRdDone := True
        when (sRdAck) {
          goto(Idle)
        }
      }
    }
  }

  io.mMem.aw.payload.assign("addr" -> sWrAddr.asUInt)
  io.mMem.w.payload.assign("data" -> sWrData)
  io.mMem.ar.payload.assign("addr" -> sRdAddr.asUInt)

  // Regs Intr @0x040
  /* Map:
      ADDR - R              | W
      +00  - Int0 Status    | Clear Int0
      +04  - Int1 Status    | Clear Int1
      +08  - Int2 Status    | Clear Int2
      +0C  - Int3 Status    | Clear Int3
      +10  - Int0 Countdown | Set and Start Int0 Countdown
      +14  - Int1 Countdown | Set and Start Int1 Countdown
      +18  - Int2 Countdown | Set and Start Int2 Countdown
      +1C  - Int3 Countdown | Set and Start Int3 Countdown
  */
  val iRegsIntr = new ApbRegFile(cfgApb8)
  iRegsIntr.io.sApb << iBridge.io.mApbs(2)
  val rActive0 = Reg(Bool).init(False)
  val rActive1 = Reg(Bool).init(False)
  val rActive2 = Reg(Bool).init(False)
  val rActive3 = Reg(Bool).init(False)
  val rCount0 = Reg(UInt(32 bits)).init(U(0))
  val rCount1 = Reg(UInt(32 bits)).init(U(0))
  val rCount2 = Reg(UInt(32 bits)).init(U(0))
  val rCount3 = Reg(UInt(32 bits)).init(U(0))

  iRegsIntr.io.iReg(0) := rActive0.asBits.resized
  iRegsIntr.io.iReg(1) := rActive1.asBits.resized
  iRegsIntr.io.iReg(2) := rActive2.asBits.resized
  iRegsIntr.io.iReg(3) := rActive3.asBits.resized
  iRegsIntr.io.iReg(4) := rCount0.asBits
  iRegsIntr.io.iReg(5) := rCount1.asBits
  iRegsIntr.io.iReg(6) := rCount2.asBits
  iRegsIntr.io.iReg(7) := rCount3.asBits

  when (iRegsIntr.io.oRegWr(4)) {
    rActive0 := True
    rCount0 := iRegsIntr.io.oReg(4).asUInt
  } elsewhen (iDebouncer.io.oRise(0)) {
    rActive0 := True
    rCount0 := 0
  } elsewhen (rActive0 && (rCount0 =/= 0)) {
    rCount0 := rCount0 - 1
  } elsewhen(iRegsIntr.io.oRegWr(0)) {
    rActive0 := False
  }
  io.mEvent0.irq := rActive0 && (rCount0 === 0)

  when (iRegsIntr.io.oRegWr(5)) {
    rActive1 := True
    rCount1 := iRegsIntr.io.oReg(5).asUInt
  } elsewhen (iDebouncer.io.oRise(1)) {
    rActive1 := True
    rCount1 := 0
  } elsewhen (rActive1 && (rCount1 =/= 0)) {
    rCount1 := rCount1 - 1
  } elsewhen(iRegsIntr.io.oRegWr(1)) {
    rActive1 := False
  }
  io.mEvent1.irq := rActive1 && (rCount1 === 0)

  when (iRegsIntr.io.oRegWr(6)) {
    rActive2 := True
    rCount2 := iRegsIntr.io.oReg(6).asUInt
  } elsewhen (iDebouncer.io.oRise(2)) {
    rActive2 := True
    rCount2 := 0
  } elsewhen (rActive2 && (rCount2 =/= 0)) {
    rCount2 := rCount2 - 1
  } elsewhen(iRegsIntr.io.oRegWr(2)) {
    rActive2 := False
  }
  io.mEvent2.irq := rActive2 && (rCount2 === 0)

  when (iRegsIntr.io.oRegWr(7)) {
    rActive3 := True
    rCount3 := iRegsIntr.io.oReg(7).asUInt
  } elsewhen (iDebouncer.io.oRise(3)) {
    rActive3 := True
    rCount3 := 0
  } elsewhen (rActive3 && (rCount3 =/= 0)) {
    rCount3 := rCount3 - 1
  } elsewhen(iRegsIntr.io.oRegWr(3)) {
    rActive3 := False
  }
  io.mEvent3.irq := rActive3 && (rCount3 === 0)

  io.oLeds := rActive3 ## rActive2 ## rActive1 ## rActive0

  // Serial debug interface (only loopback for now!)
  val transceiverCfg = StaticTransceiverConfig(cfgSerial,
    isMaster = false,
    baudRate = 115200 Hz,
    dataBits = 8,
    msbFirst = false,
    hasParity = true,
    oddParity = false)
  val iTransceiver = new StaticTransceiver(transceiverCfg)
  iTransceiver.io.sSerial << io.sDebug
  iTransceiver.io.sData << iTransceiver.io.mData

}

object Demo extends HwMain[Demo] {
  genWith(new Demo)
  clockAt(100 MHz)
  clockCfg(resetKind = SYNC)

  genEarlyPhase(new BoardRefPhase(ZyboZ720))

  genLatePhase(new XilinxInterfacePhase)
  genLatePhase(new Zynq7Phase)

  postGen{ (args, report) =>
    val projGen = new ProjectGenerator(report, ZyboZ720, Some("ZyboDemo"))
    projGen.generate()
  }

}
