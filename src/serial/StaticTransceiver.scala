package hwlib.serial

import scala.collection.mutable

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

import hwlib.sim._
import hwlib.base.StaticDivider
import hwlib.HwMain


case class StaticTransceiverConfig(
  lineCfg : SerialConfig,
  isMaster : Boolean,
  baudRate : HertzNumber,
  dataBits : Int = 8,
  msbFirst : Boolean = false,
  hasParity : Boolean = false,
  oddParity : Boolean = false,
  secondStop : Boolean = false,
  baudFrac : Int = 4,
  canReceiveError : Boolean = true,
  canSendError : Boolean = true) {

  require(dataBits >= 5 && dataBits <= 8)
  require(baudFrac >= 2)

  val frameBits = dataBits + (if (hasParity) {1} else {0})
}


class StaticTransceiver(val cfg : StaticTransceiverConfig) extends Component {

  val io = new Bundle {
    val mSerial  = ifGen (cfg.isMaster) { master(TSerial(cfg.lineCfg)) }
    val sSerial  = ifGen (!cfg.isMaster) { slave(TSerial(cfg.lineCfg)) }

    val sData    = slave(TCharStream(cfg.dataBits, cfg.canSendError))
    val mData    = master(TCharStream(cfg.dataBits, cfg.canReceiveError))

    val iPresent = in(Bool) default True
    val oPresent = out(Bool)

    val iBreak   =  in(Bool) default False
    val oBreak   = out(Bool)

    val oCarrier = ifGen(cfg.isMaster) { out(Bool) }
    val oRing    = ifGen(cfg.isMaster) { out(Bool) }
    val iCarrier = ifGen(!cfg.isMaster) { in(Bool) default False }
    val iRing    = ifGen(!cfg.isMaster) { in(Bool) default False }
  }


  // Presence Signals
  def fGetPresent()         : Bool = if (cfg.isMaster) { io.mSerial.dsr }      else { io.sSerial.dtr }
  def fSetPresent(b : Bool) : Unit = if (cfg.isMaster) { io.mSerial.dtr := b } else { io.sSerial.dsr := b }

  val sPresentOther = Bool
  val sPresentThis = Delay(io.iPresent, 1)
  val sPresent = sPresentThis & sPresentOther

  if (cfg.lineCfg.hasPresence) {
    fSetPresent(sPresentThis ^ Bool(cfg.lineCfg.negPresence))
    sPresentOther := BufferCC(fGetPresent()) ^ Bool(cfg.lineCfg.negPresence)
  } else {
    sPresentOther := True
  }


  // Modem signals
  if (cfg.isMaster) {
    if (cfg.lineCfg.hasModem) {
      io.oCarrier := BufferCC(io.mSerial.dcd) ^ Bool(cfg.lineCfg.negCarrier)
      io.oRing := BufferCC(io.mSerial.ri) ^ Bool(cfg.lineCfg.negRing)
    } else {
      io.oCarrier := False
      io.oRing := False
    }
  } else {
    if (cfg.lineCfg.hasModem) {
      io.sSerial.dcd := RegNext(io.iCarrier) ^ Bool(cfg.lineCfg.negCarrier)
      io.sSerial.ri := RegNext(io.iRing) ^ Bool(cfg.lineCfg.negRing)
    }
  }


  // Core UART
  def fGetLine()            : Bool = if (cfg.isMaster) { io.mSerial.rx }       else { io.sSerial.tx }
  def fSetLine(b : Bool)    : Unit = if (cfg.isMaster) { io.mSerial.tx := b }  else { io.sSerial.rx := b }
  def fGetAccept()          : Bool = if (cfg.isMaster) { io.mSerial.cts }      else { io.sSerial.rts }
  def fSetAccept(b : Bool)  : Unit = if (cfg.isMaster) { io.mSerial.rts := b } else { io.sSerial.cts := b }

  val iBaudGen = new StaticDivider(cfg.baudRate * (1 << cfg.baudFrac), cfg.baudFrac)

  val iReceiver = new Receiver(cfg.frameBits, cfg.baudFrac)
  iReceiver.io.iEnable := sPresent
  iReceiver.io.iBaudTick := iBaudGen.io.oTick
  iReceiver.io.iLine := BufferCC(fGetLine()) ^ Bool(cfg.lineCfg.negData)
  if (cfg.lineCfg.hasHandshake) {
    fSetAccept(iReceiver.io.oAccept ^ Bool(cfg.lineCfg.negHandshake))
  }
  io.oBreak := iReceiver.io.oBreak

  val iTransmitter = new Transmitter(cfg.frameBits, cfg.baudFrac)
  iTransmitter.io.iEnable := sPresent
  iTransmitter.io.iBaudTick := iBaudGen.io.oTick
  fSetLine(iTransmitter.io.oLine ^ Bool(cfg.lineCfg.negData))
  iTransmitter.io.iStopBits := iTransmitter.StopBits(cfg.secondStop)
  if (cfg.lineCfg.hasHandshake) {
    iTransmitter.io.iAccept := BufferCC(fGetAccept()) ^ Bool(cfg.lineCfg.negHandshake)
  }
  iTransmitter.io.iBreak := io.iBreak


  // Transmit Data Logic
  val rTrnChar = Reg(Bits(cfg.dataBits bits))
  val rTrnErr = Reg(Bool)
  val rTrnValid = Reg(Bool) init False

  io.sData.ready := iTransmitter.io.oReady
  when (iTransmitter.io.oReady) {
    rTrnChar := io.sData.char
    rTrnErr := io.sData.getEAny()
    rTrnValid := io.sData.valid
  }

  val sTrnChar = if (cfg.msbFirst) {
    rTrnChar.reversed
  } else {
    rTrnChar
  }

  val iTransmitLogic = if (cfg.hasParity) new Area {
    iTransmitter.io.iData(cfg.dataBits) := sTrnChar.xorR ^ Bool(cfg.oddParity) ^ !rTrnErr
    iTransmitter.io.iData(cfg.dataBits-1 downto 0) := sTrnChar
    iTransmitter.io.iValid := rTrnValid
  } else new Area {
    iTransmitter.io.iData := sTrnChar
    iTransmitter.io.iValid := rTrnValid & rTrnErr
  }


  // Receiver Logic
  val rRecChar = Reg(Bits(cfg.dataBits bits))
  val rRecEOvr = Reg(Bool)
  val rRecEFrm = Reg(Bool)
  val rRecEPar = Reg(Bool)
  val rRecValid = Reg(Bool) init False

  val sRecChar = if (cfg.msbFirst) {
    iReceiver.io.oData(cfg.dataBits-1 downto 0).reversed
  } else {
    iReceiver.io.oData(cfg.dataBits-1 downto 0)
  }
  val sRecEPar = Bool

  val iReceiveLogic = if (cfg.hasParity) {
    sRecEPar := iReceiver.io.oData.xorR ^ Bool(cfg.oddParity)
  } else {
    sRecEPar := False
  }

  iReceiver.io.iReady := io.mData.ready
  when (io.mData.ready) {
    rRecChar := sRecChar
    rRecEOvr := iReceiver.io.oOver
    rRecEFrm := iReceiver.io.oFrame
    rRecEPar := sRecEPar
    rRecValid := iReceiver.io.oValid
  }
  io.mData.char := rRecChar
  io.mData.setErr(rRecEOvr, rRecEFrm, rRecEPar)
  io.mData.valid := rRecValid
}

/*
class SerialSlave(val cfg : SerialConfig, val dividerWidth : Int) extends Component {

  val iReceiver = new SerialReceiver(dividerWidth)
  val iTransmitter = new SerialTransmitter(dividerWidth)

  val io = new Bundle {
    val sSerial = slave(TSerial(cfg))

    val iCfgDivide     = in(UInt(dividerWidth bits))
    val iCfgDataWidth  = in(UInt(3 bits)) default U(7) // 0 - 1 bit, 7 - 8 bit
    val iCfgMsbFirst   = in(Bool) default False
    val iCfgSecondStop = in(Bool) default False
    val iCfgHasParity  = in(Bool) default False
    val iCfgEvenParity = in(Bool) default False

    val iPresent = in(Bool) default True
    val oPresent = out(Bool)
    val iCarrier = in(Bool) default False
    val iRing = in(Bool) default False

    val mData = master(TAxiStream(iReceiver.dataCfg))
    val sData = slave(TAxiStream(iTransmitter.dataCfg))
  }

  val sEnable = Bool
  val sSerialTx = Bool
  val sSerialRx = Bool
  val sSerialCts = Bool
  val sSerialRts = Bool
  val sSerialDtr = Bool
  val sSerialDsr = Bool
  val sSerialDcd = Bool
  val sSerialRi = Bool


  io.mData << iReceiver.io.mData
  io.sData >> iTransmitter.io.sData

  iReceiver.io.iLine := sSerialTx
  sSerialCts := iReceiver.io.oReady
  iReceiver.io.iEnable := sEnable
  iReceiver.io.iCfgDivide     = io.iCfgDivide
  iReceiver.io.iCfgDataWidth  = io.iCfgDataWidth
  iReceiver.io.iCfgMsbFirst   = io.iCfgMsbFirst
  iReceiver.io.iCfgHasParity  = io.iCfgHasParity
  iReceiver.io.iCfgEvenParity = io.iCfgEvenParity

  sSerialRx := iTransmitter.io.oLine
  iTransmitter.io.iReady := sSerialRts
  iTransmitter.io.iEnable := sEnable
  iTransmitter.io.iCfgDivide     = io.iCfgDivide
  iTransmitter.io.iCfgDataWidth  = io.iCfgDataWidth
  iTransmitter.io.iCfgMsbFirst   = io.iCfgMsbFirst
  iTransmitter.io.iCfgSecondStop = io.iCfgSecondStop
  iTransmitter.io.iCfgHasParity  = io.iCfgHasParity
  iTransmitter.io.iCfgEvenParity = io.iCfgEvenParity


  sSerialDsr := io.iPresent
  io.oPresent := sSerialDtr
  sSerialDcd := io.iCarrier
  sSerialRi := io.iRing
  sEnable := sSerialDtr && sSerialDsr


  sSerialTx := Delay(io.sSerial.tx ^ Bool(cfg.negData), 2)
  io.sSerial.rx := sSerialRx ^ Bool(cfg.negData)

  if (cfg.hasHandshake) {
    sSerialRts := Delay(io.sSerial.rts ^ Bool(cfg.negHandshake), 2)
    io.sSerial.cts := sSerialCts ^ Bool(cfg.negHandshake)
  } else {
    sSerialRts := True
  }

  if (cfg.hasPresence) {
    sSerialDtr := Delay(io.sSerial.dtr ^ Bool(cfg.negPresence), 2)
    io.sSerial.dsr := sSerialDsr ^ Bool(cfg.negPresence)
  } else {
    sSerialDtr := True
  }

  if (cfg.hasModem) {
    io.sSerial.dcd := sSerialDcd ^ Bool(cfg.negCarrier)
    io.sSerial.ri := sSerialRi ^ Bool(cfg.negRing)
  }
}

*/
