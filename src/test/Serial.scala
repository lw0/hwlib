package hwlib.test

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._
import spinal.lib._

import hwlib.serial._
import hwlib.amba._
import hwlib.sim._
import hwlib.HwMain



class TestSerial extends Component {

  val lineCfg = SerialConfig(hasHandshake = true, hasPresence = true, hasModem = true, negPresence = true)
  val serialCfg = StaticTransceiverConfig(lineCfg,
    isMaster = false,
    baudRate = 921600 Hz,
    hasParity = true,
    oddParity = false)

  val io = new Bundle {
    val sSerial = slave(TSerial(lineCfg))
  }

  val rPresent = Reg(Bool) init True
  val rBreak   = Reg(Bool) init False
  val rCarrier = Reg(Bool) init False
  val rRing    = Reg(Bool) init False

  val iTransceiver = new StaticTransceiver(serialCfg)
  iTransceiver.io.sSerial << io.sSerial
  iTransceiver.io.sData << iTransceiver.io.mData
  iTransceiver.io.iPresent := rPresent
  iTransceiver.io.iBreak := rBreak
  iTransceiver.io.iCarrier := rCarrier
  iTransceiver.io.iRing := rRing

  when (iTransceiver.io.sData.ready && iTransceiver.io.mData.valid) {
    when (iTransceiver.io.mData.char === 0x30) {
      rPresent := !rPresent
    } elsewhen (iTransceiver.io.mData.char === 0x31) {
      rBreak := !rBreak
    } elsewhen (iTransceiver.io.mData.char === 0x32) {
      rCarrier := !rCarrier
    } elsewhen (iTransceiver.io.mData.char === 0x33) {
      rRing := !rRing
    }
  }

}

object TestSerial extends HwMain[TestSerial] {

  genWith(new TestSerial)
  postGen{ (args, report) =>
    report.printPruned()
  }

  simWith(new TestSerial)
  clockAt(100 MHz)
  simRun{ dut =>
    implicit val env = new ClockEnv(dut.clockDomain)

    val doneFlag = Future[Unit]()
    val iModel = new SerialModel(dut.io.sSerial, SerialSimConfig.from(dut.serialCfg))

    env.onRawCycle(32) { () =>
     env.setRst(false)
    }

    /*iModel.onGet() {
      (chr, id) =>
      val epar = if (chr.parityErr) { " !PAR" } else { "" }
      val efrm = if (chr.frameErr) { " !FRM" } else { "" }
      val eovr = if (chr.overflowErr) { " !OVR" } else { "" }
      println(s"Received: '${chr.char}'${epar}${efrm}${eovr}")
      if (chr.char == 'G') {
        doneFlag.resolve()
      }
      Take
    }

    env.waitFor(8)
    iModel.setPresent(true)

    env.waitFor(8)
    iModel.bput(SerialChar('A'))
    iModel.bput(SerialChar('B', frameErr=true))
    iModel.bput(SerialChar('C'))
    // iModel.bput(SerialChar('1'))
    iModel.bput(SerialChar('D'))
    // iModel.bput(SerialChar('1'))
    iModel.bput(SerialChar('2'))
    iModel.bput(SerialChar('E', parityErr=true))
    iModel.bput(SerialChar('F'))
    iModel.bput(SerialChar('3'))
    iModel.bput(SerialChar(0x00, frameErr=true)) // send break character
    iModel.bput(SerialChar('G'))
    iModel.bput(SerialChar('1')) // initiate break character receive
    iModel.bput(SerialChar('H'))

    doneFlag.blockValue()
    env.waitFor(64)*/
    val console = new SerialConsole(iModel)

    console.closed.blockValue()
    env.waitFor(10000)
  }
}
