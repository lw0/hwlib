package hwlib.sim

import scala.collection.mutable
import scala.util.Random
import scala.io.AnsiColor._
import scala.io.StdIn.readLine

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import hwlib.bundle.AutoBundle
import hwlib.serial.{TSerial, StaticTransceiverConfig}
import hwlib.sim.{ClockEnv, DelayQueue, DelayQueueConfig, GetAction}


case class SerialSimConfig(
  isMaster : Boolean,
  baudRate : HertzNumber,
  baudJitter : Double = 0.0,
  dataBits : Int = 8,
  msbFirst : Boolean = false,
  hasParity : Boolean = false,
  oddParity : Boolean = false,
  secondStop : Boolean = false,
  rxQueueCfg : DelayQueueConfig = DelayQueueConfig(),
  txQueueCfg : DelayQueueConfig = DelayQueueConfig()) {
  val baudPeriod = (baudRate.toTime / (1 ps)).toLong
  def baudPeriodJitter = if (baudJitter > 0) {
    (baudPeriod * (1.0 + Random.between(-baudJitter, baudJitter))).toLong
  } else {
    baudPeriod
  }
  val frameBits = 1 + dataBits + (if(hasParity){1}else{0}) + (if(secondStop){2}else{1})
  val framePeriod = baudPeriod * frameBits
}

object SerialSimConfig {
  def from(cfg : StaticTransceiverConfig,
           baudJitter : Double = 0.0,
           rxQueueCfg : DelayQueueConfig = DelayQueueConfig(),
           txQueueCfg : DelayQueueConfig = DelayQueueConfig(),
           flipRole : Boolean = true) =
      SerialSimConfig(
           isMaster = cfg.isMaster ^ flipRole,
           baudRate = cfg.baudRate,
           baudJitter = baudJitter,
           dataBits = cfg.dataBits,
           msbFirst = cfg.msbFirst,
           hasParity = cfg.hasParity,
           oddParity = cfg.oddParity,
           secondStop = cfg.secondStop,
           rxQueueCfg = rxQueueCfg,
           txQueueCfg = txQueueCfg)
}


case class SerialChar(
    char : Char,
    parityErr : Boolean = false,
    frameErr : Boolean = false,
    overflowErr : Boolean = false)


class SerialModel(line : TSerial, val cfg : SerialSimConfig)(implicit val env : ClockEnv) {

  val rxQueue = new DelayQueue[SerialChar](cfg.rxQueueCfg)
  env.dbg.addChild(this, rxQueue, "rq")

  val txQueue = new DelayQueue[SerialChar](cfg.txQueueCfg)
  env.dbg.addChild(this, txQueue, "tq")

  var isBreakActive = false
  val actionsBreak = mutable.ArrayBuffer[() => Unit]()
  val actionsBreakBegin = mutable.ArrayBuffer[() => Unit]()

  var shouldSendBreak = false
  var sendingBreak = false

  setData(true)
  setReady(false)

  fork {
    // receive ready
    while (true) {
      waitUntil(rxQueue.rawCanFit(2))
      setReady(true)
      waitUntil(!rxQueue.rawCanFit(2))
      setReady(false)
    }
  }
  fork {
    // receive data
    var overflow : Boolean = false
    while (true) {
      waitUntil(!isDataActive)
      env.dbg.printForTag(this, "RX", "  RX:START")
      sleep(cfg.baudPeriod / 2)

      if (!isDataActive) {
        sleep(cfg.baudPeriod)

        var allZeros = true

        var data : BigInt = 0
        for (i <- 0 until cfg.dataBits) {
          val bit = if (cfg.msbFirst) { cfg.dataBits - i - 1 } else { i }
          val bitValue = isDataActive
          env.dbg.printForTag(this, "RX", "  RX:DATA${i}=${if(bitValue){1}else{0}}")
          if (bitValue) {
            allZeros = false
            data = data.setBit(bit)
          }
          sleep(cfg.baudPeriod)
        }

        val parityErr = if (cfg.hasParity) {
          val parityBit = isDataActive
          if (parityBit) {
            allZeros = false
          }
          env.dbg.printForTag(this, "RX", "  RX:PARITY=${if(parityBit){1}else{0}}")
          sleep(cfg.baudPeriod)
          (data.bitCount % 2 == 1) ^ parityBit ^ cfg.oddParity
        } else {
          false
        }

        val frameErr = if (!isDataActive) {
          if (allZeros) {
            isBreakActive = true
            env.dbg.printForTag(this, "RX", "  RX:BBREAK")
            for (action <- actionsBreakBegin) {
              action()
            }
          } else {
            env.dbg.printForTag(this, "RX", "  RX:EFRAME")
          }
          waitUntil(isDataActive)
          true
        } else {
          env.dbg.printForTag(this, "RX", "  RX:STOP=${data}%02x('${data.charValue}%c')")
          false
        }

        if (frameErr && allZeros) {
          env.dbg.printForTag(this, "RX", "  RX:EBREAK")
          isBreakActive = false
          for (action <- actionsBreak) {
            action()
          }
        } else if (rxQueue.rawCanPut()) {
          rxQueue.rawPut(SerialChar((data & 0xff).toChar, parityErr, frameErr, overflow))
          overflow = false
        } else {
          overflow = true
        }
      } else {
        env.dbg.printForTag(this, "RX", "  RX:SPURIOUS")
      }
    }
  }

  fork {
    // transmit data
    while (true) {
      waitUntil((txQueue.rawCanGet() && isReadyActive) || shouldSendBreak)
      if (shouldSendBreak) {
        setData(false)
        sendingBreak = true
        sleep(cfg.framePeriod + 1)
        env.dbg.printForTag(this, "TX", "TX:  BRK_START")
        waitUntil(!shouldSendBreak)
        env.dbg.printForTag(this, "TX", "TX:  BRK_END")
      } else {
        txQueue.rawGet().map{
          chr =>
            env.dbg.printForTag(this, "TX", f"TX:  START=0x${chr.char}%02x('${chr.char}%c')")
            setData(false)
            sleep(cfg.baudPeriodJitter)
            for (i <- 0 until cfg.dataBits) {
              val bit = if (cfg.msbFirst) { cfg.dataBits - i - 1 } else { i }
              val bitValue = (chr.char & (1 << bit)) > 0
              env.dbg.printForTag(this, "TX", s"TX:  DATA${bit}=${if(bitValue){1}else{0}}")
              setData(bitValue)
              sleep(cfg.baudPeriodJitter)
            }

            if (cfg.hasParity) {
              val dataParity = (BigInt(chr.char).bitCount % 2 == 1) ^ cfg.oddParity ^ chr.parityErr
              env.dbg.printForTag(this, "TX", s"TX:  PARITY=${if(dataParity){1}else{0}}${if(chr.parityErr){" EPARINJECT"}else{""}}")
              setData(dataParity)
              sleep(cfg.baudPeriodJitter)
            }

            if (chr.frameErr) {
              env.dbg.printForTag(this, "TX", s"TX:  EFRMINJECT")
              setData(false)
              sleep(cfg.baudPeriodJitter)
            }
            env.dbg.printForTag(this, "TX", s"TX:  STOP")
            setData(true)
            sleep(cfg.baudPeriodJitter)
            if (cfg.secondStop) {
              env.dbg.printForTag(this, "TX", s"TX:  STOP")
              sleep(cfg.baudPeriodJitter)
            }
        }
      }
    }
  }

  def setData(active : Boolean) : Unit = {
    if (cfg.isMaster) {
      line.tx #= active ^ line.cfg.negData
    } else {
      line.rx #= active ^ line.cfg.negData
    }
  }
  def isDataActive = if (cfg.isMaster) {
    line.rx.toBoolean ^ line.cfg.negData
  } else {
    line.tx.toBoolean ^ line.cfg.negData
  }

  def setReady(active : Boolean) : Unit = {
    if (line.cfg.hasHandshake) {
      if (cfg.isMaster) {
        line.rts #= active ^ line.cfg.negHandshake
      } else {
        line.cts #= active ^ line.cfg.negHandshake
      }
    }
  }
  def isReadyActive = if (line.cfg.hasHandshake) {
    if (cfg.isMaster) { line.cts.toBoolean } else { line.rts.toBoolean } ^ line.cfg.negHandshake
  } else {
    true
  }


  // aux signal handling
  env.onRst{
    () =>
      if (cfg.isMaster && line.cfg.hasPresence) {
        line.dtr #= line.cfg.negPresence
      }
      if (!cfg.isMaster && line.cfg.hasPresence) {
        line.dsr #= line.cfg.negPresence
      }
      if (!cfg.isMaster && line.cfg.hasModem) {
        line.dcd #= line.cfg.negCarrier
        line.ri #= line.cfg.negRing
      }
  }

  def onBreak(action : () => Unit) = {
    actionsBreak.append(action)
  }

  def onBreakBegin(action : () => Unit) = {
    actionsBreakBegin.append(action)
  }

  def sendBreak(active : Boolean) = {
    shouldSendBreak = active
  }

  def setPresent(present : Boolean) : Unit = {
    // env.dbg.print(s"setPresent($present)")
    if (cfg.isMaster) {
      line.dtr #= present ^ line.cfg.negPresence
    } else {
      line.dsr #= present ^ line.cfg.negPresence
    }
  }
  def isPresent = if (line.cfg.hasPresence) {
    if (cfg.isMaster) { line.dsr.toBoolean } else { line.dtr.toBoolean } ^ line.cfg.negPresence
  } else {
    true
  }

  def setCarrier(active : Boolean) : Unit = {
    if (line.cfg.hasModem && !cfg.isMaster) {
      line.dcd #= active ^ line.cfg.negCarrier
    }
  }
  def isCarrierActive = if (line.cfg.hasModem) {
    line.dcd.toBoolean ^ line.cfg.negCarrier
  } else {
    false
  }

  def setRing(active : Boolean) : Unit = {
    if (line.cfg.hasModem && !cfg.isMaster) {
      line.ri #= active ^ line.cfg.negRing
    }
  }
  def isRingActive = if (line.cfg.hasModem) {
    line.ri.toBoolean ^ line.cfg.negRing
  } else {
    false
  }

  // queue apis
  def put(item : SerialChar, id : Long = 0) = txQueue.put(item, id)
  def bput(item : SerialChar, id : Long = 0) = txQueue.bput(item, id)
  def onPut(call : () => Option[(SerialChar, Long)]) = txQueue.onPut(call)
  def onPutDefault() : Unit = txQueue.onPutDefault()

  def get(id : Option[Long] = None) = rxQueue.get(id)
  def bget(id : Option[Long] = None) = rxQueue.bget(id)
  def onGet(id : Option[Long] = None)(call : (SerialChar, Long) => GetAction) = rxQueue.onGet(id)(call)
  def onGetDefault(id : Option[Long] = None) = rxQueue.onGetDefault(id)
}

class SerialConsole(model : SerialModel, slowRate : Int = 10, slowCap : Int = 1000, breakRate : Int = 100, breakCR : Boolean = false, breakLF : Boolean = true, updateCharTable : Map[Char, String] = Map[Char, String]())(implicit val env : ClockEnv) {

  val CharTable : Map[Char, String] = 
    Map.from((-128 to 31).map(c => c.toChar -> f"${YELLOW}${BOLD}%s\\x${c}%02x")) ++
    Map(0.toChar -> s"${YELLOW}${BOLD}\\0",
        '\b' -> s"${YELLOW}${BOLD}\\b",
        '\f' -> s"${YELLOW}${BOLD}\\f\f",
        '\r' -> s"${YELLOW}${BOLD}\\r${if(breakCR){"\n"}else{""}}",
        '\n' -> s"${YELLOW}${BOLD}\\n${if(breakLF){"\n"}else{""}}",
        '\t' -> s"${YELLOW}${BOLD}\\t\t")  ++
    updateCharTable

  val closed = Future[Unit]()

  var delay : Int = 0
  var halt : Boolean = false

  val lastCol = 0
  model.onGet() { (sch, id) =>
    val outStr = CharTable.get(sch.char).getOrElse(f"${YELLOW}${sch.char}%c")
    val eparStr = if (sch.parityErr) {s"${YELLOW}${REVERSED}P"} else {""}
    val efrmStr = if (sch.frameErr) {s"${YELLOW}${REVERSED}F"} else {""}
    val eovrStr = if (sch.overflowErr) {s"${YELLOW}${REVERSED}O"} else {""}
    System.out.write(s"${outStr}${eparStr}${efrmStr}${eovrStr}${RESET}${BLUE}" getBytes("UTF-8"))
    System.out.flush()
    delay = 0
    Take
  }

  fork {
    env.waitFor()
    model.setPresent(true)
    println(s"${RESET}${BOLD}Serial Console${RESET}${WHITE} (${BLUE}Input${WHITE}|${YELLOW}Output${WHITE})")
    println(s"^Af - send next character with frame error")
    println(s"^Ap - send next character with parity error")
    println(s"^Ab - send a single break character")
    println(s"^AB - send continous break character until next input")
    println(s"^Ah - halt or continue simulation while waiting for input${BLUE}")
    var repeating = true
    var escaping = false
    var nextEFrm = false
    var nextEPar = false
    var breakActive = false
    var dropNewline = false
    while (repeating) {
      while (System.in.available() == 0) {
        if (!halt) {
          sleep(model.cfg.framePeriod)
        }
        if (breakActive) {
          Thread.sleep(breakRate)
          println(s"breaking ${halt} ${breakRate}")
        } else if (delay > 0) {
          Thread.sleep(delay)
          println(s"waiting ${halt} ${delay}")
        }
        delay = (delay + slowRate) min slowCap
      }
      delay = 0
      var char = System.in.read()
      println(s"got char ${char}")
      var send = true
      if (char < 0 || (char == 24 && !escaping)) {
        send = false
        repeating = false
      } else if (char == 1 && !escaping) {
        send = false
        escaping = true
      } else if (char != 1 && escaping) {
        send = false
        if (char == 'f') {
          nextEFrm = true
        } else if (char == 'p') {
          nextEPar = true
        } else if (char == 'b') {
          dropNewline = true
          model.sendBreak(true)
          System.out.write(s"${RESET}${WHITE}<BRK" getBytes("UTF-8"))
          System.out.flush()
          waitUntil(model.sendingBreak)
          model.sendBreak(false)
          sleep(model.cfg.framePeriod)
          System.out.write(s"${RESET}${WHITE}>${BLUE}" getBytes("UTF-8"))
          System.out.flush()
        } else if (char == 'B') {
          dropNewline = true
          breakActive = true
          model.sendBreak(true)
          waitUntil(model.sendingBreak)
          System.out.write(s"${RESET}${WHITE}<BRK${BLUE}" getBytes("UTF-8"))
          System.out.flush()
        } else if (char == 'h') {
          dropNewline = true
          halt = !halt
          if (halt) {
            System.out.write(s"${RESET}${WHITE}<HALT>${BLUE}" getBytes("UTF-8"))
          } else {
            System.out.write(s"${RESET}${WHITE}<CONT>${BLUE}" getBytes("UTF-8"))
          }
          System.out.flush()
        } else {
          send = true
        }
        escaping = false
      } else if (dropNewline && char == '\n') {
        dropNewline = false
        send = false
      }
      if (send) {
        if (breakActive) {
          breakActive = false
          model.sendBreak(false)
          sleep(model.cfg.framePeriod)
          System.out.write(s"${RESET}${WHITE}>${BLUE}" getBytes("UTF-8"))
          System.out.flush()
        }
        model.bput(SerialChar(char.toChar, frameErr = nextEFrm, parityErr = nextEPar))
        nextEFrm = false
        nextEPar = false
      }
    }
    println(s"${RESET}${BOLD}Serial Console${RESET} End Of Input")
    closed.resolve()
  }

}
