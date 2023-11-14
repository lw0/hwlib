package hwlib.xilinx.blackbox

import scala.collection.mutable

import spinal.core._


abstract class OptionValue {
  def value : String
}

abstract class ClkManBandwidth extends OptionValue
object Optimized extends ClkManBandwidth {
  def value = "OPTIMIZED"
}
object High extends ClkManBandwidth {
  def value = "HIGH"
}
object Low extends ClkManBandwidth {
  def value = "LOW"
}

abstract class ClkManCompensation extends OptionValue
object ZHold extends ClkManCompensation {
  def value = "ZHOLD"
}
object BufIn extends ClkManCompensation {
  def value = "BUF_IN"
}
object External extends ClkManCompensation {
  def value = "EXTERNAL"
}
object Internal extends ClkManCompensation {
  def value = "INTERNAL"
}

abstract class ClkManSpread extends OptionValue
object CenterHigh extends ClkManSpread {
  def value = "CENTER_HIGH"
}
object CenterLow extends ClkManSpread {
  def value = "CENTER_LOW"
}
object DownHigh extends ClkManSpread {
  def value = "DOWN_HIGH"
}
object DownLow extends ClkManSpread {
  def value = "DOWN_LOW"
}

abstract class ClkManKind extends OptionValue
object Pll2Base extends ClkManKind {
  def value = "PLLE2_BASE"
}
object Pll2Adv extends ClkManKind {
  def value = "PLLE2_ADV"
}
object Mmcm2Base extends ClkManKind {
  def value = "MMCME2_BASE"
}
object Mmcm2Adv extends ClkManKind {
  def value = "MMCME2_ADV"
}


case class ClkManCfg (
  // Input clock period in <ns>.<ps> resolution (i.e. 33.333 is 30 MHz).
  ClkIn1Freq : HertzNumber = 100 MHz,
  ClkIn2Freq : HertzNumber = 100 MHz,
  // Master division value (1-106)
  ClkDiv : Double = 1.0,
  // Multiply value for all CLKOUT (2.000-64.000).
  ClkMul : Double = 5.0,
  // Phase offset in degrees of CLKFB (-360.000-360.000).
  ClkPhase : Double = 0.0,
  // Divide amount for each CLKOUT (1-128)
  ClkOutDivs : Seq[Double] = Seq(),
  // Duty cycle for each CLKOUT (0.01-0.99).
  ClkOutDuties : Seq[Double] = Seq(),
  // Phase offset for each CLKOUT (-360.000-360.000).
  ClkOutPhases : Seq[Double] = Seq(),
  // Cascade CLKOUT4 counter with CLKOUT6 (FALSE, TRUE)
  ClkOut4Cascade : Boolean = false,
  // Delays DONE until MMCM is locked (FALSE, TRUE)
  StartupWait : Boolean = false,
  // Compensation
  Compensation : ClkManCompensation = ZHold,
  // Jitter programming (OPTIMIZED, HIGH, LOW)
  Bandwidth : ClkManBandwidth = Optimized,
  // Reference input jitter in UI (0.000-0.999).
  ClkIn1Jitter : Double = 0.0,
  ClkIn2Jitter : Double = 0.0,
  // Spread Spectrum:
  SpreadEnable : Boolean = false,
  SpreadMode : ClkManSpread = CenterHigh,
  // Spread Spectrum modulation period (ns)
  SpreadModPeriod : TimeNumber = 10 us,
  // Fine phase shift enable (TRUE/FALSE)
  ClkFinePhShift : Boolean = false,
  ClkOutFinePhShifts : Seq[Boolean] = Seq()) {

  def ClkDivInt : Int = {
    val intVal = ClkDiv.toInt
    if (intVal != ClkDiv) {
      SpinalWarning(s"Truncating ClkDiv = ${ClkDiv} to ${intVal}")
    }
    intVal
  }

  def ClkMulInt : Int = {
    val intVal = ClkMul.toInt
    if (intVal != ClkMul) {
      SpinalWarning(s"Truncating ClkMul = ${ClkMul} to ${intVal}")
    }
    intVal
  }

  def ClkOutDiv(idx : Int) : Double = ClkOutDivs.lift(idx).getOrElse(1.0)
  def ClkOutDivInt(idx : Int) : Int = {
    val fracVal = ClkOutDiv(idx)
    val intVal = fracVal.toInt
    if (intVal != fracVal) {
      SpinalWarning(s"Truncating ClkOutDiv(${idx}) = ${fracVal} to ${intVal}")
    }
    intVal
  }

  def ClkOutDuty(idx : Int) : Double = ClkOutDuties.lift(idx).getOrElse(0.5)
  def ClkOutPhase(idx : Int) : Double = ClkOutPhases.lift(idx).getOrElse(0.0)
  def ClkOutFinePhShift(idx : Int) : Boolean = ClkOutFinePhShifts.lift(idx).getOrElse(false)

  def ClkOut4CascadeStr : String = if (ClkOut4Cascade) { "TRUE" } else { "FALSE" }
  def StartupWaitStr : String = if (StartupWait) { "TRUE" } else { "FALSE" }
  def SpreadEnableStr : String = if (SpreadEnable) { "TRUE" } else { "FALSE" }
  def ClkFinePhShiftStr : String = if (ClkFinePhShift) { "TRUE" } else { "FALSE" }
  def ClkOutFinePhShiftStr(idx : Int) : String = if (ClkOutFinePhShift(idx)) { "TRUE" } else { "FALSE" }

}

class ClkManPrimitive(kind : ClkManKind, cfg : ClkManCfg) extends BlackBox {

  if (globalData.config.mode == VHDL) {
    librariesUsages += "UNISIM"
    setDefinitionName(s"vcomponents.${kind.value}")
  } else {
    setDefinitionName(kind.value)
  }

  val isMmcm = Seq(Mmcm2Base, Mmcm2Adv).contains(kind)
  val isMmcmAdv = Seq(Mmcm2Adv).contains(kind)
  val isAdv = Seq(Mmcm2Adv, Pll2Adv).contains(kind)

  addGeneric("CLKIN1_PERIOD", (           cfg.ClkIn1Freq.toTime / (1 ns)).toDouble)
  addGeneric("REF_JITTER1",               cfg.ClkIn1Jitter)
  addGeneric("BANDWIDTH",                 cfg.Bandwidth.value)
    addGeneric("STARTUP_WAIT",            if (cfg.StartupWait) { "TRUE" } else { "FALSE" })
  if (isAdv) {
    addGeneric("COMPENSATION",            cfg.Compensation.value)
    addGeneric("CLKIN2_PERIOD", (         cfg.ClkIn2Freq.toTime / (1 ns)).toDouble)
    addGeneric("REF_JITTER2",             cfg.ClkIn2Jitter)
  }
  addGeneric("DIVCLK_DIVIDE",             cfg.ClkDivInt)
  addGeneric("CLKFBOUT_PHASE",            cfg.ClkPhase)
  if (isMmcm) {
    addGeneric("CLKFBOUT_MULT_F",         cfg.ClkMul)
    addGeneric("CLKOUT0_DIVIDE_F",        cfg.ClkOutDiv(0))
    addGeneric("CLKOUT6_DIVIDE",          cfg.ClkOutDivInt(6))
    addGeneric("CLKOUT6_DUTY_CYCLE",      cfg.ClkOutDuty(6))
    addGeneric("CLKOUT6_PHASE",           cfg.ClkOutPhase(6))
    addGeneric("CLKOUT4_CASCADE",         cfg.ClkOut4CascadeStr)
  } else {
    addGeneric("CLKFBOUT_MULT",           cfg.ClkMulInt)
    addGeneric("CLKOUT0_DIVIDE",          cfg.ClkOutDivInt(0))
  }
  addGeneric("CLKOUT1_DIVIDE",            cfg.ClkOutDivInt(1))
  addGeneric("CLKOUT2_DIVIDE",            cfg.ClkOutDivInt(2))
  addGeneric("CLKOUT3_DIVIDE",            cfg.ClkOutDivInt(3))
  addGeneric("CLKOUT4_DIVIDE",            cfg.ClkOutDivInt(4))
  addGeneric("CLKOUT5_DIVIDE",            cfg.ClkOutDivInt(5))
  addGeneric("CLKOUT0_DUTY_CYCLE",        cfg.ClkOutDuty(0))
  addGeneric("CLKOUT1_DUTY_CYCLE",        cfg.ClkOutDuty(1))
  addGeneric("CLKOUT2_DUTY_CYCLE",        cfg.ClkOutDuty(2))
  addGeneric("CLKOUT3_DUTY_CYCLE",        cfg.ClkOutDuty(3))
  addGeneric("CLKOUT4_DUTY_CYCLE",        cfg.ClkOutDuty(4))
  addGeneric("CLKOUT5_DUTY_CYCLE",        cfg.ClkOutDuty(5))
  addGeneric("CLKOUT0_PHASE",             cfg.ClkOutPhase(0))
  addGeneric("CLKOUT1_PHASE",             cfg.ClkOutPhase(1))
  addGeneric("CLKOUT2_PHASE",             cfg.ClkOutPhase(2))
  addGeneric("CLKOUT3_PHASE",             cfg.ClkOutPhase(3))
  addGeneric("CLKOUT4_PHASE",             cfg.ClkOutPhase(4))
  addGeneric("CLKOUT5_PHASE",             cfg.ClkOutPhase(5))
  if (isMmcmAdv) {
    addGeneric("SS_EN",                   cfg.SpreadEnableStr)
    addGeneric("SS_MODE",                 cfg.SpreadMode.value)
    addGeneric("SS_MOD_PERIOD",           (cfg.SpreadModPeriod / (1 ns)).toDouble)
    addGeneric("CLKFBOUT_USE_FINE_PS",    cfg.ClkFinePhShiftStr)
    addGeneric("CLKOUT0_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(0))
    addGeneric("CLKOUT1_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(1))
    addGeneric("CLKOUT2_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(2))
    addGeneric("CLKOUT3_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(3))
    addGeneric("CLKOUT4_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(4))
    addGeneric("CLKOUT5_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(5))
    addGeneric("CLKOUT6_USE_FINE_PS",     cfg.ClkOutFinePhShiftStr(6))
  }

  val io = new Bundle {
    val CLKIN1       =                     in(Bool)
    val CLKIN2       = ifGen(isAdv) {      in(Bool)          default False }
    val CLKINSEL     = ifGen(isAdv) {      in(Bool)          default True }
    val CLKFBIN      =                     in(Bool)
    val PWRDWN       =                     in(Bool)          default False
    val RST          =                     in(Bool)          default False
    val LOCKED       =                    out(Bool)
    val CLKFBSTOPPED = ifGen(isMmcmAdv) { out(Bool) }
    val CLKINSTOPPED = ifGen(isMmcmAdv) { out(Bool) }

    val CLKFBOUT     =                    out(Bool)
    val CLKOUT0      =                    out(Bool)
    val CLKOUT1      =                    out(Bool)
    val CLKOUT2      =                    out(Bool)
    val CLKOUT3      =                    out(Bool)
    val CLKOUT4      =                    out(Bool)
    val CLKOUT5      =                    out(Bool)
    val CLKFBOUTB    = ifGen(isMmcm) {    out(Bool) }
    val CLKOUT6      = ifGen(isMmcm) {    out(Bool) }
    val CLKOUT0B     = ifGen(isMmcm) {    out(Bool) }
    val CLKOUT1B     = ifGen(isMmcm) {    out(Bool) }
    val CLKOUT2B     = ifGen(isMmcm) {    out(Bool) }
    val CLKOUT3B     = ifGen(isMmcm) {    out(Bool) }

    val DCLK         = ifGen(isAdv) {      in(Bool)          default False }
    val DADDR        = ifGen(isAdv) {      in(UInt(7 bits))  default U(0, 7 bits) }
    val DI           = ifGen(isAdv) {      in(Bits(16 bits)) default B(0, 16 bits) }
    val DWE          = ifGen(isAdv) {      in(Bool)          default False }
    val DEN          = ifGen(isAdv) {      in(Bool)          default False }
    val DO           = ifGen(isAdv) {     out(Bits(16 bits)) }
    val DRDY         = ifGen(isAdv) {     out(Bool) }

    val PSCLK        = ifGen(isMmcmAdv) {  in(Bool)          default False }
    val PSEN         = ifGen(isMmcmAdv) {  in(Bool)          default False }
    val PSINCDEC     = ifGen(isMmcmAdv) {  in(Bool)          default False }
    val PSDONE       = ifGen(isMmcmAdv) { out(Bool) }
  }

  noIoPrefix()
  addTag(noNumericType)
}


case class ClockSpec(val freq : HertzNumber, phase : Double = 0.0, duty : Double = 0.5)


class ClockOptTarget
object FreqErrorMean extends ClockOptTarget
object FreqErrorMax extends ClockOptTarget
case class FreqError(name : String) extends ClockOptTarget
object PhaseErrorMean extends ClockOptTarget
object PhaseErrorMax extends ClockOptTarget
case class PhaseError(name : String) extends ClockOptTarget
object DutyErrorMean extends ClockOptTarget
object DutyErrorMax extends ClockOptTarget
case class DutyError(name : String) extends ClockOptTarget


case class FractRange(min : Double, max : Double, unit : Double) {
  val iMin = Math.ceil(min / unit).toInt
  val iMax = Math.floor(max / unit).toInt

  def iter(rev : Boolean = false) = if (rev) {
    for (i <- iMax to iMin by -1) yield i * unit
  } else {
    for (i <- iMin to iMax by  1) yield i * unit
  }
}


case class ClkManBounds(fInMin  : HertzNumber, fInMax  : HertzNumber,
                        fPfdMin : HertzNumber, fPfdMax : HertzNumber,
                        fVcoMin : HertzNumber, fVcoMax : HertzNumber,
                        rDmin   : Double = 1.0, rDmax : Double = 106.0, rDunit : Double = 1.0,
                        rMmin   : Double = 2.0, rMmax : Double = 64.0, rMunit : Double = 0.125,
                        rOmin   : Double = 1.0, rOmax : Double = 128.0, rOunit : Double = 1.0,
                        rFmin   : Double = 2.0, rFmax : Double = 128.0, rFunit : Double = 0.125,
                        rPmin   : Double = 0.0, rPmax : Double = 64.0, rPunit : Double = 0.125,
                        rHTmin  : Double = 1.0, rLTmin : Double = 0.5, rXTunit : Double = 0.5,
                        nOut : Int = 7, enFract : Boolean = true) {

  def genVCO(fIn : HertzNumber) = {
    val rangeD = FractRange((fIn / fPfdMax).toDouble max rDmin, (fIn / fPfdMin).toDouble min rDmax, rDunit)
    val rangeM = FractRange((fVcoMin / fIn * rangeD.min).toDouble max rMmin, (fVcoMax / fIn * rangeD.max).toDouble min rMmax, rMunit)
    val rMin = fVcoMin / fIn
    val rMax = fVcoMax / fIn
    val factors = mutable.Set[Double]()
    val fInValid = fIn.toDouble >= fInMin.toDouble && fIn.toDouble <= fInMax.toDouble
    for (d <- rangeD.iter(rev=false); m <- rangeM.iter(rev=true) if fInValid if (m / d) <= rMax if (m / d) >= rMin if !factors.contains(m / d)) yield {
      factors += m / d
      (fIn * m / d, d, m)
    }
  }

  case class OutParams(name : String, fVCO : HertzNumber, spec : ClockSpec) {
    val ratioIdeal = (fVCO / spec.freq).toDouble

    val ratioInt = Math.round(ratioIdeal / rOunit) * rOunit min rOmax max rOmin
    val freqInt = fVCO / ratioInt
    val freqErrInt = ((freqInt - spec.freq) / spec.freq).toDouble

    val delayIdealInt = (1.0 + (spec.phase % 1.0)) % 1.0 * ratioInt
    val delayInt = Math.round(delayIdealInt / rPunit) * rPunit min rPmax max rPmin
    val phaseInt = delayInt / ratioInt
    val phaseErrInt = phaseInt - spec.phase

    val highIdealInt = ratioInt * spec.duty
    val highInt = if (ratioInt == 1.0) {
      // counters turned off, original signal
      0.5
    } else {
      Math.round(highIdealInt / rXTunit) * rXTunit min (ratioInt - rLTmin) max rHTmin
    }
    val dutyInt = highInt / ratioInt
    val dutyErrInt = dutyInt - spec.duty

    val ratioFract = Math.round(ratioIdeal / rFunit) * rFunit min rFmax max rFmin
    val freqFract = fVCO / ratioFract
    val freqErrFract = ((freqFract - spec.freq) / spec.freq).toDouble

    val delayIdealFract = (1.0 + (spec.phase % 1.0)) % 1.0 * ratioFract
    val delayFract = Math.round(delayIdealFract / rPunit) * rPunit min rPmax max rPmin
    val phaseFract = delayFract / ratioFract
    val phaseErrFract = phaseFract - spec.phase

    // fractional counters only support 50%
    val highFract = Math.round(ratioFract / 2.0 / rFunit) * rFunit
    val dutyFract = highFract / ratioFract
    val dutyErrFract = dutyFract - spec.duty

    val isFractDifferent = enFract && (ratioInt != ratioFract)

    def ratio(isFract : Boolean = false)    = if (isFract) { ratioFract }    else { ratioInt }
    def freq(isFract : Boolean = false)     = if (isFract) { freqFract }     else { freqInt }
    def freqErr(isFract : Boolean = false)  = if (isFract) { freqErrFract }  else { freqErrInt }
    def phase(isFract : Boolean = false)    = if (isFract) { phaseFract }    else { phaseInt }
    def phaseErr(isFract : Boolean = false) = if (isFract) { phaseErrFract } else { phaseErrInt }
    def duty(isFract : Boolean = false)     = if (isFract) { dutyFract }     else { dutyInt }
    def dutyErr(isFract : Boolean = false)  = if (isFract) { dutyErrFract }  else { dutyErrInt }
  }

  def genOptions(freqVCO : HertzNumber, clocks : Map[String, ClockSpec], targets : Seq[ClockOptTarget]) = {
    val option = clocks.map(e => e._1 -> OutParams(e._1, freqVCO, e._2))
    for (key <- option.keys.map(Some(_)) ++ Seq(None) if key.map(option(_).isFractDifferent).getOrElse(true)) yield {
      val errors = option.map{e =>
        val isFract = key.map(_ == e._1).getOrElse(false)
        e._1 -> (e._2.freqErr(isFract), e._2.phaseErr(isFract), e._2.dutyErr(isFract))
      }
      val maxFreqErr = errors.map(e => Math.abs(e._2._1)).max
      val meanFreqErr = errors.map(e => Math.abs(e._2._1)).sum / errors.size
      val maxPhaseErr = errors.map(e => Math.abs(e._2._2)).max
      val meanPhaseErr = errors.map(e => Math.abs(e._2._2)).sum / errors.size
      val maxDutyErr = errors.map(e => Math.abs(e._2._3)).max
      val meanDutyErr = errors.map(e => Math.abs(e._2._3)).sum / errors.size
      val error = (targets).map{
        case FreqErrorMax => maxFreqErr
        case FreqErrorMean => meanFreqErr
        case FreqError(name) => errors.get(name).map(e => Math.abs(e._1)).getOrElse(0.0)
        case PhaseErrorMax => maxPhaseErr
        case PhaseErrorMean => meanPhaseErr
        case PhaseError(name) => errors.get(name).map(e => Math.abs(e._2)).getOrElse(0.0)
        case DutyErrorMax => maxDutyErr
        case DutyErrorMean => meanDutyErr
        case DutyError(name) => errors.get(name).map(e => Math.abs(e._3)).getOrElse(0.0)
      }
      (key, option, error)
    }
  }

  case class Params(freqIn : HertzNumber, div : Double, mul : Double, freqVCO : HertzNumber, fractOutput : Option[String], outputs : Map[String, OutParams]) {

    val order = fractOutput.map(name => Seq(outputs(name))).getOrElse(Seq[OutParams]()) ++ fractOutput.map(name => (outputs.keys.filterNot(_ == name))).getOrElse(outputs.keys).map(outputs(_)).toSeq

    val outDivs = order.map(param => param.ratio(fractOutput.map(_ == param.name).getOrElse(false)))
    val outPhases = order.map(param => param.phase(fractOutput.map(_ == param.name).getOrElse(false)))
    val outDuties = order.map(param => param.duty(fractOutput.map(_ == param.name).getOrElse(false)))

    val mapping = Map.from((0 until order.length).map(idx => order(idx).name -> idx))

    val canFit = order.length <= nOut

    def desc = {
      val optionStr = outputs.map{p =>
        val isFract = fractOutput.map(_ == p._1).getOrElse(false)
        val ratio = p._2.ratio(isFract)
        val freq = p._2.freq(isFract).decompose
        val freqErr = p._2.freqErr(isFract)
        val phase = p._2.phase(isFract)
        val phaseErr = p._2.phaseErr(isFract)
        val duty = p._2.duty(isFract)
        val dutyErr = p._2.dutyErr(isFract)
        f"${"'"+p._1+"'"}%10s:  ${if (isFract) {"F"} else {"O"}}=${ratio}%7.3f   ${freq._1}%7.3f ${freq._2}%3s (${freqErr*100.0}%5.1f%%)   ${phase * 360.0}%5.1f deg (${phaseErr*100.0}%5.1f%%)   ${duty*100.0}%5.1f%% (${dutyErr*100.0}%5.1f%%)"
      }.mkString("\n")
      val fIn = freqIn.decompose
      val fVCO = freqVCO.decompose
      f"fInput=${fIn._1}%7.3f ${fIn._2}%3s  fVco=${fVCO._1}%7.3f ${fVCO._2}%3s  D=${div}%7.3f  M=${mul}%6.3f\n${optionStr}"
    }

    def getCfg(from : ClkManCfg) = from.copy(
      ClkIn1Freq = freqIn,
      ClkDiv = div,
      ClkMul = mul,
      ClkOutDivs = outDivs,
      ClkOutDuties = outDuties,
      ClkOutPhases = outPhases.map(_ * 360.0))
  }

  def optimize(fIn : HertzNumber, clocks : Map[String, ClockSpec], targets : Seq[ClockOptTarget] = Seq()) = {
    var mayCurError : Option[Seq[Double]] = None
    var mayBest : Option[Params] = None
    val optTargets = targets ++ Seq(FreqErrorMax, PhaseErrorMax, DutyErrorMax)
    for ((fVCO, d, m) <- genVCO(fIn); (key, option, error) <- genOptions(fVCO, clocks, optTargets)) {
      val better = mayCurError match {
        case Some(curError) => curError.zip(error).find(e => e._1 != e._2).map(e => e._1 > e._2).getOrElse(false)
        case None => true
      }
      if (better) {
        mayCurError = Some(error)
        val better = Params(fIn, d, m, fVCO, key, option)
        // println("---Better---")
        // println(better.desc)
        mayBest = Some(better)
      }
    }
    mayBest
  }
}


abstract class ClkBufKind {
  def name : String
}
object ClkBufGlobal extends ClkBufKind {
  def name = "BUFG" // Primitive: Global Clock Simple Buffer
  // I, O
}
object ClkBufGlobalEn0 extends ClkBufKind {
  def name = "BUFGCE" // Primitive: Global Clock Buffer with Clock Enable
  // I, O, CE
}
object ClkBufGlobalEn1 extends ClkBufKind {
  def name = "BUFGCE_1" // Primitive: Global Clock Buffer with Clock Enable and Output State 1
  // I, O, CE
}
object ClkBufGlobalMux0 extends ClkBufKind {
  def name = "BUFGMUX" // Primitive: Global Clock Mux Buffer
  // I0, I1, S, O
}
object ClkBufGlobalMux1 extends ClkBufKind {
  def name = "BUFGMUX_1" // Primitive: Global Clock Mux Buffer with Output State 1
  // I0, I1, S, O
}
object ClkBufGlobalMuxCtrl extends ClkBufKind {
  def name = "BUFGMUX_CTRL" // Primitive: 2-to-1 Global Clock MUX Buffer
  // I0, I1, S, O
}
object ClkBufHRow extends ClkBufKind {
  def name = "BUFH" // Primitive: HROW Clock Buffer for a Single Clocking Region
  // I, O
}
object ClkBufHRowEn extends ClkBufKind {
  def name = "BUFHCE" // Primitive: HROW Clock Buffer for a Single Clocking Region with Clock Enable
  // I, CE, O
  // CE_TYPE, INIT_OUT
}
object ClkBufInOut extends ClkBufKind {
  def name = "BUFIO" // Primitive: Local Clock Buffer for I/O
  // I, O
}
object ClkBufMulti extends ClkBufKind {
  def name = "BUFMR" // Primitive: Multi-Region Clock Buffer
  // I, O
}
object ClkBufMultiEn extends ClkBufKind {
  def name = "BUFMRCE" // Primitive: Multi-Region Clock Buffer with Clock Enable
  // I, O, CE
  // CE_TYPE, INIT_OUT
}
object ClkBufRegion extends ClkBufKind {
  def name = "BUFR" // Primitive: Regional Clock Buffer for I/O and Logic Resources within a Clock Region
  // I, O, CE, CLR
  // BUFR_DIVIDE, SIM_DEVICE
}

class ClkBufPrimitive(kind : ClkBufKind) extends BlackBox {

  if (globalData.config.mode == VHDL) {
    librariesUsages += "UNISIM"
    setDefinitionName(s"vcomponents.${kind.name}")
  } else {
    setDefinitionName(kind.name)
  }

  val isMux = Set(ClkBufGlobalMux0, ClkBufGlobalMux1, ClkBufGlobalMuxCtrl).contains(kind)
  val hasCE = Set(ClkBufGlobalEn0, ClkBufGlobalEn1, ClkBufHRowEn, ClkBufMultiEn, ClkBufRegion).contains(kind)
  val isRegion = ClkBufRegion == kind

  val io = new Bundle {
    val I   = ifGen(!isMux)   {  in(Bool) }
    val I0  = ifGen(isMux)    {  in(Bool) }
    val I1  = ifGen(isMux)    {  in(Bool) default False }
    val S   = ifGen(isMux)    {  in(Bool) default False }
    val CE  = ifGen(hasCE)    {  in(Bool) default True }
    val CLR = ifGen(isRegion) {  in(Bool) default False }
    val O   =                   out(Bool)
  }

  noIoPrefix()
  addTag(noNumericType)
}


/*class ClkBufGlobalCtrlPrimitive extends BlackBox {
  val name = "BUFGCTRL"
  object GlobalCtrl extends ClkBufKind {
    def name = "BUFGCTRL" // Primitive: Global Clock Control Buffer
    // CE0, CE1, IGNORE0, IGNORE1, I0, I1, S0, S1, O
    // INIT_OUT, PRESELECT_I0, PRESELECT_I1
  }

  if (globalData.config.mode == VHDL) {
    librariesUsages += "UNISIM"
    setDefinitionName(s"vcomponents.${name}")
  } else {
    setDefinitionName(name)
  }


}*/

abstract class Part {
  def clkManBounds(kind : ClkManKind) : ClkManBounds
}

object Artix7Speed3 extends Part {
  val clkManInfo = ClkManBounds(
        fInMin  =  10 MHz, fInMax  =  800 MHz,
        fPfdMin =  10 MHz, fPfdMax =  550 MHz,
        fVcoMin = 600 MHz, fVcoMax = 1600 MHz)
  override def clkManBounds(kind : ClkManKind) = kind match {
    case Pll2Adv => clkManInfo.copy(nOut = 6, enFract = false)
    case Pll2Base => clkManInfo.copy(nOut = 6, enFract = false)
    case Mmcm2Adv => clkManInfo.copy(nOut = 7, enFract = true)
    case Mmcm2Base => clkManInfo.copy(nOut = 7, enFract = true)
  }
}

object Artix7Speed2 extends Part {
  val clkManInfo = ClkManBounds(
        fInMin  =  10 MHz, fInMax  =  800 MHz,
        fPfdMin =  10 MHz, fPfdMax =  500 MHz,
        fVcoMin = 600 MHz, fVcoMax = 1440 MHz)
  override def clkManBounds(kind : ClkManKind) = kind match {
    case Pll2Adv => clkManInfo.copy(nOut = 6, enFract = false)
    case Pll2Base => clkManInfo.copy(nOut = 6, enFract = false)
    case Mmcm2Adv => clkManInfo.copy(nOut = 7, enFract = true)
    case Mmcm2Base => clkManInfo.copy(nOut = 7, enFract = true)
  }
}

object Artix7Speed1 extends Part {
  val clkManInfo = ClkManBounds(
        fInMin  =  10 MHz, fInMax  =  800 MHz,
        fPfdMin =  10 MHz, fPfdMax =  450 MHz,
        fVcoMin = 600 MHz, fVcoMax = 1200 MHz)
  override def clkManBounds(kind : ClkManKind) = kind match {
    case Pll2Adv => clkManInfo.copy(nOut = 6, enFract = false)
    case Pll2Base => clkManInfo.copy(nOut = 6, enFract = false)
    case Mmcm2Adv => clkManInfo.copy(nOut = 7, enFract = true)
    case Mmcm2Base => clkManInfo.copy(nOut = 7, enFract = true)
  }
}
