package hwlib

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.collection.mutable.Buffer
import scala.io.AnsiColor._
import scala.util.matching.Regex

import spinal.core._
import spinal.core.sim._
import spinal.core.internals._


object Expand {
  val Pattern : Regex = "\\{\\{([^\\}]+)\\}\\}".r

  def apply(str : String)(callback : (String) => Option[String]) : String = {
    Pattern.replaceSomeIn(str, (m : Regex.Match) => callback(m.group(1)))
  }
}

object WriteArtifact {

  def apply(context : GlobalData, name : String, contents : String) : String = {
    val topName = context.toplevel.definitionName
    val outDir = context.config.targetDirectory
    val fields = Map("topName" -> topName, "outDir" -> outDir)
    val filename = Expand(name)(fields.get(_)).replace("//", "/")
    val content = Expand(contents)(fields.get(_))
    Files.write(Paths.get(filename), content.getBytes(StandardCharsets.UTF_8))
    SpinalInfo(s"Generated ${filename}")
    filename
  }
}

class HwMain[T <: Component] {

  class SimSuite(val name : String, val build : (Args) => SimCompiled[T]) {
    val runFuncs = Buffer[(String, (T) => Unit)]()

    def addRun(name : String, run : (T) => Unit) : Unit = {
      runFuncs.addOne((name, run))
    }

    def execute(args : Args, runsEnabled : Set[String]) : Unit = {
      val compiled = build(args)
      for ((run, idx) <- runFuncs.view.zipWithIndex) {
        if (runsEnabled.isEmpty || runsEnabled.contains(run._1)) {
          val runName = if (run._1 == null) {
            if (runFuncs.length > 1) {
              s"${name}_Run${idx}"
            } else {
              s"${name}"
            }
          } else {
            s"${name}_${run._1}"
          }
          println(s"${BOLD}${GREEN}===== Launch run ${CYAN}${runName}${GREEN} =====${RESET}")
          compiled.doSim(runName)(run._2(_))
        }
      }
    }
  }

  var genFunc : Option[(Args) => SpinalReport[T]] = None
  var genEarlyPhases = Buffer[Phase]()
  var genLatePhases = Buffer[Phase]()
  var postFunc : Option[(Args, SpinalReport[T]) => Unit] = None

  var baseFrequency : ClockDomain.ClockFrequency = ClockDomain.UnknownFrequency()
  def clockAt(freq : HertzNumber) = {
    baseFrequency = ClockDomain.FixedFrequency(freq)
  }

  var baseConfig : ClockDomainConfig = ClockDomainConfig()
  def clockCfg(clockEdge : EdgeKind = RISING,
               resetKind : ResetKind = ASYNC,
               resetLevel : Polarity = HIGH,
               enableLevel : Polarity = HIGH) = {
    baseConfig = ClockDomainConfig(
      clockEdge = clockEdge,
      resetKind = resetKind,
      resetActiveLevel = resetLevel,
      clockEnableActiveLevel = enableLevel)
  }

  val simSuites = Buffer[SimSuite]()

  def mainName : String = {
    getClass().getSimpleName().stripSuffix("$")
  }

  def suiteName(suite : String) : String = {
    if (suite == null) {
      mainName
    } else {
      suite
    }
  }

  def spinalConfig = {
    val config = SpinalConfig(
        defaultClockDomainFrequency=baseFrequency,
        defaultConfigForClockDomains=baseConfig)
    config.phasesInserters += ((phases) => {
      if (genEarlyPhases.nonEmpty) {
        val earlyIndex = phases.indexWhere(_.isInstanceOf[PhaseDummy])
        if (earlyIndex < 0) {
          SpinalError(s"Can not insert early phases, no anchor phase found!")
        } else {
          phases.insertAll(earlyIndex + 1, genEarlyPhases)
        }
      }
      if (genLatePhases.nonEmpty) {
        val lateIndex = phases.indexWhere(_.isInstanceOf[PhaseGetInfoRTL])
        if (lateIndex < 0) {
          SpinalError(s"Can not insert late phases, no anchor phase found!")
        }else {
          phases.insertAll(lateIndex, genLatePhases)
        }
      }
    })
    config
  }

  def genWith(comp : => T) : Unit = {
    gen { args =>
      spinalConfig
        .copy(
          mode=Verilog,
          onlyStdLogicVectorAtTopLevelIo = true,
          oneFilePerComponent = true,
          targetDirectory=s"${args.gendir}/")
        .generate(if (args.topname != null) comp.setDefinitionName(args.topname) else comp)
    }
  }

  def gen(generate : (Args) => SpinalReport[T]) : Unit = {
    if (genFunc.isEmpty) {
      genFunc = Some(generate)
    } else {
      SpinalError(s"${this} defines multiple generate-functions")
    }
  }

  def genEarlyPhase(phase : Phase) : Unit = {
    genEarlyPhases += phase
  }

  def genLatePhase(phase : Phase) : Unit = {
    genLatePhases += phase
  }

  def postGen(post : (Args, SpinalReport[T]) => Unit) : Unit = {
    if (postFunc.isEmpty) {
      postFunc = Some(post)
    } else {
      SpinalError(s"${this} defines multiple post-generate-functions")
    }
  }


  def simWith(comp : => T) : Unit = {
    simSuiteWith(null)(comp)
  }

  def sim(compile : (Args) => SimCompiled[T]) : Unit = {
    simSuite(null)(compile)
  }

  def simSuiteWith(suite : String)(comp : => T) : Unit = {
    simSuite(suite) { args =>
      (if (args.xsim) SimConfig.withXSim else SimConfig.withVerilator)
          .workspacePath(s"${args.simdir}/")
          .workspaceName(args.simname)
          .withConfig(spinalConfig)
          .withWave
          //.addSimulatorFlag("-initfile=/home/lwenzel/Xilinx/Vivado/2022.1/data/xsim/ip/xsim_ip.ini")
          //.withSimScript("auto_detect_xpm")
          .compile(if (args.topname != null) comp.setDefinitionName(args.topname) else comp)
    }
  }

  def simSuite(suite : String)(compile : (Args) => SimCompiled[T]) : Unit = {
    val sname = suiteName(suite)
    val maySimSuite = simSuites.find((s) => s.name == sname)
    if (maySimSuite.isEmpty) {
      simSuites.addOne(new SimSuite(sname, compile))
    } else {
      SpinalError(s"${this} defines multiple compile-functions for simulation suite \"${sname}\"")
    }
  }

  def simRun(run : (T) => Unit) : Unit = {
    simSuiteRun(null, null)(run)
  }

  def simRun(name : String)(run : (T) => Unit) : Unit = {
    simSuiteRun(null, name)(run)
  }

  def simSuiteRun(suite : String, name : String = null)(run : (T) => Unit) : Unit = {
    val sname = suiteName(suite)
    val maySimSuite = simSuites.find((s) => s.name == sname)
    if (maySimSuite.isDefined) {
      maySimSuite.get.addRun(name, run)
    } else {
      SpinalError(s"${this} defines a run-function for undefined simulation suite \"${sname}\"")
    }
  }

  def main(argv : Array[String]) : Unit = {
    val args = Args(argv)
    if (args.gen) {
      if (genFunc.isDefined) {
        println(s"${BOLD}${GREEN}=== Generating design ${BLUE}${mainName}${GREEN} ===${RESET}")
        val report = genFunc.get(args)
        if (postFunc.isDefined) {
          postFunc.get(args, report)
        }
      } else {
        println(s"${BOLD}${RED}=== Can not generate design ${BLUE}${mainName}${RED} ===${RESET}")
        System.exit(1)
      }
    }
    if (args.sim) {
      if (simSuites.nonEmpty) {
        for (suite <- simSuites) {
          if (args.suitesEnabled.isEmpty || args.suitesEnabled.contains(suite.name)) {
            val runsEnabled = args.suitesEnabled.get(suite.name).getOrElse(Set[String]())
            println(s"${BOLD}${GREEN}=== Simulating design ${BLUE}${mainName}${GREEN} with suite ${CYAN}${suite.name}${GREEN} ===${RESET}")
            suite.execute(args, runsEnabled)
          }
        }
      } else {
        println(s"${BOLD}${RED}=== Can not simulate design ${BLUE}${mainName}${RED} ===${RESET}")
        System.exit(1)
      }
    }
  }
}


class Args(
  val topname : String,
  val gen : Boolean,
  val gendir : String,
  val sim : Boolean,
  val simdir : String,
  val simname : String,
  val xsim : Boolean,
  val suitesEnabled : Map[String, Set[String]]) {
}

object Args {
  val topRegex : Regex = """top:(\w+)""".r
  val simRegex : Regex = """sim(?::(/?(?:[^/\s]+/)*)([^/\s]+))?""".r
  val xsimRegex : Regex = """xsim(?::(/?(?:[^/\s]+/)*)([^/\s]+))?""".r
  val genRegex : Regex = """gen(?::(/?[^/\s]+(?:/[^/\s]+)*))?""".r
  val suiteRegex : Regex = """suite:(\w+(?::\w+(?:,\w+)*)?(?:;\w+(?::\w+(?:,\w+)*)?)*)?""".r
  val suitePartRegex : Regex = """(\w+)(?::(\w+(?:,\w+)*))?""".r

  def apply(argv : Array[String]) : Args = {
    var topname : String = null
    var gen : Boolean = false
    var gendir : String = null
    var sim : Boolean = false
    var simdir : String = null
    var simname : String = null
    var xsim : Boolean = false
    var suites = Map[String, Set[String]]()
    argv.foreach { str =>
      str match {
        case topRegex(name) => {
          topname = name
        }
        case simRegex(path, name) => {
          sim = true
          xsim = false
          simdir = if (path != null) path else "sim"
          simname = if (path != null) name else "sim"
        }
        case xsimRegex(path, name) => {
          sim = true
          xsim = true
          simdir = if (path != null) path else "sim"
          simname = if (path != null) name else "sim"
        }
        case genRegex(path) => {
          gen = true
          gendir = if (path != null) path else "gen"
        }
        case suiteRegex(spec) => {
          if (spec != null) {
            suites = Map.from(
                spec.split(";")
                    .map( s => suitePartRegex.findFirstMatchIn(s))
                    .map{ m =>
                      val suiteName = m.get.group(1)
                      val runSet : Set[String] = if (m.get.group(2) != null) {
                        Set.from(m.get.group(2).split(","))
                      } else {
                        Set()
                      }
                      (suiteName, runSet)
                    })
          }
        }
        case _ => println(s"${BOLD}${YELLOW}Ignoring argument \"$str\"${RESET}")
      }
    }
    new Args(topname, gen, gendir, sim, simdir, simname, xsim, suites)
  }
}


