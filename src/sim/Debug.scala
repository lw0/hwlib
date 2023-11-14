package hwlib.sim

import scala.collection.mutable.{Map, Set}
import scala.collection.immutable.Seq
import scala.io.AnsiColor._



class Debug(val env : ClockEnv) {
  private val instances = Map[Int, (Option[Int], String)]()

  private val tags = Set[String]()

  private val indentPat = "^|(?<=\n)".r

  def activate(tag : String) : Unit = {
    tags.addOne(tag)
  }

  def deactivate(tag : String) : Unit = {
    tags.subtractOne(tag)
  }

  def addRoot(instance : AnyRef, name : String) : Unit = {
    instances(instance.hashCode) = (None, name)
  }

  def addChild(parent : AnyRef, instance : AnyRef, name : String) : Unit = {
    instances(instance.hashCode) = (Some(parent.hashCode), name)
  }

  def print(msg : String, trace : Boolean = false) : Unit = {
    val stack = if (trace) "\n" + indentPat.replaceAllIn(getStack(), "    ") else ""
    println(f"[${env.cyclesRaw}%05d] ${msg}${stack}")
  }

  def printFor(instance : AnyRef, msg : String, trace : Boolean = false) : Unit = {
    val mayName = resolve(instance)
    if (mayName.isDefined) {
      print(s"[${mayName.get}] ${msg}", trace)
    }
  }

  def printForTag(instance : AnyRef, tag : String, msg : String, trace : Boolean = false) : Unit = {
    if (tags.contains(tag)) {
      printFor(instance, msg, trace)
    }
  }

  def fail(msg : String, trace : Boolean = false) : Unit = {
    print(s"[${RED}FAIL${RESET}] ${BOLD}${msg}${RESET}", trace)
  }

  def failFor(instance : AnyRef, msg : String, trace : Boolean = false) : Unit = {
    val name = resolve(instance).getOrElse(f"${instance.hashCode()}%08x")
    print(s"[${BOLD}${name}${RESET}] [${RED}FAIL${RESET}] ${BOLD}${msg}${RESET}", trace)
  }

  def failForTag(instance : AnyRef, tag : String, msg : String, trace : Boolean = false) : Unit = {
    if (tags.contains(tag)) {
      failFor(instance, msg, trace)
    }
  }

  def warn(msg : String, trace : Boolean = false) : Unit = {
    print(s"[${YELLOW}WARN${RESET}] ${BOLD}${msg}${RESET}", trace)
  }

  def warnFor(instance : AnyRef, msg : String, trace : Boolean = false) : Unit = {
    val name = resolve(instance).getOrElse(f"${instance.hashCode()}%08x")
    print(s"[${BOLD}${name}${RESET}] [${YELLOW}WARN${RESET}] ${BOLD}${msg}${RESET}", trace)
  }

  def warnForTag(instance : AnyRef, tag : String, msg : String, trace : Boolean = false) : Unit = {
    if (tags.contains(tag)) {
      warnFor(instance, msg, trace)
    }
  }

  def pass(msg : String, trace : Boolean = false) : Unit = {
    print(s"[${GREEN}PASS${RESET}] ${BOLD}${msg}${RESET}", trace)
  }

  def passFor(instance : AnyRef, msg : String, trace : Boolean = false) : Unit = {
    val name = resolve(instance).getOrElse(f"${instance.hashCode()}%08x")
    print(s"[${BOLD}${name}${RESET}] [${GREEN}PASS${RESET}] ${BOLD}${msg}${RESET}", trace)
  }

  def passForTag(instance : AnyRef, tag : String, msg : String, trace : Boolean = false) : Unit = {
    if (tags.contains(tag)) {
      passFor(instance, msg, trace)
    }
  }


  private def resolve(instance : AnyRef) : Option[String] = {
    var parent : Option[Int] = Some(instance.hashCode)
    var name = ""
    while (parent.isDefined) {
      val entry = instances.get(parent.get)
      if (entry.isDefined) {
        parent = entry.get._1
        name = entry.get._2 + (if (name.size > 0) "." + name else "")
      } else {
        return None
      }
    }
    Some(name)
  }

  private def getStack(filter : String = "^hwlib.*") : String = {
    val lines = try {
      require(false)
      Seq[String]()
    } catch { case ex : Throwable =>
      ex.getStackTrace.drop(2).filter(_.toString.matches(filter)).toSeq
    }
    lines.mkString("\n")
  }

  def dump() : Unit = {
    instances.foreachEntry{
      (hash, tup) =>
        println(f"$hash%08x: ${tup._1.map((h) => f"$h%08x")} <- \"${tup._2}\"")
    }
  }
}

