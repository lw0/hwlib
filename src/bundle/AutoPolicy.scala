package hwlib.bundle

import scala.collection.mutable.StringBuilder

import spinal.core._



case class AutoPolicy (
  revDir       : Boolean = false,
  usePrefix    : Boolean = false,
  prefix       : String = null,
  polDefault   : Int = 0,
  default      : () => Data = null,
  polDrop      : Int = 0,
  polUpsize    : Int = 0,
  polDownsize  : Int = 0) {

  def mayDefault   = (polDefault  & 0x3) == 0x1
  def mayIgnore    = (polDefault  & 0x2) == 0x2
  def warnDefault  = (polDefault  & 0xc) == 0x4
  def mayDrop      = (polDrop     & 0x3) == 0x1
  def warnDrop     = (polDrop     & 0xc) == 0x4
  def mayUpsize    = (polUpsize   & 0x3) == 0x1
  def warnUpsize   = (polUpsize   & 0xc) == 0x4
  def mayDownsize  = (polDownsize & 0x3) == 0x1
  def warnDownsize = (polDownsize & 0xc) == 0x4

  def withParent(parent : AutoPolicy) : AutoPolicy = {
    copy(
      polDefault = polDefault | parent.polDefault,
      polDrop = polDrop | parent.polDrop,
      polUpsize = polUpsize | parent.polUpsize,
      polDownsize = polDownsize | parent.polDownsize)
  }

  def getPrefix(preprefix : String, name : String) : String = {
    if (usePrefix) {
      preprefix concat (if (prefix == null) name else prefix)
    } else {
      preprefix
    }
  }

  def assignData(to : Data, from : Data, permissive : Boolean) : Unit = {
    // TODO-lw shouldnt this be BaseType instead of data?
    // TODO-lw maybe check for BitVector.width > 0 in addition to != null
    (to != null, from != null) match {
      case (true, true) => new Composite(to) { // both signals exist
        val msg = s"Width mismatch assigning ${to} from ${from}!"
        val wTo = widthOf(to)
        val wFrom = widthOf(from)
        // println(s"${to} (${to.component}) := ${from} (${from.component})")
        if (wTo == wFrom || from.hasTag(tagAutoResize)) {
          to := from
        } else if (wTo > wFrom && mayUpsize) {
          if (warnUpsize) SpinalWarning(msg)
          to := from.resized
        } else if (mayDownsize) {
          if (warnDownsize) SpinalWarning(msg)
          to := from.resized
        } else {
          PendingError(msg)
        }
      }
      case (true, false) => new Composite(to) { // from signal is missing
        if (mayIgnore && !permissive) {
          val msg = s"Ignoring ${to} assignment!"
          if (warnDefault) SpinalWarning(msg)
        } else if (mayDefault || permissive) {
          val msg = s"Assigning ${to} from default ${default}!"
          if (warnDefault && !permissive) SpinalWarning(msg)
          if (default != null) {
            // println(s"${to} (${to.component}) := ${default} (${default.component})")
            to := default().resized
          } else {
            to := to.getZero
          }
        } else {
          val msg = s"Nothing to assign to ${to}!"
          PendingError(msg)
        }
      }
      case (false, true) => new Composite(from) { // from signal is dropped
        if(!permissive) {
          val msg = s"Dropping signal ${from}!"
          if (warnDrop) {
            SpinalWarning(msg)
          } else if (!mayDrop) {
            PendingError(msg)
          }
        }
      }
      case _ => {}
    }
  }

  def initData(to : Data, from : Data) : Unit = {
    if (from != null) {
      to.init(from.resized)
    } else {
      if (mayDefault && (default != null)) {
        to.init(default().resized)
      } else {
        to.init(to.getZero)
      }
    }
  }

  def assignBundle(to : AutoBundle, from : AutoBundle) : Unit = {
    (to != null, from != null) match {
      case (true, true) => { // both bundles exist
        to.connect(from)
      }
      case (true, false) => { // from signal is missing
        if (!mayIgnore) {
          if (mayDefault) {
            val msg = s"Assigning ${to} from default!"
            if (warnDefault) SpinalWarning(msg)
            to.assign(false)
          } else {
            val msg = s"Nothing to connect to ${to}!"
            PendingError(msg)
          }
        }
      }
      case (false, true) => { // from signal is dropped
        val msg = s"Dropping signal ${from}!"
        if (warnDrop) {
          SpinalWarning(msg)
        } else if (!mayDrop) {
          PendingError(msg)
        }
      }
      case _ => {}
    }
  }

  override def toString() : String = {
    val buf = new StringBuilder("AutoPolicy(")
    buf ++= (if (revDir) "Reverse" else "Normal")
    if (usePrefix) {
      buf ++= s", UsePrefix($prefix)"
    }
    if (polDefault > 0) {
      buf ++= (if (warnDefault) ", Warn" else ", May")
      buf ++= (if (mayIgnore) "Ignore" else "Default(${default()})")
    }
    if (polDrop > 0) {
      buf ++= (if (warnDrop) ", WarnDrop" else ", MayDrop")
    }
    if (polUpsize == polDownsize) {
      if (polUpsize > 0) {
        buf ++= (if (warnUpsize) ", WarnResize" else ", MayResize")
      }
    } else {
      if (polUpsize > 0) {
        buf ++= (if (warnUpsize) ", WarnUpsize" else ", MayUpsize")

      }
      if (polDownsize > 0) {
        buf ++= (if (warnUpsize) ", WarnDownsize" else ", MayDownsize")
      }
    }
    buf ++= ")"
    buf.result()
  }
}

class AutoPolicyClause
object     NoPrefix                    extends AutoPolicyClause
object     UsePrefix                   extends AutoPolicyClause
case class UsePrefix(prefix : String)  extends AutoPolicyClause

object     NoDefault                   extends AutoPolicyClause
object     ForbidDefault               extends AutoPolicyClause
object     UseDefaultZero              extends AutoPolicyClause
class UseDefault(val default : () => Data) extends AutoPolicyClause
object UseDefault {
  def apply(default : => Data) = new UseDefault(() => default)
  def unapply(clause : UseDefault) = Some(clause.default)
}
object     MayIgnore                   extends AutoPolicyClause
object     WarnDefaultZero             extends AutoPolicyClause
class WarnDefault(val default : () => Data) extends AutoPolicyClause
object WarnDefault {
  def apply(default : => Data) = new WarnDefault(() => default)
  def unapply(clause : WarnDefault) = Some(clause.default)
}
object     WarnIgnore                  extends AutoPolicyClause
object     SilentDefault               extends AutoPolicyClause
object     SilentIgnore                extends AutoPolicyClause

object     NoDrop                      extends AutoPolicyClause
object     MayDrop                     extends AutoPolicyClause
object     ForbidDrop                  extends AutoPolicyClause
object     WarnDrop                    extends AutoPolicyClause
object     SilentDrop                  extends AutoPolicyClause

object     NoUpsize                    extends AutoPolicyClause
object     MayUpsize                   extends AutoPolicyClause
object     ForbidUpsize                extends AutoPolicyClause
object     WarnUpsize                  extends AutoPolicyClause
object     SilentUpsize                extends AutoPolicyClause

object     NoDownsize                  extends AutoPolicyClause
object     MayDownsize                 extends AutoPolicyClause
object     ForbidDownsize              extends AutoPolicyClause
object     WarnDownsize                extends AutoPolicyClause
object     SilentDownsize              extends AutoPolicyClause

object     NoResize                    extends AutoPolicyClause
object     MayResize                   extends AutoPolicyClause
object     ForbidResize                extends AutoPolicyClause
object     WarnResize                  extends AutoPolicyClause
object     SilentResize                extends AutoPolicyClause

object AutoPolicy {
  def make(revDir : Boolean, clauses : Seq[AutoPolicyClause]) : AutoPolicy = {
    var usePrefix = false
    var polDefault, polDrop, polUpsize, polDownsize = 0
    var prefix : String = null
    var default : () => Data = null
    clauses.foreach {
      case NoPrefix        => usePrefix = false
      case UsePrefix       => usePrefix = true; prefix = null
      case UsePrefix(p)    => usePrefix = true; prefix = p
      case NoDefault       => polDefault = 0x0
      case ForbidDefault   => polDefault = 0x2
      case UseDefaultZero  => polDefault = 0x1; default = null
      case UseDefault(d)   => polDefault = 0x1; default = d
      case MayIgnore       => polDefault = 0x2
      case WarnDefaultZero => polDefault = 0x5; default = null
      case WarnDefault(d)  => polDefault = 0x5; default = d
      case WarnIgnore      => polDefault = 0x6
      case SilentDefault   => polDefault = 0x9
      case SilentIgnore    => polDefault = 0xa
      case NoDrop          => polDrop = 0x0
      case MayDrop         => polDrop = 0x1
      case ForbidDrop      => polDrop = 0x2
      case WarnDrop        => polDrop = 0x5
      case SilentDrop      => polDrop = 0x9
      case NoUpsize        => polUpsize = 0x0
      case MayUpsize       => polUpsize = 0x1
      case ForbidUpsize    => polUpsize = 0x2
      case WarnUpsize      => polUpsize = 0x5
      case SilentUpsize    => polUpsize = 0x9
      case NoDownsize      => polDownsize = 0x0
      case MayDownsize     => polDownsize = 0x1
      case ForbidDownsize  => polDownsize = 0x2
      case WarnDownsize    => polDownsize = 0x5
      case SilentDownsize  => polDownsize = 0x9
      case NoResize        => polUpsize = 0x0; polDownsize = 0x0
      case MayResize       => polUpsize = 0x1; polDownsize = 0x1
      case ForbidResize    => polUpsize = 0x2; polDownsize = 0x2
      case WarnResize      => polUpsize = 0x5; polDownsize = 0x5
      case SilentResize    => polUpsize = 0x9; polDownsize = 0x9
    }
    AutoPolicy (revDir,
      usePrefix, prefix,
      polDefault, default,
      polDrop, polUpsize, polDownsize)
  }
}

