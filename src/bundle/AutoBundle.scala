package hwlib.bundle

import scala.collection.mutable.Map

import spinal.core._



class AutoBundle extends Bundle {

  def nameThis(prefix : String = "") : Unit = nameHere(this, prefix)
  def nameHere(parent : Nameable, prefix : String = "") : Unit = {
    for ((pname, (element, revDir, policy)) <- flatElementMap(false, prefix)) {
      element.setPartialName(parent, pname)
    }
  }

  def >>(to : AutoBundle) = to.connect(this)
  def <<(from : AutoBundle) = this.connect(from)
  def connect(from : AutoBundle) : Unit = {
    for ((name, (left, rev, right, policy, isBundle)) <- zipElementMap(from)) {
      (rev, isBundle) match {
        case (false, false) => {
          policy.assignData(left, right, false)
        }
        case (false, true) => {
          policy.assignBundle(left.asInstanceOf[AutoBundle], right.asInstanceOf[AutoBundle])
        }
        case (true, false) => {
          policy.assignData(right, left, false)
        }
        case (true, true) => {
          policy.assignBundle(right.asInstanceOf[AutoBundle], left.asInstanceOf[AutoBundle])
        }
      }
    }
  }

  def assign(parts : (String, Data)*) : this.type = assign(false, parts:_*)
  def assign(revDir : Boolean, parts : (String, Data)*) : this.type = {
    for ((name, (left, rev, right, policy)) <- zipFlatElementMap(Map.from(parts))) {
      if (!rev && !revDir) {
        policy.assignData(left, right, true)
      } else if (rev && revDir) {
        policy.assignData(left, right, true)
      }
    }
    this
  }

  def asRegInit(parts : (String, Data)*) : this.type = asRegInit(false, parts:_*)
  def asRegInit(revDir : Boolean, parts : (String, Data)*) : this.type = {
    for ((name, (left, rev, right, policy)) <- zipFlatElementMap(Map.from(parts))) {
      if (!rev && !revDir) {
        left.setAsReg()
        policy.initData(left, right)
      } else if (rev && revDir) {
        left.setAsReg()
        policy.initData(left, right)
      }
    }
    this
  }

  def cloneAssign(parts : (String, Data)*) : this.type = cloneAssign(false, parts:_*)
  def cloneAssign(revDir : Boolean, parts : (String, Data)*) : this.type = {
    cloneOf(this).asInstanceOf[this.type].assign(revDir, parts:_*)
  }

  def cloneRegInit(parts : (String, Data)*) : this.type = cloneRegInit(false, parts:_*)
  def cloneRegInit(revDir : Boolean, parts : (String, Data)*) : this.type = {
    cloneOf(this).asInstanceOf[this.type].asRegInit(revDir, parts:_*)
  }


  protected val elementPolicies = Map[String, AutoPolicy]()
  protected val nullPolicy = AutoPolicy()

  protected def element(name : String, clauses : AutoPolicyClause*) : Unit = {
    elementPolicies(name) = AutoPolicy.make(false, clauses)
  }

  protected def elementMap() : Map[String, (Data, AutoPolicy)] = {
    val map = Map[String, (Data, AutoPolicy)]()
    for ((name, element) <- elements) {
      val policy = elementPolicies.get(name).getOrElse(nullPolicy)
      map(name) = (element, policy)
    }
    map
  }

  protected def flatElementMap(reverse : Boolean = false, prefix : String = "", ppolicy : AutoPolicy = null) : Map[String, (Data, Boolean, AutoPolicy)] = {
    val map = Map[String, (Data, Boolean, AutoPolicy)]()
    for ((name, (element, policy)) <- elementMap()) {
      val localrev = reverse != policy.revDir
      val localpolicy = if (ppolicy != null) policy.withParent(ppolicy) else policy
      val localmap = if (element.isInstanceOf[AutoBundle]) {
        val localprefix = policy.getPrefix(prefix, name)
        element.asInstanceOf[AutoBundle].flatElementMap(localrev, localprefix, localpolicy)
      } else {
        Map((prefix concat name) -> (element, localrev, localpolicy))
      }
      val overlap = map.keySet.intersect(localmap.keySet)
      if (overlap.nonEmpty) {
        SpinalError(s"AutoBundle ${this} has ambiguous elements ${overlap}")
      }
      map.addAll(localmap)
    }
    map
  }

  protected def zipElementMap(other : AutoBundle) : Map[String, (Data, Boolean, Data, AutoPolicy, Boolean)] = {
    val leftMap = elementMap()
    val rightMap = other.elementMap()
    val keys = leftMap.keySet.union(rightMap.keySet)
    val map = Map[String, (Data, Boolean, Data, AutoPolicy, Boolean)]()
    keys.foreach{
      name =>
        val left = leftMap.get(name)
        val right = rightMap.get(name)
        (left, right) match {
          case (Some((lelement, lpolicy)), Some((relement, rpolicy))) => {
            if (lpolicy.revDir != rpolicy.revDir) {
              SpinalError(s"Elements ${name} have opposing directions between ${this} and ${other}")
            }
            val policy = if (lpolicy.revDir) rpolicy else lpolicy
            val isAutoBundle = lelement.isInstanceOf[AutoBundle] && relement.isInstanceOf[AutoBundle]
            map(name) = (lelement, lpolicy.revDir, relement, policy, isAutoBundle)
          }
          case (Some((lelement, lpolicy)), None) => {
            val isAutoBundle = lelement.isInstanceOf[AutoBundle]
            map(name) = (lelement, lpolicy.revDir, null, lpolicy, isAutoBundle)
          }
          case (None, Some((relement, rpolicy))) => {
            val isAutoBundle = relement.isInstanceOf[AutoBundle]
            map(name) = (null, rpolicy.revDir, relement, rpolicy, isAutoBundle)
          }
          case _ => {}
        }
    }
    map
  }

  protected def zipFlatElementMap(otherMap : Map[String, Data]) : Map[String, (Data, Boolean, Data, AutoPolicy)] = {
    val leftMap = flatElementMap()
    val keys = leftMap.keySet.union(otherMap.keySet)
    val map = Map[String, (Data, Boolean, Data, AutoPolicy)]()
    keys.foreach{
      name =>
        val left = leftMap.get(name)
        if (left.isDefined) {
          val right = otherMap.get(name)
          map(name) = (left.get._1, left.get._2, right.getOrElse(null), left.get._3)
        }
    }
    map
  }
}

