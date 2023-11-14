package hwlib.bundle

import spinal.core._
import spinal.lib.IMasterSlave



class AutoBundleBidir extends AutoBundle with IMasterSlave {
  def element(name : String, revDir: Boolean, clauses : AutoPolicyClause*) : Unit = {
    elementPolicies(name) = AutoPolicy.make(revDir, clauses)
  }

  def setDirections(asMaster : Boolean) : Unit = {
    for ((name, (element, policy)) <- elementMap()) {
      (element.isInstanceOf[IMasterSlave], policy.revDir != asMaster) match {
        case (true, true) => element.asInstanceOf[IMasterSlave].asMaster()
        case (true, false) => element.asInstanceOf[IMasterSlave].asSlave()
        case (false, true) => element.asOutput()
        case (false, false) => element.asInput()
      }
    }
  }

  override def asSlave() : Unit = {
    setDirections(false)
  }

  override def asMaster() : Unit = {
    setDirections(true)
  }

}

