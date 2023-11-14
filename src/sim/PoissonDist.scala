package hwlib.sim

import scala.util.Random
import scala.math._



case class PoissonDist(val lambda : Double, val offset : Long = 0){
  def apply() = {
    val limit = exp(-lambda)
    var res = 0L
    var prob = 1.0
    do {
      res += 1
      prob *= Random.nextDouble()
    } while (prob > limit)
    res + offset - 1
  }
}


object DelayGen {
  def apply(mean : Double) = {
    val dist = PoissonDist(0.0 max mean, 1)
    () => dist()
  }
}


object RateGen {
  def apply(mean : Double) = {
    val bw = Double.MinPositiveValue max mean min 1.0
    val dist = PoissonDist((1.0 - bw) / bw, 1)
    () => dist()
  }
}


object InstantGen {
  def apply() = {
    () => 0L
  }
}


