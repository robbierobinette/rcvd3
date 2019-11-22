package rcvcore

import math.{log, cos, sqrt, Pi}

object GaussianGenerator {
  val generator = GaussianGenerator(0)
  def next(): Double = generator()
}

case class GaussianGenerator(seed: Long = 0) {
  val rand = scala.util.Random
  var currentSeed = seed
  rand.setSeed(seed)

  def apply(): Double = {
    // val u = rand.nextDouble()
    // val v = rand.nextDouble()
    // sqrt(-2.0 * log(u)) * cos(2.0 * Pi * v);
    // (u + v) - 1.0
    rand.nextGaussian()
    // 0.0
  }
}
