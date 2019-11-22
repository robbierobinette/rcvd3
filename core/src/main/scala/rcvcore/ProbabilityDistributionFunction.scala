package rcvcore

import scala.collection.mutable

case class ProbabilityDistributionFunction(mean: Double, stddev: Double, skew: Double, y_scale: Double, x_scale: Double) {
  lazy val data: (Seq[Double], Seq[Double]) = compute_pdf()

  def compute_pdf(): (Seq[Double], Seq[Double]) = {
    val y_data = mutable.ArrayBuffer[Double]()
    val x_data = mutable.ArrayBuffer[Double]()

    val steps = 1000.0
    val start = -100.0
    val end = 100.0
    val step = (end - start) / steps
    val pi = 3.14159265
    val u = this.mean
    val scale = 1 / Math.sqrt(2 * pi * stddev * stddev)
    for (i <- 0 until steps.toInt) {
      val x = start + i * step
      val p = scale * Math.exp(-1 * (x - u) * (x - u) / (2 * stddev * stddev))
      y_data.append(p)
      x_data.append(x)
    }
    (x_data, y_data)
  }
}
