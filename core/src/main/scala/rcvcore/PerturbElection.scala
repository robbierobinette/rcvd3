package rcvcore
import scala.util.Random.nextGaussian



case class PerturbPopulation(population: CombinedPopulation,
                           ideologyNoise: Double,
                           stddevNoise: Double,
                           weightNoise: Double) {

  lazy val newPopulations: Seq[PopulationGroup] = population.populations.map(pop => {
    pop.copy(
      mean = pop.mean + nextGaussian() * ideologyNoise,
      stddev = pop.stddev + nextGaussian() * stddevNoise,
      weight = pop.weight + nextGaussian() * weightNoise
    )
  })
  lazy val newCombinedPopulation = CombinedPopulation(newPopulations)
}

case class PerturbCandidates(candidates: Seq[Candidate],
                           ideologyNoise: Double,
                           moneyNoise: Double,
                           qualityNoise: Double) {
  val newCandidates: Unit = candidates.foreach(c => {
    c.copy(
      ideology = c.ideology + nextGaussian() * ideologyNoise,
        money = c.money + nextGaussian() * moneyNoise,
        quality = c.quality + nextGaussian() * qualityNoise
    )
  })
}
