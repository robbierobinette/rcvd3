package rcvcore

case class ElectionDefinition(candidates: Seq[Candidate], population: CombinedPopulation, config: ElectionConfig, seed: Long) {
  def randomVoter: Voter = population.randomVoter(config)
}
