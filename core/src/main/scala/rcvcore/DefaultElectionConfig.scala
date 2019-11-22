package rcvcore

object DefaultElectionConfig {
  def apply(): ElectionDefinition = {
    val uncertainty: Double = 10
    val partyLoyaltyScale: Double = 10
    val qualityScale: Double = 1
    val partyBonus: Double = 2

    val republicans = PopulationGroup(Republicans, 1, 15, 15, 0, 100)
    val democrats = PopulationGroup(Democrats, 1, -15, 15, 0, 100)
    val independents = PopulationGroup(Independents, 0, 0, 15, 0, 100)
    val populations = Seq(republicans, democrats, independents)
    val combinedPopulation = CombinedPopulation(populations)

    val config = ElectionConfig(uncertainty, partyLoyaltyScale, qualityScale, partyBonus)

    val n_candidates = 10
    val ideologyRange = 40
    val candidates = (0 until n_candidates).map(i => {
      val is = -ideologyRange + 2 * ideologyRange * i.toDouble / (n_candidates - 1)
      var party = independents
      var money = 0
      var name = s"ind-$i"
      if (is < -7) {
        party = democrats
        money = 5
        name = s"dem-$i"
      }
      else if (is > 7) {
        party = republicans
        money = 5
        name = s"rep-$i"
      }
      Candidate(name, party, is, 0, 0, incumbent = false)
    })
    ElectionDefinition(candidates, combinedPopulation, config, System.nanoTime())
  }
}
