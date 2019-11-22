package rcvcore


import scala.io.Source
import scala.util.Random


case class DistrictVotingRecord(district: String, incumbent: String, dPct1: Double, rPct1: Double, dPct2: Double, rPct2: Double) {
  lazy val state = district.split("-")(0)
}

case object WeightedPopulation {
  def apply(dvr: DistrictVotingRecord): CombinedPopulation = {
    import dvr._
    val rPct = (rPct1 + rPct2) / 2.0 / 100.0
    val dPct = (dPct1 + dPct2) / 2.0 / 100.0

    val iWeight = .40
    val rWeight = (1 - iWeight) * rPct
    val dWeight = (1 - iWeight) * dPct

    val rep = PopulationGroup(Republicans, 1, 15, 15, 0, rWeight)
    val dem = PopulationGroup(Democrats, 1, -15, 15, 0, dWeight)
    val ind = PopulationGroup(Independents, 0, 0, 15, 0, iWeight)
    CombinedPopulation(Seq(rep, dem, ind))
  }
}

case object WeightAndSkewedPopulation {
  def apply(dvr: DistrictVotingRecord, skewFactor: Double): CombinedPopulation = {
    import dvr._
    val rPct = (rPct1 + rPct2) / 2.0 / 100.0
    val dPct = (dPct1 + dPct2) / 2.0 / 100.0

    val iWeight = .30
    val rWeight = (1 - iWeight) * rPct
    val dWeight = (1 - iWeight) * dPct
    val skew = (rWeight - dWeight) / 2.0  * skewFactor * 100

    val rep = PopulationGroup(Republicans, 1, 15 + skew, 15, 0, rWeight * 100)
    val dem = PopulationGroup(Democrats, 1, -15 + skew, 15, 0, dWeight * 100)
    val ind = PopulationGroup(Independents, 0, 0 + skew, 15, 0, iWeight * 100)
    CombinedPopulation(Seq(rep, dem, ind))
  }
}

case class CongressionalSimulationConfig(label: String,
                                         config: ElectionConfig,
                                         populationSkew: Double,
                                         nRepublicanCandidates: Int,
                                         nDemocraticCandidates: Int,
                                         nIndependentCandidates: Int,
                                         ideologyNoise: Double,
                                         primarySkew: Double,
                                         moneyNoise: Double,
                                         qualityNoise: Double) {


  def adjustElectionProcess(baseProcess: ElectionProcess): ElectionProcess = {
    baseProcess match {
      case e: ElectionWithPrimary =>
        e.copy(primarySkew = primarySkew)
      case e: RCVElection => e
    }
  }

  def generateDefinition(dvr: DistrictVotingRecord, seed: Long): ElectionDefinition = {
    Random.setSeed(seed)
    val districtPop = WeightAndSkewedPopulation(dvr, populationSkew)
    val repCandidates = (0 until nRepublicanCandidates).map(i => getCandidate(districtPop.partyMap(Republicans), s"R$i"))
    val demCandidates = (0 until nDemocraticCandidates).map(i => getCandidate(districtPop.partyMap(Democrats), s"D$i"))
    val indCandidates = (0 until nIndependentCandidates).map(i => getCandidate(districtPop.partyMap(Independents), s"I$i"))
    val candidates = (repCandidates ++ demCandidates ++ indCandidates).sortWith((a , b) => a.ideology < b.ideology)
    val electionDefSeed = Random.nextLong()
    ElectionDefinition(candidates, districtPop, config, electionDefSeed)
  }

  def getCandidate(party: PopulationGroup, name: String): Candidate = {
    Candidate(name, party,
      party.mean + Random.nextGaussian() * ideologyNoise,
      Random.nextGaussian() * moneyNoise,
      Random.nextGaussian() * qualityNoise,
      incumbent = false)
  }

  def describe(): String = {
    s" label: $label" +
      s" ideologyNoise: $ideologyNoise" +
      s" primarySkew: $primarySkew" +
      s" moneyNoise: $moneyNoise" +
      s" qualityNoise: $qualityNoise" +
      s" wastedVoteFactor: ${config.wastedVoteFactor}" +
      s" uncertainty : ${config.uncertainty}" +
      s" partyLoyalty : ${config.partyLoyalty}" +
      s" qualityScale : ${config.qualityScale}" +
      s" partyBonus : ${config.partyBonusScale}"
  }
}

case class CongressionalSimulation(votingRecords: Seq[DistrictVotingRecord], config: CongressionalSimulationConfig, process: ElectionProcess) {
  val start = System.nanoTime()
  val results: Seq[CandidateResult] = votingRecords.map { vr =>
    val result = config.adjustElectionProcess(process).run(config.generateDefinition(vr, System.nanoTime()))
    result.orderedResults.head
  }
  println(f"${process.getClass().getCanonicalName()} Simulation took ${(System.nanoTime() - start) / 1e9}%.3f seconds")
}
