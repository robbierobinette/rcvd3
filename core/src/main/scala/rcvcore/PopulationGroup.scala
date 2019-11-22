package rcvcore

import scala.util.Random


case class PopulationGroup(tag: PopulationTag,
                           partyBonus: Double = 0,
                           mean: Double = 0,
                           stddev: Double = 15,
                           skew: Double = 0,
                           weight: Double = 100) {

  def pluralName = tag.pluralName

  def name = tag.name

  def partyBonus(other: PopulationGroup): Double = {
    tag.partyLoyalty(other.tag)
  }

  def getPDFData: (Seq[Double], Seq[Double]) = {
    ProbabilityDistributionFunction(mean, stddev, skew, 1.0, 30).data
  }

  def randomVoter(config: ElectionConfig): Voter = {
    Voter(this, GaussianGenerator.next() * stddev + mean, config)
  }

  def randomCandidate(name: String, ideologyStddev: Double, moneyStddev: Double, qualityStddev: Double): Candidate = {
    Candidate(name, this,
      this.mean + GaussianGenerator.next() * ideologyStddev,
      GaussianGenerator.next() * moneyStddev,
      GaussianGenerator.next() * qualityStddev,
      incumbent = false
    )
  }
}
