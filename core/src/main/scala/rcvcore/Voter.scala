package rcvcore

import scala.util.Random

case class Voter(party: PopulationGroup, ideologyScore: Double, config: ElectionConfig) {

  def distanceScore(candidate: Candidate): Double = {
    200.0 - math.abs(ideologyScore - candidate.ideology)
  }

  def partyLoyalty(candidate: Candidate, config: ElectionConfig): Double = {
    party.partyBonus(candidate.party) * config.partyLoyalty
  }

  def uncertainty(config: ElectionConfig): Double = {
    GaussianGenerator.next() * config.uncertainty
  }

  def partyBonus(candidate: Candidate, config: ElectionConfig): Double = {
    candidate.party.partyBonus * config.partyBonusScale
  }

  def wastedVote(candidate: Candidate): Double = {
    if (candidate.party.tag == Republicans || candidate.party.tag == Democrats)
      return 0
    else
      return -config.wastedVoteFactor

  }

  def score(candidate: Candidate): Double = {
    distanceScore(candidate) +
      candidate.money +
      partyLoyalty(candidate, config) +
      partyBonus(candidate, config) +
      uncertainty(config) +
      wastedVote(candidate) +
      candidate.quality * config.qualityScale

  }

  def favorite(candidates: Seq[Candidate]): Candidate = {
    var f = CandidateScore(candidates.head, score(candidates.head))
    candidates.tail.foreach(c => {
      val s = score(c)
      if (s > f.score) {
        f = CandidateScore(c, s)
      }
    })
    f.candidate
  }

  def ballot(candidates: Seq[Candidate]): RCVBallot = {
    val scores = candidates.map(c => {
      CandidateScore(c, score(c))
    })
    RCVBallot(scores)
  }
}
