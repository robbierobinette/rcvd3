package rcvcore

import scala.collection.mutable
import scala.util.Random

case class RCVBallot(candidates: Seq[CandidateScore]) {
  val sortedCandidates: Seq[CandidateScore] = candidates.sortWith { case (a, b) => a.score > b.score }

  def candidate(activeCandidates: Set[Candidate]): Candidate = {
    for (c <- sortedCandidates) {
      if (activeCandidates.contains(c.candidate))
        return c.candidate
    }
    candidates.head.candidate
  }
}


case class RCVResult(process: ElectionProcess, rounds: Seq[RCVRoundResult]) extends ElectionResult {
  override val results: mutable.Map[Candidate, Int] = rounds.last.results

  override def candidates: Seq[Candidate] = rounds.last.candidates
}


case class RCVRoundResult(candidates: Seq[Candidate], activeCandidates: Set[Candidate],
                           results: mutable.Map[Candidate, Int]) extends ElectionResult {

}

case class RCVElection(nVoters: Int = 1000, debug: Boolean = false) extends ElectionProcess {
  def name = "Ranked Choice"

  var rounds: Seq[RCVRoundResult] = Seq[RCVRoundResult]()

  def run(electionDef: ElectionDefinition): ElectionResult = {
    Random.setSeed(electionDef.seed)
    val config = electionDef.config.copy(wastedVoteFactor = 0)
    val ballots = (0 until nVoters).map(_ => {
      electionDef.population.randomVoter(config).ballot(electionDef.candidates)
    })

    var activeCandidates = Set(electionDef.candidates: _*)
    do {
      val round = computeRoundResult(ballots, activeCandidates, electionDef.candidates)
      rounds = rounds :+ round
      activeCandidates = activeCandidates - round.orderedResults.last.candidate
    } while (rounds.last.orderedResults.head.votes.toDouble / nVoters < .5)
    if (debug) debugPrint()
    RCVResult(this, rounds)
  }

  def computeRoundResult(ballots: Seq[RCVBallot], activeCandidates: Set[Candidate], candidates: Seq[Candidate]): RCVRoundResult = {
    val results: mutable.Map[Candidate, Int] = mutable.Map[Candidate, Int]()
    for (b <- ballots) {
      results(b.candidate(activeCandidates)) = results.getOrElse(b.candidate(activeCandidates), 0) + 1
    }
    RCVRoundResult(candidates, activeCandidates, results)
  }

  def debugPrint(): Unit = {
    rounds.foreach(r => {
      println("Round!")
      r.orderedResults.foreach(cr => {
        println(f"${cr.candidate.name}%20s ${cr.candidate.ideology}% 6.0f ${cr.votes}%6d")
      })
    })
  }
}

object RCVElectionTest {
  def main(args: Array[String]): Unit = {
    val electionDef = DefaultElectionConfig()
    val election = RCVElection(nVoters = 1000, debug = true)
    election.run(electionDef)
  }
}
