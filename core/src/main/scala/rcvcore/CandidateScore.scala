package rcvcore

case class CandidateScore(candidate: Candidate, score: Double) extends Ordered[CandidateScore] {
  def compare(that: CandidateScore): Int = {
    math.signum(score - that.score).toInt
  }
}
