package rcvcore

import scala.collection.mutable

trait ElectionResult {
  val results: mutable.Map[Candidate, Int]
  lazy val nVotes: Int = results.values.sum
  lazy val orderedResults: Seq[CandidateResult] = results.toSeq.map(pair => CandidateResult(pair._1, pair._2)).
    sortWith((a, b) => a.votes > b.votes)
  def winner: Candidate = orderedResults.head.candidate
  def candidates: Seq[Candidate]
  def votesFor(c: Candidate): Int = results.getOrElse(c, 0)
  def voteTotals: Seq[Int] = candidates.map(c => votesFor(c))
  def votePercent: Seq[Double] = voteTotals.map(v => v.toDouble / nVotes * 100.0)
}
