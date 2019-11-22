package rcvcore
import scala.collection.mutable

case class ElectionWithPrimaryResult(process: ElectionProcess,
                                dResult: ElectionResult,
                                rResult: ElectionResult,
                                generalResult: ElectionResult) extends ElectionResult {
  override val results: mutable.Map[Candidate, Int] = generalResult.results

  override def candidates: Seq[Candidate] = generalResult.candidates
}
