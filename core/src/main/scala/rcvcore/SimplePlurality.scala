package rcvcore

import scala.collection.mutable

case class SimplePlurality(nVoters: Int = 10000) extends ElectionProcess {
  // population.populations.foreach(p => println(f"${p.tag}: pop.mean ${p.mean}% 7.2f") )


  def name: String = "Simple Plurality"

  override def run(electionDefinition: ElectionDefinition): ElectionResult = {
    import electionDefinition._
    val results = mutable.Map[Candidate, Int]()
    (0 until nVoters).foreach(_ => {
      val c = population.randomVoter(config).favorite(candidates)
      results(c) = results.getOrElse(c, 0) + 1
    })
    PluralityResult(candidates, results, this )
  }
}

case class PluralityResult(candidates: Seq[Candidate], results: mutable.Map[Candidate, Int], process: ElectionProcess) extends ElectionResult {

}
