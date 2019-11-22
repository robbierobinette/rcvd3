package rcvcore

import scala.util.Random

case class ElectionWithPrimary(primarySkew: Double, nVoters: Int = 1000, debug: Boolean = false) extends ElectionProcess {

  // import electionDef._
  def name: String = "Primary"

  def run(electionDef: ElectionDefinition): ElectionWithPrimaryResult = {

    Random.setSeed(electionDef.seed)


    val generalDems = electionDef.population.partyMap(Democrats)
    val primaryDems = generalDems.copy(mean = generalDems.mean - primarySkew)
    val demCandidates = electionDef.candidates.filter(c => c.party.tag == Democrats)
    val demResults = SimplePlurality(nVoters).run(
      ElectionDefinition(demCandidates,
        CombinedPopulation(Seq(primaryDems)),
        electionDef.config,
        electionDef.seed))

    val generalReps = electionDef.population.partyMap(Republicans)
    val primaryReps = generalReps.copy(mean = generalReps.mean + primarySkew)
    val repCandidates = electionDef.candidates.filter(c => c.party.tag == Republicans)
    val repResults = SimplePlurality(nVoters).run(ElectionDefinition(repCandidates,
      CombinedPopulation(Seq(primaryReps)),
      electionDef.config,
       electionDef.seed))

    val otherCandidates = electionDef.candidates.filter(c => c.party.tag != Democrats && c.party.tag != Republicans)
    val finalCandidates = otherCandidates :+
      demResults.orderedResults.head.candidate :+
      repResults.orderedResults.head.candidate

    val generalResults = SimplePlurality(nVoters).run(ElectionDefinition(finalCandidates,
      electionDef.population,
      electionDef.config,
       electionDef.seed))

    if (debug) {
      println("Democratic Primary:")
      demResults.orderedResults.foreach(cr => {
        println(f"${cr.candidate.name}%12s ${cr.candidate.ideology}% 5.1f ${cr.votes}%5d")
      })

      println("Republican Primary:")
      repResults.orderedResults.foreach(cr => {
        println(f"${cr.candidate.name}%12s ${cr.candidate.ideology}% 5.1f ${cr.votes}%5d")
      })

      println("General Election:")
      generalResults.orderedResults.foreach(cr => {
        println(f"${cr.candidate.name}%12s ${cr.candidate.ideology}% 5.1f ${cr.votes}%5d")
      })
    }
    ElectionWithPrimaryResult(this, demResults, repResults, generalResults)
  }
}

object ElectionWithPrimaryTest {
  def main(args: Array[String]): Unit = {
    val electionDefinition = DefaultElectionConfig()
    val election = ElectionWithPrimary(primarySkew = .5, debug = true)
    election.run(electionDefinition)
  }
}