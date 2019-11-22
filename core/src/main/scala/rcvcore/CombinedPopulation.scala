package rcvcore

import scala.util.Random
case class CombinedPopulation(populations: Seq[PopulationGroup]) {
  val partyMap: Map[PopulationTag, PopulationGroup] = Map[PopulationTag, PopulationGroup](populations.map(p => (p.tag, p)): _*)
  val summedWeight: Double = populations.map(_.weight).sum

  def partyForTag(tag: PopulationTag): PopulationGroup = partyMap(tag)
  def partyForName(name: String): PopulationGroup = populations.find(p => p.name == name).get

  def weightedPopulation(): PopulationGroup = {
    var r = Random.nextDouble * summedWeight
    for (p <- populations) {
      if (r <= p.weight)
        return p
      else
        r -= p.weight
    }
    // should *never* get here.
    println("returning populations.last!!!")
    populations.last
  }


  var count = 0

  def randomVoter(config: ElectionConfig): Voter = {
    // count += 1
    // return populations(count % 3).randomVoter()
    val pop = weightedPopulation()
    pop.randomVoter(config)
  }

}
