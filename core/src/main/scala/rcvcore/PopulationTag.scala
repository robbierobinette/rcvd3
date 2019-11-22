package rcvcore

trait PopulationTag {
  def shortName: String
  def name: String
  def pluralName: String
  def id: Int
  val loyalty: Seq[Double]
  def partyLoyalty(o: PopulationTag) = loyalty(o.id)
  override def toString = name
}

case object Republicans extends PopulationTag {
  def shortName = "rep"
  def name = "Republican"
  def pluralName = "Republicans"
  def id = 0
  val loyalty = Seq[Double](1.0, 0.0, 0.5)
}

case object Democrats extends PopulationTag {
  def shortName = "dem"
  def name = "Democratic"
  def pluralName = "Democrats"
  def id = 1
  val loyalty = Seq[Double](0.0, 1.0, 0.5)
}
case object Independents extends PopulationTag {
  def shortName = "ind"
  def name = "Independent"
  def pluralName = "Independents"
  def id = 2
  val loyalty = Seq[Double](0.0, 0.0, 0.0)
}
