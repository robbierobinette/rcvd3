package rcvcore


case class DVRParser(text: String) {
  lazy val votingRecords: Seq[DistrictVotingRecord] = {
    val lines = text.split("\n")
    val header = lines.head

    println(s"header: $header")
    println(s"got ${lines.tail.length} records")
    lines.tail.flatMap(l => parseLine(l))
  }
  def parseLine(l: String): Option[DistrictVotingRecord] = {
    try {
      val fields = l.split(",")
      val name = fields(0)
      val incumbent = fields(1)
      val dPct1 = fields(3).toDouble
      val rPct1 = fields(4).toDouble
      val (dPct2, rPct2) = if (fields(5) == "null") {
        (dPct1, rPct1)
      }
      else {
        (fields(5).toDouble, fields(6).toDouble)
      }
      Some(DistrictVotingRecord(name, incumbent, dPct1, rPct1, dPct2, rPct2))
    }
    catch {
      case _: Exception =>
        println(s"Unable to parse $l")
        None
    }
  }
}
