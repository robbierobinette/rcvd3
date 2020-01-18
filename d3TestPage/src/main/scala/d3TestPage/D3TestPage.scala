package D3TestPage

import d3TestPage.IdeologyColorPicker
import d3v4._
import d3v4.d3force.Simulation
import org.scalajs.dom
import org.scalajs.dom.{Event, MouseEvent, UIEvent, Window, raw}
import rcvcore.{Candidate, DefaultElectionConfig, ElectionConfig, ElectionDefinition, PopulationTag, RCVBallot, RCVElection, RCVResult, RCVRoundResult, Voter}

import scala.scalajs.js
import js.JSConverters._
import scala.collection.mutable
import scala.scalajs.js.UndefOr

case class BoundingBox(top: Double, bottom: Double, left: Double, right: Double) {
  def xCenter: Double = (right + left) / 2

  def adjustX(b: SimulationNode): js.UndefOr[Double] = {
    if (b.x.isEmpty) return b.x
    val x = b.x.get
    if (x <= left && left - x <= 5)
      b.x = left
    if (x >= right && x - right <= 5)
      b.x = right
    b.x
  }

  def adjustY(b: SimulationNode): js.UndefOr[Double] = {
    if (b.y.isEmpty) return b.y
    val y = b.y.get
    if (y <= top && top - y <= 5)
      b.y = top
    if (y >= bottom && y - bottom <= 5)
      b.y = bottom
    b.y
  }
}


case class RCVElectionDisplay(electionDef: ElectionDefinition) {

  case class VoterBubble(voter: Voter, radius: Double, ballot: RCVBallot) extends SimulationNodeImpl {

  }

  private val numNodes = 200
  private val voters = (0 until numNodes).map(_ => createVoter)
  private val rounds = computeRounds(voters.map(_.ballot))


  def computeRounds(ballots: Seq[RCVBallot]): Seq[RCVRoundResult] = {
    val nVoters = ballots.length
    var rounds = Seq[RCVRoundResult]()
    var activeCandidates = Set(electionDef.candidates: _*)
    do {
      val round = computeRoundResult(ballots, activeCandidates, electionDef.candidates)
      rounds = rounds :+ round
      activeCandidates = activeCandidates - round.orderedResults.last.candidate
    } while (rounds.last.orderedResults.head.votes.toDouble / nVoters < .5)
    rounds
  }


  def computeRoundResult(ballots: Seq[RCVBallot], activeCandidates: Set[Candidate], candidates: Seq[Candidate]): RCVRoundResult = {
    val results: mutable.Map[Candidate, Int] = mutable.Map[Candidate, Int]()
    for (b <- ballots) {
      results(b.candidate(activeCandidates)) = results.getOrElse(b.candidate(activeCandidates), 0) + 1
    }
    RCVRoundResult(candidates, activeCandidates, results)
  }


  def createVoter: VoterBubble = {
    val v = electionDef.randomVoter
    val b = v.ballot(electionDef.candidates)
    VoterBubble(v, 5, b)
  }

  private val simulation = createVoters(electionDef)

  def createVoters(electionDef: ElectionDefinition): Simulation[VoterBubble] = {
    val simulation = d3.forceSimulation(voters.toJSArray)
      .force("x", d3.forceX().x((b: VoterBubble) => 400.0).strength(.15))
      .force("collision", d3.forceCollide().radius((b: VoterBubble) => b.radius))
      .on("tick", ticked _)
    simulation
  }

  def ticked(sim: Simulation[VoterBubble]): Unit = {
    val u = d3.select("#viz")
      .selectAll("circle")
      .data(sim.nodes())

    u.enter()
      .append("circle")
      .attr("r", (b: VoterBubble) => b.radius)
      .style("fill", (b: VoterBubble) => IdeologyColorPicker(b.voter.ideologyScore))
      .style("opacity", ".8")
      .merge(u)
      .attr("cx", (b: VoterBubble) => b.x)
      .attr("cy", (b: VoterBubble) => b.y)
    u.exit().remove()
  }

  def setCenters(stage: Int): Unit = {
    println(s"setCenter: stage $stage")
    if (stage == 0) {
      simulation.force("x", d3.forceY(100).strength(.1))
      simulation.force("y", d3.forceY(100).strength(.1))
    }
    else {
      val roundIndex = stage - 1
      val round = rounds(roundIndex)
      val nCandidates = round.candidates.length
      val width = 800
      val colWidth = width / (nCandidates + 1)
      val centers = (0 until nCandidates).map(i => colWidth * i + colWidth / 2)
      simulation.force("y", d3.forceY(500).strength (.005))
      simulation.force("x", d3.forceX().x((v: VoterBubble) => {
        val c = v.ballot.candidate(round.activeCandidates)
        val newX = centers(round.candidates.indexOf(c))
        println(f"setting candidate for voter ${v.voter.ideologyScore}%.2f to ${c.name}:${c.ideology}%.2f newX $newX")
        newX
      }
      ))
    }
    simulation.alpha(1).alphaTarget(.3).alphaMin(.1).alphaDecay(.001).restart()
  }
}

object D3TestPage {
  private var lastSection = 0
  private var sproing = false


  private val electionDef = DefaultElectionConfig()

  private val display: RCVElectionDisplay = RCVElectionDisplay(electionDef)
  display.setCenters(0)

  def main(args: Array[String]): Unit = {
    val svg = d3.select("viz")
    d3.select(dom.window).on("scroll", () => onScroll("window"))
  }


  def onScroll(m: String): Unit = {
    val yOffset = dom.window.pageYOffset
    val section_dim = dom.document.getElementById("sections").getBoundingClientRect()
    val sectionsHeight = section_dim.bottom - section_dim.top
    val windowHeight = dom.window.innerHeight
    val scrollAmount = (sectionsHeight - windowHeight) / 9
    val section = yOffset.toInt / scrollAmount.toInt
    println(s"$m: onScroll called:  Offset $yOffset sectionsHeight $sectionsHeight windowHeight $windowHeight")
    if (section != lastSection) {
      println(s"setting section to $section!")
      display.setCenters(section)
      lastSection = section
    }
  }

  def onClick(m: String): Unit = {
    println(s"$m: onClick called!!!")
  }

  //  def setFlip(section: Int): Unit = {
  //    flip = section
  //    sproing = true
  //    simulation
  //      .force("x", d3.forceX().x((b: Bubble) => {
  //        println(s"setFlip: b.category ${b.category}, b.xCenter: ${b.xCenter} ")
  //        b.xCenter
  //      }))
  //    simulation.alpha(1).restart()
  //  }
  //
  //
  //  def flipCenters(): Unit = {
  //    flip += 1
  //    println(f"flipCenters $flip%d")
  //    sproing = true
  //    simulation
  //      .force("x", d3.forceX().x((b: Bubble) => {
  //        println(s"flipCenters: b.category ${b.category}, b.xCenter: ${b.xCenter} ")
  //        b.xCenter
  //      }))
  //    simulation.alpha(1).alphaTarget(.1).alphaMin(.1).alphaDecay(.001).restart()
  //  }


}
