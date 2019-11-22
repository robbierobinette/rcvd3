package rcvcore

trait ElectionProcess {
  def run(electionDefinition: ElectionDefinition):  ElectionResult
  def name: String
}
