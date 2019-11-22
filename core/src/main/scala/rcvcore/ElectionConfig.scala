package rcvcore

case class ElectionConfig(uncertainty: Double,
                          partyLoyalty: Double,
                          qualityScale: Double,
                          partyBonusScale: Double,
                          wastedVoteFactor: Double = 0.0) {

}
