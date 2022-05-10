package nonograms

import nonograms.algos.{AlgoCheckUnambiguousClueNearStart, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoMinisSpec extends AnyFunSuite {
  test("checkUnambiguousClueNearEdge") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCheckUnambiguousClueNearStart())), "--MX---M--", "D-M----M--")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCheckUnambiguousClueNearStart())), "-M-----M--", "DMD----M--")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCheckUnambiguousClueNearStart())), "-M-----M--".reverse, "DMD----M--".reverse)
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCheckUnambiguousClueNearStart())), "-M--------".reverse, "DMDDDDDDDD".reverse)
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCheckUnambiguousClueNearStart())), "-M--------", "DMDDDDDDDD")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCheckUnambiguousClueNearStart())), "--MX---M--".reverse, "D-M----M--".reverse)
  }

}
