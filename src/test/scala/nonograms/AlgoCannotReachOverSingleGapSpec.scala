package nonograms

import nonograms.algos.{AlgoCannotReachOverSingleGap, AlgoGapAssigner, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoCannotReachOverSingleGapSpec extends AnyFunSuite {
  test("- M M M M X M") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCannotReachOverSingleGap())), "- M M M M X M", "-MMMMMM")
  }

  test("- M M M - M -") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCannotReachOverSingleGap())), "- M M M - M X", "-MMMDM-")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoCannotReachOverSingleGap())), "- - X X M D - M M M X - - - M M M - M X", "----MD-MMM----MMMDM-")
  }
}
