package nonograms

import nonograms.algos.{AlgoCheckUnambiguousClueNearStart, AlgoMarkObvious, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoMarkObviousSpec extends AnyFunSuite {
  test("AlgoMarkObvious") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoMarkObvious())), "XXX", "MMM")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoMarkObvious())), "-XX", "-M-")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoMarkObvious())), "XX-XXXXXXX", "MM-MMMMMMM")
    // Will give clue of [2,6] on row of 10
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoMarkObvious())), "-XX-XXXXXX", "-M--MMMMM-")
  }

  test("- - - M M M D - - - - D - - D M - - D M") {
    // Gives clue 6  2  1  3  1
    // Cannot do anything with this one, obvious squares are marked
//    TestUtils.testAlgo(AlgoMinis(Seq(AlgoMarkObvious())), "X X X M M M D X X - X D - - D M X X D M", "MMMMMMD----D--DM--DM")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoMarkObvious())), "X X X M M M D X X - X D - - D M X X D M", "---MMMD----D--DM--DM")
  }
}
