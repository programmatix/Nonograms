package nonograms

import nonograms.algos.{AlgoGapAssigner, AlgoMarkObvious, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoGapAssignerSpec extends AnyFunSuite {
  test("will ignore these") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoGapAssigner())), "XXX", "---")
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoGapAssigner())), "-XX", "---")
  }

  test("- - - M M M D - - - - D - - D M - - D M") {
    // Gives clue 6  2  1  3  1
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoGapAssigner())), "X X X M M M D X X - X D - - D M X X D M", "MMMMMMD----D--DM--DM")
  }
}
