package nonograms

import nonograms.algos.{AlgoCannotReachOverSingleGap, AlgoDeleteNextToCompletedLargestClues, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoDeleteNextToCompletedLargestCluesSpec extends AnyFunSuite {
  test("start") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoDeleteNextToCompletedLargestClues())), "MMM----", "MMMD---")
  }

  test("end") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoDeleteNextToCompletedLargestClues())), "----MMM", "---DMMM")
  }

  test("middle") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoDeleteNextToCompletedLargestClues())), "--MMM--", "-DMMMD-")
  }

  test("two") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoDeleteNextToCompletedLargestClues())), "--MMM--MMM--", "-DMMMDDMMMD-")
  }
}
