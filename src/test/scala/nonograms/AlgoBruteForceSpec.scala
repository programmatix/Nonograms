package nonograms

import nonograms.algos.AlgoBruteForce
import org.scalatest.FunSuite

class AlgoBruteForceSpec extends FunSuite {
  test("1x1") {
    TestUtils.testAlgo(AlgoBruteForce(), "O", "D")
    TestUtils.testAlgo(AlgoBruteForce(), "X", "M")
  }

  test("1x2 1") {
    assert(TestUtils.testBruteForceDelete("OX",0,0))
  }

  test("1x2") {
    TestUtils.testAlgo(AlgoBruteForce(), "OX", "DM")
    TestUtils.testAlgo(AlgoBruteForce(), "OO", "DD")
    TestUtils.testAlgo(AlgoBruteForce(), "XX", "MM")
    TestUtils.testAlgo(AlgoBruteForce(), "XO", "MD")
  }
}
