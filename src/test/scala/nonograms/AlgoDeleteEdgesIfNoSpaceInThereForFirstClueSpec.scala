package nonograms

import nonograms.algos.{AlgoDeleteEdgesIfNoSpaceInThereForFirstClue, AlgoGapAssigner, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoDeleteEdgesIfNoSpaceInThereForFirstClueSpec extends AnyFunSuite {
  test("--DXXX") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoDeleteEdgesIfNoSpaceInThereForFirstClue())), "--DXXX", "DDD---")
  }
}
