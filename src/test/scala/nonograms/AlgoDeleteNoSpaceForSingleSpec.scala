package nonograms

import nonograms.algos.{AlgoDeleteEdgesIfNoSpaceInThereForFirstClue, AlgoDeleteNoSpaceForSingle, AlgoMinis}
import org.scalatest.funsuite.AnyFunSuite

class AlgoDeleteNoSpaceForSingleSpec extends AnyFunSuite {
  test("- - D M M M M M M M M M M M M D M M D -  1  12  2") {
    TestUtils.testAlgo(AlgoMinis(Seq(AlgoDeleteNoSpaceForSingle())), "- X D M M M M M M M M M M M M D M M D -", "- - D M M M M M M M M M M M M D M M D D")
  }


  test("AlgoDeleteNoSpaceForSingle 1") {
    val input = """OOOOOOXXXX"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector(), Vector((0,3)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoDeleteNoSpaceForSingle())), board, state)
    val bs = result.last

    assert (TestUtils.deld(bs,0,0))
    assert (TestUtils.deld(bs,0,1))
    assert (TestUtils.deld(bs,0,2))
    assert (TestUtils.deld(bs,0,3))
    assert (TestUtils.untd(bs,0,4))
    assert (TestUtils.untd(bs,0,5))
  }


  test("AlgoDeleteNoSpaceForSingle 4") {
    val (board, state) = TestUtils.create("--DXX")
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoDeleteNoSpaceForSingle())), board, state)
    // Untouched
    assert (result.last == state)
  }

  test("AlgoDeleteNoSpaceForSingle 5") {
    val (board, state) = TestUtils.create("XXX----D--")
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoDeleteNoSpaceForSingle())), board, state)
    assert (result.last == TestUtils.create("XXX----DDD")._2)
  }

}
