package nonograms

import nonograms.algos._
import org.scalatest.funsuite.AnyFunSuite

class AlgoSpec extends AnyFunSuite {
  test("clear empty lines") {
    val board = TestUtils.createBasicBoard()
    val result = Solver.applyAlgo(AlgoClearEmptyLines(), board)

    // 2nd col
    assert (result.last.get(0,1) == SquareStateDeleted())
    assert (result.last.get(1,1) == SquareStateDeleted())
    assert (result.last.get(2,1) == SquareStateDeleted())

    // 3rd col
    assert (result.last.get(0,2) == SquareStateDeleted())
    assert (result.last.get(1,2) == SquareStateDeleted())
    assert (result.last.get(2,2) == SquareStateDeleted())

    // Bottom row
    assert (result.last.get(2,0) == SquareStateDeleted())
    assert (result.last.get(2,1) == SquareStateDeleted())
    assert (result.last.get(2,2) == SquareStateDeleted())
    assert (result.last.get(2,3) == SquareStateDeleted())

    assert (result.last.get(0,0) == SquareStateUntouched())
    assert (result.last.get(1,0) == SquareStateUntouched())

    assert (!BoardState.doesSolve(board, result.last).isSolved)
  }

  test("fill full row") {
    val input =
      """XXXX
        |OOOO
      """.stripMargin
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoFullLines(), board)

    assert (result.last.get(0,0) == SquareStateMarked())
    assert (result.last.get(0,1) == SquareStateMarked())
    assert (result.last.get(0,2) == SquareStateMarked())
    assert (result.last.get(0,3) == SquareStateMarked())

    assert (result.last.get(1,0) == SquareStateUntouched())
    assert (result.last.get(1,1) == SquareStateUntouched())
    assert (result.last.get(1,2) == SquareStateUntouched())
    assert (result.last.get(1,3) == SquareStateUntouched())

    assert (BoardState.doesSolve(board, result.last, ignoreUndeletedUntouched = true).isSolved)
  }

  test("fill full col") {
    val input =
      """OXOO
        |OXOO
      """.stripMargin
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoFullLines(), board)

    assert (result.last.get(0,0) == SquareStateUntouched())
    assert (result.last.get(0,1) == SquareStateMarked())
    assert (result.last.get(0,2) == SquareStateUntouched())
    assert (result.last.get(0,3) == SquareStateUntouched())

    assert (result.last.get(1,0) == SquareStateUntouched())
    assert (result.last.get(1,1) == SquareStateMarked())
    assert (result.last.get(1,2) == SquareStateUntouched())
    assert (result.last.get(1,3) == SquareStateUntouched())

    assert (BoardState.doesSolve(board, result.last, ignoreUndeletedUntouched = true).isSolved)
  }

  test("AlgoOnlyInnerSingleGaps 1") {
    val input = """XOX"""
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoOnlyInnerSingleGaps(), board)
    assert (BoardState.doesSolve(board, result.last).isSolved)
  }

  test("AlgoOnlyInnerSingleGaps 2") {
    val input = """XXXOXXX"""
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoOnlyInnerSingleGaps(), board)
    assert (BoardState.doesSolve(board, result.last).isSolved)
  }

  test("AlgoOnlyInnerSingleGaps 3") {
    val input = """XXXOXXXOX"""
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoOnlyInnerSingleGaps(), board)
    assert (BoardState.doesSolve(board, result.last).isSolved)
  }

  test("AlgoOnlyInnerSingleGaps 4") {
    val input =
      """X
        |O
        |X""".stripMargin
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoOnlyInnerSingleGaps(), board)
    assert (BoardState.doesSolve(board, result.last).isSolved)
  }

  test("AlgoOnlyInnerSingleGaps 5") {
    val input =
      """OX
        |OO
        |OX""".stripMargin
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoOnlyInnerSingleGaps(), board)
    assert (BoardState.doesSolve(board, result.last, ignoreUndeletedUntouched = true).isSolved)
  }


  test("AlgoMarkMiddle 1") {
    val input = """XXXO"""
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoMarkMiddleSingleClue(), board)

    assert (result.last.get(0,0) == SquareStateMarked()) // gets done by cols
    assert (result.last.get(0,1) == SquareStateMarked())
    assert (result.last.get(0,2) == SquareStateMarked())
    assert (result.last.get(0,3) == SquareStateUntouched())
  }

  test("AlgoMarkMiddle 2") {
    val input = """XXX"""
    val board = Board.create(input)
    val result = Solver.applyAlgo(AlgoMarkMiddleSingleClue(), board)

    assert (result.last.get(0,0) == SquareStateMarked())
    assert (result.last.get(0,1) == SquareStateMarked())
    assert (result.last.get(0,2) == SquareStateMarked())
  }

  test("AlgoStartOfClueAlreadyMarked 1") {
    val input = """XXO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,0)))
    val result = Solver.applyAlgoWithState(AlgoStartOfClueAlreadyMarked(), board, state)

    assert (result.last.get(0,0) == SquareStateMarked())
    assert (result.last.get(0,1) == SquareStateMarked())
    assert (result.last.get(0,2) == SquareStateUntouched())
  }

  test("AlgoStartOfClueAlreadyMarked col") {
    val input =
      """X
        |X
        |O""".stripMargin
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,0)))
    val result = Solver.applyAlgoWithState(AlgoStartOfClueAlreadyMarked(), board, state)

    assert (result.last.get(0,0) == SquareStateMarked())
    assert (result.last.get(1,0) == SquareStateMarked())
    assert (result.last.get(2,0) == SquareStateUntouched())
  }


  test("AlgoEndOfClueAlreadyMarked 1") {
    val input = """OOOOOOOOXX"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,9)))
    val result = Solver.applyAlgoWithState(AlgoStartOfClueAlreadyMarked(), board, state)

    assert (result.last.get(0,7) == SquareStateUntouched())
    assert (result.last.get(0,8) == SquareStateMarked())
    assert (result.last.get(0,9) == SquareStateMarked())
  }

  test("AlgoEndOfClueAlreadyMarked 2") {
    val input = """OOOOOOOXXX"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,9)))
    val result = Solver.applyAlgoWithState(AlgoStartOfClueAlreadyMarked(), board, state)

    assert (result.last.get(0,6) == SquareStateUntouched())
    assert (result.last.get(0,7) == SquareStateMarked())
    assert (result.last.get(0,8) == SquareStateMarked())
    assert (result.last.get(0,9) == SquareStateMarked())
  }


  test("AlgoNearStartOfClueAlreadyMarked 1") {
    val input = """OXXXOOOOOOOOOOOOOOOOO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,1)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoNearStartOfClueAlreadyMarked())), board, state)
    val bs = result.last

    assert (TestUtils.untd(bs,0,0))
    assert (TestUtils.mrkd(bs,0,1))
    assert (TestUtils.mrkd(bs,0,2))
    assert (TestUtils.untd(bs,0,3))
    assert (TestUtils.untd(bs,0,4))
  }

  test("AlgoNearStartOfClueAlreadyMarked no effect") {
    val input = """OOOOXXXOOOOOOOOOOOO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,4)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoNearStartOfClueAlreadyMarked())), board, state)
    val bs = result.last

    assert (TestUtils.untd(bs,0,0))
    assert (TestUtils.untd(bs,0,1))
    assert (TestUtils.untd(bs,0,2))
    assert (TestUtils.untd(bs,0,3))
    assert (TestUtils.mrkd(bs,0,4))
    assert (TestUtils.untd(bs,0,5))
    assert (TestUtils.untd(bs,0,6))
  }

  test("AlgoNearStartOfClueAlreadyMarked 2") {
    val input = """OOXXXOOOOOOOOOOOOOOOOO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,2)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoNearStartOfClueAlreadyMarked())), board, state)
    val bs = result.last

    assert (TestUtils.untd(bs,0,0))
    assert (TestUtils.untd(bs,0,1))
    assert (TestUtils.mrkd(bs,0,2))
    assert (TestUtils.untd(bs,0,3))
    assert (TestUtils.untd(bs,0,4))
  }

  test("AlgoNearStartOfClueAlreadyMarked 3") {
    val input = """OOXXXXOOOOOOOOOOOOOOOO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,2)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoNearStartOfClueAlreadyMarked())), board, state)
    val bs = result.last

    assert (TestUtils.untd(bs,0,0))
    assert (TestUtils.untd(bs,0,1))
    assert (TestUtils.mrkd(bs,0,2))
    assert (TestUtils.mrkd(bs,0,3))
    assert (TestUtils.untd(bs,0,4))
  }

  test("AlgoNearStartOfClueAlreadyMarked 4") {
    val input = """OOOOOOOOXXXXOO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,11)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoNearStartOfClueAlreadyMarked())), board, state)
    val bs = result.last

    assert (TestUtils.untd(bs,0,8))
    assert (TestUtils.untd(bs,0,9))
    assert (TestUtils.mrkd(bs,0,10))
    assert (TestUtils.mrkd(bs,0,11))
    assert (TestUtils.untd(bs,0,12))
    assert (TestUtils.untd(bs,0,13))
  }

  test("AlgoNearStartOfClueAlreadyMarked col") {
    val input =
      """O
        |O
        |O
        |O
        |O
        |O
        |O
        |O
        |X
        |X
        |X
        |X
        |O
        |O""".stripMargin
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((11,0)))
    val result = Solver.applyAlgoWithState(AlgoMinis(Seq(AlgoNearStartOfClueAlreadyMarked())), board, state)
    val bs = result.last

    assert (TestUtils.untd(bs,8,0))
    assert (TestUtils.untd(bs,9,0))
    assert (TestUtils.mrkd(bs,10,0))
    assert (TestUtils.mrkd(bs,11,0))
    assert (TestUtils.untd(bs,12,0))
    assert (TestUtils.untd(bs,13,0))
  }

  test("AlgoDeleteRemainingFromSolvedLines 1") {
    val input = """XOX"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,0), (0,2)))
    val result = Solver.applyAlgoWithState(AlgoDeleteRemainingFromSolvedLines(), board, state)
    val bs = result.last

    assert (TestUtils.mrkd(bs,0,0))
    assert (TestUtils.deld(bs,0,1))
    assert (TestUtils.mrkd(bs,0,2))
  }

  test("AlgoDeleteImpossibleSingle 1") {
    val input = """OOOXXXOOOO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,3)))
    val result = Solver.applyAlgoWithState(AlgoDeleteImpossibleSingle(), board, state)
    val bs = result.last

    assert (TestUtils.deld(bs,0,0))
    assert (TestUtils.untd(bs,0,1))
    assert (TestUtils.untd(bs,0,2))
    assert (TestUtils.mrkd(bs,0,3))
    assert (TestUtils.untd(bs,0,4))
    assert (TestUtils.untd(bs,0,5))
    assert (TestUtils.deld(bs,0,6))
    assert (TestUtils.deld(bs,0,9))
  }

  test("AlgoDeleteImpossibleSingle 2") {
    val input = """OOOOOOOOXX"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,8)))
    val result = Solver.applyAlgoWithState(AlgoDeleteImpossibleSingle(), board, state)
    val bs = result.last

    assert (TestUtils.deld(bs,0,0))
    assert (TestUtils.deld(bs,0,1))
    assert (TestUtils.deld(bs,0,2))
    assert (TestUtils.deld(bs,0,3))
    assert (TestUtils.deld(bs,0,4))
    assert (TestUtils.deld(bs,0,5))
    assert (TestUtils.deld(bs,0,6))
    assert (TestUtils.untd(bs,0,7))
    assert (TestUtils.mrkd(bs,0,8))
    assert (TestUtils.untd(bs,0,9))
  }


  test("AlgoDeleteImpossibleSingle 3") {
    val input = """OOOOOOOOXO"""
    val board = Board.create(input)
    val state = TestUtils.mark(board, Vector((0,8)))
    val result = Solver.applyAlgoWithState(AlgoDeleteImpossibleSingle(), board, state)
    val bs = result.last

    assert (TestUtils.deld(bs,0,0))
    assert (TestUtils.deld(bs,0,1))
    assert (TestUtils.deld(bs,0,2))
    assert (TestUtils.deld(bs,0,3))
    assert (TestUtils.deld(bs,0,4))
    assert (TestUtils.deld(bs,0,5))
    assert (TestUtils.deld(bs,0,6))
    assert (TestUtils.deld(bs,0,7))
    assert (TestUtils.mrkd(bs,0,8))
    assert (TestUtils.deld(bs,0,9))
  }

  test("AlgoOnlyInnerSingleGapsAfterDeletesAtEdges 1") {
    val (board, state) = TestUtils.create("""DX-XX""")
    val result = Solver.applyAlgoWithState(AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(), board, state)
    val bs = result.last

    assert (TestUtils.deld(bs,0,0))
    assert (TestUtils.mrkd(bs,0,1))
    assert (TestUtils.deld(bs,0,2))
    assert (TestUtils.mrkd(bs,0,3))
    assert (TestUtils.mrkd(bs,0,4))
  }

  test("AlgoOnlyInnerSingleGapsAfterDeletesAtEdges 2") {
    val (board, state) = TestUtils.create("""X-XXD""")
    val result = Solver.applyAlgoWithState(AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(), board, state)
    val bs = result.last

    assert (TestUtils.mrkd(bs,0,0))
    assert (TestUtils.deld(bs,0,1))
    assert (TestUtils.mrkd(bs,0,2))
    assert (TestUtils.mrkd(bs,0,3))
    assert (TestUtils.deld(bs,0,4))
  }

  test("AlgoOnlyInnerSingleGapsAfterDeletesAtEdges 3") {
    val (board, state) = TestUtils.create("""X-XX""")
    val result = Solver.applyAlgoWithState(AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(), board, state)
    val bs = result.last

    assert (TestUtils.mrkd(bs,0,0))
    assert (TestUtils.deld(bs,0,1))
    assert (TestUtils.mrkd(bs,0,2))
    assert (TestUtils.mrkd(bs,0,3))
  }

  test("AlgoOnlyInnerSingleGapsAfterDeletesAtEdges 4") {
    val (board, state) = TestUtils.create("""XDDX-X--XX""")
    val result = Solver.applyAlgoWithState(AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(), board, state)
    assert(result.last == state)
  }

  test("AlgoMarkObviousRemaining 1") {
    val (board, state) = TestUtils.create("""DX-MM""")
    val result = Solver.applyAlgoWithState(AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(), board, state)
    val bs = result.last

    assert (TestUtils.mrkd(bs,0,1))
  }

  test("AlgoMarkObviousRemaining 2") {
    val (board, state) = TestUtils.create("""DX-XX""")
    val result = Solver.applyAlgoWithState(AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(), board, state)
    val bs = result.last

    assert (TestUtils.mrkd(bs,0,1))
    assert (TestUtils.mrkd(bs,0,3))
    assert (TestUtils.mrkd(bs,0,4))
  }

  test("AlgoDeleteNextToCompletedRanges biggest") {
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "-M---", "DMD--")
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "M----", "MD---")
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "----M", "---DM")
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "--MMM--", "-DMMMD-")
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "M--MM--", "M-DMMD-")
  }

  test("AlgoDeleteNextToCompletedRanges multiple all same") {
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "-M--M--", "DMDDMD-")
    TestUtils.testAlgo(AlgoDeleteNextToCompletedRanges(), "-MM--MM--", "DMMDDMMD-")
  }

  test("AlgoDeleteNextToCompletedRanges untouched") {
    TestUtils.testAlgoUntouched(AlgoDeleteNextToCompletedRanges(), "DXM-D")
  }

  test("AlgoEachCluedStretchHasOwnRange simple") {
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "-MDXM", "DMDMM")
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "-M---", "DMDDD")
  }

  test("AlgoEachCluedStretchHasOwnRange start/end range marked") {
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "MXX--D", "MMMDDD")
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "--XXMD", "DDMMMD")
  }

  test("AlgoEachCluedStretchHasOwnRange near start/end range marked") {
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "DD--MXXX--------D", "DD--MM--DDDDDDDDD")
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "--------XXXM--D", "DDDDDDDD--MM--D")
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "--------XXXM--DDD", "DDDDDDDD--MM--DDD")
    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "--MXXX--------D", "--MM--DDDDDDDDD")
  }

//  test("AlgoEachCluedStretchHasOwnRange nothing marked") {
//    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "-XXD", "-M-D")
//    TestUtils.testAlgo(AlgoEachCluedStretchHasOwnRange(), "--XXXD", "--M--D")
//  }

  test("AlgoEachCluedStretchHasOwnRange untouched") {
    TestUtils.testAlgoUntouched(AlgoEachCluedStretchHasOwnRange(), "---MMXXX")
    TestUtils.testAlgoUntouched(AlgoEachCluedStretchHasOwnRange(), "-MDXX")
    TestUtils.testAlgoUntouched(AlgoEachCluedStretchHasOwnRange(), "-MX--")
  }
}
