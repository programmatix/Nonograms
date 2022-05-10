package nonograms

import org.scalatest.funsuite.AnyFunSuite

class BoardSpec extends AnyFunSuite {
  private def createBasicBoard() = TestUtils.createBasicBoard()

  test("create") {
    val input =
      """XOOO
        |OOOX
        |OOOO
      """.stripMargin
    val board = Board.create(input)
    assert(board.isFilled(0, 0))
    assert(!board.isFilled(0, 1))
    assert(board.isFilled(1, 3))
    assert(!board.isFilled(2, 3))
    assert(board.numCols == 4)
    assert(board.numRows == 3)
  }

  test("init") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)
    assert(state.get(0, 0) == SquareStateUntouched())
    assert(state.get(board.numRows() - 1, board.numCols() - 1) == SquareStateUntouched())
  }

  test("delete unfilled") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)
    Board.attempt(ActionDelete(1, 2), board, state) match {
      case v: ActionSuccess =>
        assert(v.newState.get(1, 2) == SquareStateDeleted())
      case v: ActionFailure => assert(false)
    }
  }

  test("delete filled") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)
    Board.attempt(ActionDelete(1, 3), board, state) match {
      case v: ActionSuccess => assert(false)
      case v: ActionFailure =>
    }
  }

  test("mark filled") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)
    Board.attempt(ActionMark(1, 3), board, state) match {
      case v: ActionSuccess =>
        assert(v.newState.get(1, 3) == SquareStateMarked())
      case v: ActionFailure => assert(false)
    }
  }

  test("mark unfilled") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)
    Board.attempt(ActionMark(1, 2), board, state) match {
      case v: ActionSuccess => assert(false)
      case v: ActionFailure =>
    }
  }


  test("solve doesn't change state") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)

    assert(state.get(0, 0).isInstanceOf[SquareStateUntouched])

    Board.attempt(ActionDelete(0, 0), board, state)

    assert(state.get(0, 0).isInstanceOf[SquareStateUntouched])
  }

  test("consecutive solves") {
    val board = createBasicBoard()
    val state = BoardState.initialise(board)

    val state1 = Board.attempt(ActionDelete(0, 1), board, state).asInstanceOf[ActionSuccess].newState

    assert(state1.get(0, 1).isInstanceOf[SquareStateDeleted])

    val state2 = Board.attempt(ActionDelete(1, 2), board, state1).asInstanceOf[ActionSuccess].newState

    assert(state2.get(1, 2).isInstanceOf[SquareStateDeleted])
  }

  test("solved") {
    val board = createBasicBoard()
    val state = BoardState.initialiseWithClearedEmpties(board)

    val state1 = Board.attempt(ActionMark(0, 0), board, state).asInstanceOf[ActionSuccess].newState
    val state2 = Board.attempt(ActionMark(1, 3), board, state1).asInstanceOf[ActionSuccess].newState

    assert(!BoardState.doesSolve(board, state).isSolved)
    assert(!BoardState.doesSolve(board, state1).isSolved)
    assert(BoardState.doesSolve(board, state2).isSolved)
  }

  test("areAllDeletedAtEdges") {
    def check(input: String) = {
      val (_, state) = TestUtils.create(input)
      state.getRow(0).areAllDeletedAtEdges()
    }

    assert(!check("-DD----"))
    assert(!check("----DD-"))
    assert(!check("-D-D-"))
    assert(!check("-D-"))
    assert(check("D-D"))
    assert(check("--D"))
    assert(check("---"))
    assert(check("-DD"))
    assert(!check("-D-DD"))
    assert(check("DD-DD"))
    assert(check("DDDDD"))
  }

  test("getRanges") {
    def check(input: String) = {
      val (_, state) = TestUtils.create(input)
      state.getRow(0).getRanges()
    }

    assert(check("-D---D-") == Vector(1, 3, 1))
    assert(check("MD-M-DM") == Vector(1, 3, 1))
    assert(check("-D-D-") == Vector(1, 1, 1))
    assert(check("-D-") == Vector(1, 1))
    assert(check("D-D") == Vector(1))
    assert(check("--D") == Vector(2))
    assert(check("---") == Vector(3))
    assert(check("-DD") ==  Vector(1))
    assert(check("-D-DD") == Vector(1,1))
    assert(check("DD-DD") == Vector(1))
    assert(check("DDDDD") == Vector())
    assert(check("D-DMM") == Vector(1,2))
  }

  test("getMarkRanges") {
    def check(input: String) = {
      val (_, state) = TestUtils.create(input)
      state.getRow(0).getMarkRanges()
    }

    val r = check("MD-MMDM")

    assert(r(0).fromIdx == 0)
    assert(r(0).toIdx == 0)
    assert(r(0).marked == 1)
    assert(r(1).fromIdx == 2)
    assert(r(1).toIdx == 4)
    assert(r(1).marked == 2)
    assert(r(2).fromIdx == 6)
    assert(r(2).toIdx == 6)
    assert(r(2).marked == 1)
  }

  test("getMarkRanges no marks") {
    def check(input: String) = {
      val (_, state) = TestUtils.create(input)
      state.getRow(0).getMarkRanges()
    }

    val r = check("-D--MD-")

    assert(r(0).fromIdx == 0)
    assert(r(0).toIdx == 0)
    assert(r(0).marked == 0)
    assert(r(1).fromIdx == 2)
    assert(r(1).toIdx == 4)
    assert(r(1).marked == 1)
    assert(r(2).fromIdx == 6)
    assert(r(2).toIdx == 6)
    assert(r(2).marked == 0)
  }

  test("getMarkedStretches") {
    def check(input: String) = {
      val (_, state) = TestUtils.create(input)
      state.getRow(0).getMarkedStretches().map(_.len()).toVector
    }

    assert(check("-D---D-") == Vector())
    assert(check("MD-M-DM") == Vector(1, 1, 1))
    assert(check("MMMD-M-DM") == Vector(3, 1, 1))
    assert(check("MMM--MM-DM") == Vector(3, 2, 1))
    assert(check("--MMM--MM-DM--") == Vector(3, 2, 1))
    assert(check("D-DMM") == Vector(2))
  }


  test("flip") {
    val board = TestUtils.create("-M--")._2
    val flipped = board.flipHorizontally()
    assert (TestUtils.mrkd(flipped,0,2))
  }

  test("flip vertically") {
    val board = TestUtils.create(
      """-M--
        |---M""".stripMargin)._2
    val flipped = board.flipVertically()
    assert (TestUtils.untd(flipped,0,0))
    assert (TestUtils.untd(flipped,0,1))
    assert (TestUtils.untd(flipped,0,2))
    assert (TestUtils.mrkd(flipped,0,3))
    assert (TestUtils.untd(flipped,1,0))
    assert (TestUtils.mrkd(flipped,1,1))
    assert (TestUtils.untd(flipped,1,2))
    assert (TestUtils.untd(flipped,1,3))
  }
}
