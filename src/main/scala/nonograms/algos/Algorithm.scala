package nonograms.algos

import nonograms._

case class SolverParams(allowPruning: Boolean = true)

// Note, although the board is passed in, no algorithm is allowed to peek.  Algos have to solve in the same way a human
// could
case class ForSolver(board: Board, clues: Clues, state: BoardState, params: SolverParams = SolverParams()) {
  def flipHorizontally(): ForSolver = {
    ForSolver(
      board.flipHorizontally(),
      clues.flipHorizontally(),
      state.flipHorizontally()
    )
  }

  def flipVertically(): ForSolver = {
    ForSolver(
      board.flipVertically(),
      clues.flipVertically(),
      state.flipVertically()
    )
  }
}

case class SolverResult(last: BoardState, bruteForceMaxLevel: Int = 0)

trait Algorithm {
  def solve(s: ForSolver): SolverResult
}


object Algorithm {
  def deleteRow(row: Int, board: Board, curState: BoardState): BoardState = {
    var retState = curState

    for (col <- Range(0, board.numCols())) {
      if (!curState.isNotMarkedOrDeleted(row, col)) {
        Board.attempt(ActionDelete(row, col), board, retState) match {
          case v: ActionSuccess =>
            retState = v.newState
          case _                =>
            // Should succeed
            assert(false)
        }
      }
    }

    retState
  }

  def deleteCol(col: Int, board: Board, curState: BoardState): BoardState = {
    var retState = curState

    for (row <- Range(0, board.numRows())) {
      if (!curState.isNotMarkedOrDeleted(row, col)) {
        Board.attempt(ActionDelete(row, col), board, retState) match {
          case v: ActionSuccess =>
            retState = v.newState
          case _                =>
            // Should succeed
            assert(false)
        }
      }
    }

    retState
  }

  def delete(row: Int, col: Int, board: Board, curState: BoardState, allowBadMoves: Boolean = false): BoardState = {
    if (curState.isNotMarkedOrDeleted(row, col)) {
      Board.attempt(ActionDelete(row, col), board, curState, allowBadMoves) match {
        case v: ActionSuccess =>
          v.newState
        case _                =>
          // Should succeed
          assert(false, s"Failed to delete ${row},${col}")
          curState
      }
    }
    else {
      curState
    }
  }



  def markRow(row: Int, board: Board, curState: BoardState): BoardState = {
    markRowRange(row, 0, board.numCols(), board, curState)
  }

  def mark(row: Int, col: Int, board: Board, curState: BoardState, allowBadMoves: Boolean = false): BoardState = {
    if (row >= board.numRows()) {
      assert(false, s"Bad row $row, max ${board.numRows()}")
    }
    if (col >= board.numCols()) {
      assert(false, s"Bad col $col, max ${board.numCols()}")
    }

    if (curState.isNotMarkedOrDeleted(row, col)) {
      Board.attempt(ActionMark(row, col), board, curState, allowBadMoves) match {
        case v: ActionSuccess =>
          v.newState
        case _                =>
          // Should succeed
          assert(false)
          curState
      }
    }
    else {
      curState
    }
  }

  // colEnd is not-exclusive
  def markRowRange(row: Int, colStart: Int, colEnd: Int, board: Board, curState: BoardState): BoardState = {
    var retState = curState

    for (col <- Range(colStart, colEnd)) {
      if (curState.isNotMarkedOrDeleted(row, col)) {
        Board.attempt(ActionMark(row, col), board, retState) match {
          case v: ActionSuccess =>
            retState = v.newState
          case _                =>
            // Should succeed
            assert(false)
        }
      }
    }

    retState
  }

  def markCol(col: Int, board: Board, curState: BoardState): BoardState = {
    markColRange(col, 0, board.numRows(), board, curState)
  }

  // rowEnd is not-exclusive
  def markColRange(col: Int, rowStart: Int, rowEnd: Int, board: Board, curState: BoardState): BoardState = {
    var retState = curState

    for (row <- Range(rowStart, rowEnd)) {
      if (curState.isNotMarkedOrDeleted(row, col)) {
        Board.attempt(ActionMark(row, col), board, retState) match {
          case v: ActionSuccess =>
            retState = v.newState
          case _                =>
            // Should succeed
            assert(false)
        }
      }
    }

    retState
  }
}
