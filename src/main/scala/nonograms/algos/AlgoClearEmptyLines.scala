package nonograms.algos

import nonograms._

import scala.collection.mutable.ArrayBuffer

// Get rid of blank lines to reduce search space quickly
case class AlgoClearEmptyLines() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    for (row <- Range(0, s.board.numRows())) {
      val clue = s.clues.getRow(row)
      if (clue.isEmpty()) {

        for (col <- Range(0, s.board.numCols())) {
          Board.attempt(ActionDelete(row, col), s.board, curState) match {
            case v: ActionSuccess =>
              if (!out.contains(v.newState)) {
                out += v.newState
              }
              curState = v.newState
            case _                =>
              // Should succeed
              assert(false)
          }
        }
      }
    }

    for (col <- Range(0, s.board.numCols())) {
      val clue = s.clues.getCol(col)
      if (clue.isEmpty()) {

        for (row <- Range(0, s.board.numRows())) {
          Board.attempt(ActionDelete(row, col), s.board, curState) match {
            case v: ActionSuccess =>
              if (!out.contains(v.newState)) {
                out += v.newState
              }
              curState = v.newState
            case _                =>
              // Should succeed
              assert(false)
          }
        }
      }

    }

    SolverResult(curState)
  }
}
