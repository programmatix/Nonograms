package nonograms.algos

import nonograms._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// "2,1" "MM--M-"   -> "MMDDMD"
case class AlgoDeleteRemainingFromSolvedLines() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (!line.isComplete()) {
        if (clues.totalFilled() == line.totalMarked()) {
          for (idx <- Range(0, lineLength)) {
            if (line.squares(idx).isUntouched()) {
              if (row.isDefined) {
                handle(Algorithm.delete(row.get, idx, s.board, curState))
              }
              else {
                handle(Algorithm.delete(idx, col.get, s.board, curState))
              }
            }
          }
        }
      }
    }

    for (row <- Range(0, s.board.numRows())) {
      val clues = s.clues.getRow(row)
      val solved = s.state.getRow(row)
      doLine(clues, solved, Some(row), None, s.board.numCols())
    }

    for (col <- Range(0, s.board.numCols())) {
      val clues = s.clues.getCol(col)
      val solved = s.state.getCol(col)
      doLine(clues, solved, None, Some(col), s.board.numRows())
    }

    SolverResult(curState)
  }


}
