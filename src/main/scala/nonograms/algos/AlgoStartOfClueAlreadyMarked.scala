package nonograms.algos

import nonograms._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// "3" "M----"   -> "MMM--"
// "3" "MM---"   -> "MMM--"
// "2,1" "M---"  -> "MM---"
// And inverse:
// "3" "----M"   -> "--MMM"
case class AlgoStartOfClueAlreadyMarked() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, solved: Vector[Boolean], row: Option[Int], col: Option[Int], lineLength: Int) {
      if (solved.nonEmpty && !clues.isEmpty()) {

        // Start of row/col
        if (solved.head) {
          val clueLen = clues.clues.head

          for (idx <- Range(1, clueLen)) {
            if (row.isDefined) {
              handle(Algorithm.mark(row.get, idx, s.board, curState))
            }
            else {
              handle(Algorithm.mark(idx, col.get, s.board, curState))
            }
          }
        }

        // End of row/col
        if (solved.last) {
          val clueLen = clues.clues.last

          for (idx <- Range(lineLength - 1, lineLength - clueLen - 1, -1)) {
            if (row.isDefined) {
              handle(Algorithm.mark(row.get, idx, s.board, curState))
            }
            else {
              handle(Algorithm.mark(idx, col.get, s.board, curState))
            }
          }
        }
      }
    }

    for (row <- Range(0, s.board.numRows())) {
      val clues = s.clues.getRow(row)
      val solved = s.state.getRowMarks(row)
      doLine(clues, solved, Some(row), None, s.board.numCols())
    }

    for (col <- Range(0, s.board.numCols())) {
      val clues = s.clues.getCol(col)
      val solved = s.state.getColMarks(col)
      doLine(clues, solved, None, Some(col), s.board.numRows())
    }

    SolverResult(curState)
  }


}
