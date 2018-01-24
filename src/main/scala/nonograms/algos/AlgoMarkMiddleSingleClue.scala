package nonograms.algos

import nonograms._

import scala.collection.mutable.ArrayBuffer

// Handle "3" in a 5row -> "??X??"
// Handle "4" in a 5row -> "?XXX?"
case class AlgoMarkMiddleSingleClue() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (clues.clues.length == 1) {
        val min = clues.minimum()
        val diff = lineLength - min

        if (row.isDefined) {
          handle(Algorithm.markRowRange(row.get, diff, lineLength - diff, s.board, curState))
        }
        else {
          handle(Algorithm.markColRange(col.get, diff, lineLength - diff, s.board, curState))
        }
      }
    }

    for (row <- Range(0, s.board.numRows())) {
      val clues = s.clues.getRow(row)
      doLine(clues, Some(row), None, s.board.numCols())
    }

    for (col <- Range(0, s.board.numCols())) {
      val clues = s.clues.getCol(col)
      doLine(clues, None, Some(col), s.board.numRows())
    }

    SolverResult(curState)
  }


}
