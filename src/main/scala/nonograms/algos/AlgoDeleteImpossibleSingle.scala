package nonograms.algos

import nonograms._

import scala.collection.mutable.ArrayBuffer

// "2" "--M---"   -> "D-M-DD"
case class AlgoDeleteImpossibleSingle() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (!line.isComplete()) {
        if (clues.clues.size == 1) {

          line.idxOfFirstMark() match {
            case Some(idxOfFirstMark) =>
              val from = Math.max(idxOfFirstMark - (clues.clues.head - 1), 0)
              val to = Math.min(idxOfFirstMark + (clues.clues.head - 1), lineLength - 1)

              for (idx <- Range(0, lineLength)) {
                if (idx < from || idx > to) {
                  if (row.isDefined) {
                    handle(Algorithm.delete(row.get, idx, s.board, curState))
                  }
                  else {
                    handle(Algorithm.delete(idx, col.get, s.board, curState))
                  }
                }
              }
            case _                    =>
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
