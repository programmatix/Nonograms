package nonograms.algos

import nonograms._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// "3" "-M----"   -> "-MM--"
// "4" "-MM---"   -> "-MMM--"
// "4,1" "--M---"  -> "--MM---"
case class AlgoNearStartOfClueAlreadyMarked() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (!line.isComplete()) {
        if (!clues.isEmpty()) {

          // Start of row/col
          line.idxOfFirstMark() match {
            case Some(idxFirstMark) =>
              val clueLen = clues.clues.head
              val distToIdx = idxFirstMark + 1
              if (clueLen > distToIdx) {
                val until = idxFirstMark + (clueLen - idxFirstMark)

                for (idx <- Range(idxFirstMark, until)) {
                  if (row.isDefined) {
                    handle(Algorithm.mark(row.get, idx, s.board, curState))
                  }
                  else {
                    handle(Algorithm.mark(idx, col.get, s.board, curState))
                  }
                }
              }
            case _                  =>
          }

          // End of row/col
          line.idxOfLastMark() match {
            case Some(idxLastMark) =>
              val clueLen = clues.clues.last
              val distToIdx = lineLength - idxLastMark
              if (clueLen > distToIdx) {
                val until = idxLastMark - (clueLen - distToIdx)

                for (idx <- Range(idxLastMark - 1, until - 1, -1)) {
                  if (row.isDefined) {
                    handle(Algorithm.mark(row.get, idx, s.board, curState))
                  }
                  else {
                    handle(Algorithm.mark(idx, col.get, s.board, curState))
                  }
                }
              }
            case _                 =>
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
