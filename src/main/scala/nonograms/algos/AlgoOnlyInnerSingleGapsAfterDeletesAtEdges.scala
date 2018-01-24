package nonograms.algos

import nonograms.{BoardState, LineClues, LineState}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// "1,2" "D----" -> "DXDXX"
case class AlgoOnlyInnerSingleGapsAfterDeletesAtEdges() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (!line.isComplete() && line.areAllDeletedAtEdges() && line.squares.length > 1) {
        val min = clues.minimum()
        val fills = min == line.remainingAfterDeleted()
        if (fills) {
          val startOffset = line.idxOfFirstNotDeleted()
          if (startOffset.isDefined) {
            var idx = startOffset.get
            for (clueIt <- clues.clues.indices) {
              val clueLength = clues.clues(clueIt)
              if (row.isDefined) {
                handle(Algorithm.markRowRange(row.get, idx, idx + clueLength, s.board, curState))
                if (clueIt != clues.clues.length - 1) {
                  handle(Algorithm.delete(row.get, idx + clueLength, s.board, curState))
                }
              }
              else {
                handle(Algorithm.markColRange(col.get, idx, idx + clueLength, s.board, curState))
                if (clueIt != clues.clues.length - 1) {
                  handle(Algorithm.delete(idx + clueLength, col.get, s.board, curState))
                }
              }
              idx += clueLength + 1
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
