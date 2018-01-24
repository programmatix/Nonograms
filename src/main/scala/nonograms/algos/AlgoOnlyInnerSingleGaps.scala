package nonograms.algos

import nonograms.BoardState

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// Handle "X0X" or "XXOX" or "XXOXXXOXXX"
// When there's only one possible fit
case class AlgoOnlyInnerSingleGaps() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    for (row <- Range(0, s.board.numRows())) {
      val clue = s.clues.getRow(row)
      val total = clue.totalFilled()
      val gaps = clue.clues.length - 1
      val fills = total + gaps == s.board.numCols()
      if (fills) {
        var idx = 0
        for(clueIt <- clue.clues.indices) {
          val clueLength = clue.clues(clueIt)
          handle(Algorithm.markRowRange(row, idx, idx + clueLength, s.board, curState))
          if (clueIt != clue.clues.length - 1) {
            handle(Algorithm.delete(row, idx + clueLength, s.board, curState))
          }
          idx += clueLength + 1
        }
      }
    }

    for (col <- Range(0, s.board.numCols())) {
      val clue = s.clues.getCol(col)
      val total = clue.totalFilled()
      val gaps = clue.clues.length - 1
      val fills = total + gaps == s.board.numRows()
      if (fills) {
        var idx = 0
        for(clueIt <- clue.clues.indices) {
          val clueLength = clue.clues(clueIt)
          handle(Algorithm.markColRange(col, idx, idx + clueLength, s.board, curState))
          if (clueIt != clue.clues.length - 1) {
            handle(Algorithm.delete(idx + clueLength, col, s.board, curState))
          }
          idx += clueLength + 1
        }
      }
    }


    SolverResult(curState)
  }
}
