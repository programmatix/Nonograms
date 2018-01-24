package nonograms.algos

import nonograms._

import scala.collection.mutable.ArrayBuffer

// See what spaces a single clue can't fit into
// "3" "--D---"   -> "DDD---"
case class AlgoDeleteNoSpaceForSingle() extends Algorithm {

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

          val clueLen = clues.clues.head

          // idx is non-inclusive
          def checkRange(idx: Int, startOfDeletableRange: Int): Unit = {
            val rangeLen = (idx - startOfDeletableRange)
            if (rangeLen > 0 && rangeLen < clueLen) {
              // Found a range we can't fit in, delete
              for (idx <- Range(startOfDeletableRange, idx)) {
                if (row.isDefined) {
                  handle(Algorithm.delete(row.get, idx, s.board, curState))
                }
                else {
                  handle(Algorithm.delete(idx, col.get, s.board, curState))
                }
              }
            }
          }


          var startOfDeletableRange = 0
          var lookingForPossiblyDeletableRange = false
          var insideRangeContainingMark = false

          // Need to look for a range with no marks that can't fit our clue
          for (idx <- Range(0, lineLength)) {
            val square = line.squares(idx)
            if (lookingForPossiblyDeletableRange) {
              if (!square.isUntouched()) {
                // Range is finished
                lookingForPossiblyDeletableRange = false
                if (!square.isMarked()) {
                  checkRange(idx, startOfDeletableRange)
                }
                else {
                  insideRangeContainingMark = true
                }
              }
            }
            else {
              if (square.isMarked()) {
                insideRangeContainingMark = true
              }
              if (square.isUntouched() && !insideRangeContainingMark) {
                lookingForPossiblyDeletableRange = true
                startOfDeletableRange = idx
              }
              if (insideRangeContainingMark && square.isDeleted()) {
                insideRangeContainingMark = false
              }
            }
          }

          if (lookingForPossiblyDeletableRange) {
            if (lineLength > 1) {
              checkRange(lineLength, startOfDeletableRange)
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
