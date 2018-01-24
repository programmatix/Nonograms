package nonograms.algos

import nonograms.{BoardState, LineClues, LineState}

import scala.collection.mutable.ArrayBuffer

// Surround definitely completed ranges with blanks.  Currently only supports:
// a) the biggest range only
// b) if all ranges are same size
// "1" "-M--" -> "DMD-"
case class AlgoDeleteNextToCompletedRanges() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    val out = ArrayBuffer.empty[BoardState]
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (!line.isComplete()) {
        val ranges = line.getMarkedStretches()
        val cluesAllSame = clues.clues.length > 1 && clues.areAllSame()

        ranges.foreach(range => {


          if (cluesAllSame && range.len() == clues.clues.head || range.len() == clues.biggest()) {

            val exactClues = clues.exactClues(range.len())
            val possibleClues = clues.possibleClues(range.len())

            if (cluesAllSame || (exactClues == 1 && possibleClues == 1)) {
              // This range could only match one clue, we can fill it in
              if (range.fromIdx - 1 >= 0) {
                if (row.isDefined) {
                  handle(Algorithm.delete(row.get, range.fromIdx - 1, s.board, curState))
                }
                else {
                  handle(Algorithm.delete(range.fromIdx - 1, col.get, s.board, curState))
                }
              }

              if (range.toIdx + 1 < lineLength) {
                if (row.isDefined) {
                  handle(Algorithm.delete(row.get, range.toIdx + 1, s.board, curState))
                }
                else {
                  handle(Algorithm.delete(range.toIdx + 1, col.get, s.board, curState))
                }
              }
            }
          }
        })
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
