package nonograms.algos

import nonograms.{BoardState, LineClues, LineState}

// If it's trivially obvious what needs to be marked:
// a) The number of ranges left == the number of blocks
// b) Each range left is the same length as its corresponding block
// "1,2" "D-DXX" -> "DXDXX"
// "1,2" "D-D--D" -> "DXDXXD"
case class AlgoMarkObviousRemaining() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    var curState = s.state

    def handle(input: BoardState) = {
      curState = input
    }

    // Abstract over rows and cols
    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int) {
      if (!line.isComplete()) {
        val ranges = line.getRanges()
        if (ranges == clues.clues) {


          for (idx <- Range(0, lineLength)) {
            if (line.squares(idx).isUntouched()) {
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
