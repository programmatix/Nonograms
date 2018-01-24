package nonograms.algos

// Fill full lines quickly to reduce search space
case class AlgoFullLines() extends Algorithm {

  def solve(s: ForSolver): SolverResult = {
    var curState = s.state

    for (row <- Range(0, s.board.numRows())) {
      val clue = s.clues.getRow(row)
      val isFull = clue.clues.size == 1 && clue.clues.head == s.board.numCols()
      if (isFull) {
        val x = Algorithm.markRow(row, s.board, curState)
        curState = x
      }
    }

    for (col <- Range(0, s.board.numCols())) {
      val clue = s.clues.getCol(col)
      val isFull = clue.clues.size == 1 && clue.clues.head == s.board.numRows()
      if (isFull) {
        val x = Algorithm.markCol(col, s.board, curState)
        curState = x
      }
    }

    SolverResult(curState)
  }
}
