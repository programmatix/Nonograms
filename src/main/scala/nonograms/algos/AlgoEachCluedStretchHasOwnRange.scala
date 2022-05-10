package nonograms.algos

import nonograms._

case class LineBasic(board: Board,         // Always unpruned
                     row: Option[Int],
                     col: Option[Int],
                     line: LineState,      // May be pruned
                     clues: LineClues,     // May be pruned
                     pruned: Option[Pruned] = None) {
  def lineLength: Int = line.squares.length  // May be pruned
}



// When there's a separate stretch for each clue, and each stretch (group of marks) is in its own range
@Deprecated // AlgoMinis
case class AlgoEachCluedStretchHasOwnRange() extends Algorithm {

  // Abstract over rows and cols
  def doLine(b: LineBasic, startState: BoardState): BoardState = {
    var out = startState

    if (!b.line.isComplete() && b.line.squares.length > 1) {
      val ranges = b.line.getMarkRanges()
      val rangesWithMarks = ranges.count(v => v.marked > 0)

      if (rangesWithMarks == b.clues.clues.length) {
        out = AlgoHelpers.eachRangeContainsZeroOrOneClues(b, out, ranges)
      }
    }

    out
  }

  def solve(s: ForSolver): SolverResult = {
    var out = s.state

    for (row <- Range(0, s.board.numRows())) {
      val clues = s.clues.getRow(row)
      val solved = s.state.getRow(row)
      val lb = LineBasic(s.board, Some(row), None, solved, clues)
      out = doLine(lb, out)
    }

    for (col <- Range(0, s.board.numCols())) {
      val clues = s.clues.getCol(col)
      val solved = s.state.getCol(col)
      val lb = LineBasic(s.board, None, Some(col), solved, clues)
      out = doLine(lb, out)
    }

    SolverResult(out)
  }


}
