package nonograms.algos

import nonograms._

case class AlgoMiniParams(b: LineBasic,
                          startState: BoardState,
                          stretches: Vector[MarkRange],
                          ranges: Vector[MarkRange],
                          // Number of ranges that contain one or more marks
                          rangesWithMarks: Int,
                          isReversed: Boolean)

trait AlgoMini {
  def run(p: AlgoMiniParams): BoardState
}

object AlgoMini {
  val all = Vector(AlgoCheckUnambiguousClueNearStart(),
    AlgoEachRangeContainsZeroOrOneClues())
}

// Looks for clues near the edges
// "1" "DM----" -> "DMDDDD"
// "1,2" "DM----" -> "DMD---"
case class AlgoCheckUnambiguousClueNearStart() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    if (p.stretches.nonEmpty) {
      val stretch = p.stretches.head
      val idxFirstMark = stretch.fromIdx

      val isSpaceForFirstClueBeforeFirstMark = (idxFirstMark - p.b.clues.clues.head) >= 1

      if (isSpaceForFirstClueBeforeFirstMark) {
        p.startState
      }
      else {
        // First mark has to be the first clue
        AlgoHelpers.firstStretchIsFirstClue(p.b, p.startState, stretch)
      }
    }
    else {
      p.startState
    }
  }
}

case class AlgoEachRangeContainsZeroOrOneClues() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    if (p.rangesWithMarks == p.b.clues.clues.length) {
      AlgoHelpers.eachRangeContainsZeroOrOneClues(p.b, p.startState, p.ranges)
    }
    else {
      p.startState
    }
  }
}

// A new setup for applying mini algos to a line.  Faster performance than multiple Algorithms:
// a) A lot of useful line results are calculated once and reused
// b) Rows & Cols are only iterated once

// Plus:
// a) The board is flipped so each algorithm only has to be written once
case class AlgoMinis(algosToUse: Seq[AlgoMini]) extends Algorithm {


  // Abstract over rows and cols
  def doLine(b: LineBasic, startState: BoardState, isFlipped: Boolean): BoardState = {
    var out = startState

    // No need to do anything with complete lines, and ignore 1-length cols for testing
    if (!b.line.isComplete() && b.line.squares.length > 1) {
      val ranges = b.line.getMarkRanges()
      val rangesWithMarks = ranges.count(v => v.marked > 0)
      val stretches = b.line.getMarkedStretches()
      val p = AlgoMiniParams(b, out, stretches, ranges, rangesWithMarks, isFlipped)

      algosToUse.foreach(algo => {
        val newP = p.copy(startState = out)
        out = algo.run(newP)
      })
    }

    out
  }

  def solve(s: ForSolver): SolverResult = {
    var out = s.state

    for (row <- Range(0, s.board.numRows())) {
      val clues = s.clues.getRow(row)
      val solved = s.state.getRow(row)
      val lb = LineBasic(s.board, Some(row), None, solved, clues, s.board.numCols())
      out = doLine(lb, out, false)
    }

    for (col <- Range(0, s.board.numCols())) {
      val clues = s.clues.getCol(col)
      val solved = s.state.getCol(col)
      val lb = LineBasic(s.board, None, Some(col), solved, clues, s.board.numRows())
      out = doLine(lb, out, false)
    }

    val flippedHorizontalled: ForSolver = s.copy(state = out).flipHorizontally()

    for (row <- Range(0, flippedHorizontalled.board.numRows())) {
      val clues = flippedHorizontalled.clues.getRow(row)
      val solved = flippedHorizontalled.state.getRow(row)
      val lb = LineBasic(flippedHorizontalled.board, Some(row), None, solved, clues, flippedHorizontalled.board.numCols())
      out = doLine(lb, flippedHorizontalled.state, true)
    }

    // Flip it back again
    out = out.flipHorizontally()

    val flippedVertically: ForSolver = s.copy(state = out).flipVertically()

    for (col <- Range(0, flippedVertically.board.numCols())) {
      val clues = flippedVertically.clues.getCol(col)
      val solved = flippedVertically.state.getCol(col)
      val lb = LineBasic(flippedVertically.board, None, Some(col), solved, clues, flippedVertically.board.numRows())
      out = doLine(lb, flippedVertically.state, false)
    }

    // Flip it back again
    out = out.flipVertically()

    SolverResult(out)
  }


}
