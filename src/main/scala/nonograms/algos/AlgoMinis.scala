package nonograms.algos

import nonograms._

case class Pruned(lineDataOrig: Vector[Boolean],
                  lineDataPruned: Vector[Boolean],
                  lineStateOrig: LineState,
                  lineStatePruned: LineState,
                  clues: LineClues,
                  droppedFront: Int,
                  droppedBack: Int)

case class AlgoMiniParams(b: LineBasic,
                          startState: BoardState,
                          // Marked stretches
                          stretches: Vector[MarkRange],
                          ranges: Vector[MarkRange],
                          // Number of ranges that contain one or more marks
                          rangesWithMarks: Int,
                          isReversed: Boolean)

trait AlgoMini {
  def run(p: AlgoMiniParams): BoardState

  def adjust(range: MarkRange, p: AlgoMiniParams): MarkRange = MarkRange(adjust(range.fromIdx, p), adjust(range.toIdx, p), range.marked)

  def adjust(ranges: Seq[MarkRange], p: AlgoMiniParams): Seq[MarkRange] = ranges.map(range => adjust(range, p))

  def adjust(idx: Int, p: AlgoMiniParams): Int = adjust(idx, p.b)

  def adjust(idx: Int, ls: LineBasic): Int = {
    ls.pruned match {
      case Some(pruned) => idx + pruned.droppedFront
      case _ => idx
    }
  }
}

object AlgoMini {
  // AlgoMarkObvious not included as it doesn't look at board state
  val all = Vector(AlgoCheckUnambiguousClueNearStart(),
    AlgoEachRangeContainsZeroOrOneClues(),
    AlgoGapAssigner(),
    AlgoDeleteEdgesIfNoSpaceInThereForFirstClue())
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

  def pruneRow(s: ForSolver, row: Int) = {
    val rowData = s.board.getRowData(row)
    val lineState = s.state.getRow(row)
    prune(rowData, lineState)
  }

  def pruneCol(s: ForSolver, col: Int) = {
    val lineData = s.board.getColData(col)
    val lineState = s.state.getCol(col)
    prune(lineData, lineState)
  }



  // This was an exploratory attempt at this:
  // Remove any completed clues and deleted squares from the edges (effectively shrinking the board), and retry the algo.
  // However it leads to the algos attempting to manipulate the board state using the wrong indices.  Would have to map
  // those which looks like a lot of work..
  def prune(lineData: Vector[Boolean], lineState: LineState) = {
     val firstUntouchedIdx = AlgoMinis.findSquaresToPrune(lineData, lineState)
     val lastUntouchedCount = AlgoMinis.findSquaresToPrune(lineData.reverse, LineState(lineState.squares.reverse))

    val lineDataPruned = lineData.dropRight(lastUntouchedCount).drop(firstUntouchedIdx)
    val lineStatePruned = LineState(lineState.squares.dropRight(lastUntouchedCount).drop(firstUntouchedIdx))

//    val lineDataPruned = lineData
//    val lineStatePruned = lineState

    val clues = Clues.generateClues(lineDataPruned)

     Pruned(lineData, lineDataPruned, lineState, lineStatePruned, clues, firstUntouchedIdx, lastUntouchedCount)
//    Pruned(lineStatePruned, clues, 0, 0)
  }

  def solve(s: ForSolver): SolverResult = {
    var out = s.state

    for (row <- Range(0, s.board.numRows())) {
      val pruned = pruneRow(s, row)
      val lb = LineBasic(s.board, Some(row), None, pruned.lineStatePruned, pruned.clues, Some(pruned))
      out = doLine(lb, out, false)
    }

    for (col <- Range(0, s.board.numCols())) {
      val pruned = pruneCol(s, col)
      val lb = LineBasic(s.board, None, Some(col), pruned.lineStatePruned, pruned.clues, Some(pruned))
      out = doLine(lb, out, false)
    }

    val flippedHorizontally: ForSolver = s.copy(state = out).flipHorizontally()

    for (row <- Range(0, flippedHorizontally.board.numRows())) {
      val pruned = pruneRow(flippedHorizontally, row)
      val lb = LineBasic(flippedHorizontally.board, Some(row), None, pruned.lineStatePruned, pruned.clues, Some(pruned))
      out = doLine(lb, flippedHorizontally.state, true)
    }

    // Flip it back again
    out = out.flipHorizontally()

    val flippedVertically: ForSolver = s.copy(state = out).flipVertically()

    for (col <- Range(0, flippedVertically.board.numCols())) {
      val pruned = pruneCol(flippedVertically, col)
      val lb = LineBasic(flippedVertically.board, None, Some(col), pruned.lineStatePruned, pruned.clues, Some(pruned))
      out = doLine(lb, flippedVertically.state, true)
    }

    // Flip it back again
    out = out.flipVertically()

    SolverResult(out)
  }
}

object AlgoMinis {
  def findSquaresToPrune(lineData: Vector[Boolean], lineState: LineState): Int = {
    var clues = Clues.generateClues(lineData).clues
    var index = 0
    var done = false
    var markedSquares: Option[Int] = None
    while (!done && index < lineData.length) {
      lineState.squares(index) match {
        case _: SquareStateUntouched =>
          markedSquares match {
            case Some(ms) =>
              // Haven't completed a clue - go back to before it
              if (ms != clues.head) {
                index -= ms
              }
            case _ =>
          }
          done = true
        case _: SquareStateDeleted =>
          markedSquares match {
            case Some(ms) =>
              assert (ms == clues.head)
              clues = clues.drop(1)
              markedSquares = None
            case _ =>
          }
          index += 1
        case _: SquareStateMarked =>
          markedSquares match {
            case Some(ms) => markedSquares = Some(ms + 1)
            case _ => markedSquares = Some(1)
          }
          index += 1
      }
    }
    index
  }
}