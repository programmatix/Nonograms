package nonograms.algos

import nonograms._

import scala.collection.mutable.ArrayBuffer

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
                          // May better be called gaps
                          ranges: Vector[MarkRange],
                          runs: Seq[Run],
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
    AlgoDeleteEdgesIfNoSpaceInThereForFirstClue(),
    AlgoDeleteNoSpaceForSingle(),
    AlgoNearStartOfClueAlreadyMarked(),
    AlgoCannotReachOverSingleGap(),
    AlgoDeleteNextToCompletedLargestClues())
}

// A new setup for applying mini algos to a line.  Faster performance than multiple Algorithms:
// a) A lot of useful line results are calculated once and reused
// b) Rows & Cols are only iterated once

// Plus:
// a) The board is flipped so each algorithm only has to be written once

case class AlgoMinis(algosToUse: Seq[AlgoMini]) extends Algorithm {
  def analyseLine(b: LineBasic): Seq[Run] = {
    var index = 0
    var state: Class[_] = b.line.squares(0).getClass
    var runLength = 0
    var runIndex = 0
    val ret = ArrayBuffer.empty[Run]

    def add() {
      val toAdd: Run = if (state == classOf[SquareStateUntouched]) Untouched(runIndex, runLength)
      else if (state == classOf[SquareStateMarked]) Marked(runIndex, runLength)
      else Deleted(runIndex, runLength)
      ret.append(toAdd)
    }

    while (index < b.lineLength) {
      val square = b.line.squares(index)
      if (square.getClass == state) {
        runLength += 1
      }
      else {
        add()
        state = square.getClass
        runIndex = index
        runLength = 1
      }
      index += 1
    }

    add()

    ret.toSeq
  }

  // Abstract over rows and cols
  def doLine(b: LineBasic, startState: BoardState, isFlipped: Boolean): BoardState = {
    var out = startState

    // No need to do anything with complete lines, and ignore 1-length cols for testing
    if (!b.line.isComplete() && b.line.squares.length > 1) {
      val ranges = b.line.getMarkRanges()
      val rangesWithMarks = ranges.count(v => v.marked > 0)
      val stretches = b.line.getMarkedStretches()
      val p = AlgoMiniParams(b, out, stretches, ranges, analyseLine(b), rangesWithMarks, isFlipped)

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
    prune(s.params.allowPruning, rowData, lineState)
  }

  def pruneCol(s: ForSolver, col: Int) = {
    val lineData = s.board.getColData(col)
    val lineState = s.state.getCol(col)
    prune(s.params.allowPruning, lineData, lineState)
  }



  // Remove any completed clues and deleted squares from the edges (effectively shrinking the board).  This allows
  // a lot of algos to just work at the edges, removing a lot of their complexity.
  def prune(allowPruning: Boolean, lineData: Vector[Boolean], lineState: LineState) = {
    if (allowPruning) {
      val firstUntouchedIdx = AlgoMinis.findSquaresToPrune(lineData, lineState)
      val lastUntouchedCount = AlgoMinis.findSquaresToPrune(lineData.reverse, LineState(lineState.squares.reverse))

      val lineDataPruned = lineData.dropRight(lastUntouchedCount).drop(firstUntouchedIdx)
      val lineStatePruned = LineState(lineState.squares.dropRight(lastUntouchedCount).drop(firstUntouchedIdx))

      val clues = Clues.generateClues(lineDataPruned)

      Pruned(lineData, lineDataPruned, lineState, lineStatePruned, clues, firstUntouchedIdx, lastUntouchedCount)
    }
    else {
      val clues = Clues.generateClues(lineData)
      Pruned(lineData, lineData, lineState, lineState, clues, 0, 0)
    }
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
              else if (ms == clues.head) {
                // Have a situation like M M D D M M - [2,2,...]
                // Can fill in that last "-" to D - but, not if we prune.  So skip pruning here.
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