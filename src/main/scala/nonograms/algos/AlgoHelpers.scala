package nonograms.algos

import nonograms.{BoardState, MarkRange}

object AlgoHelpers {


  private def handle(input: BoardState): BoardState = {
    input
    //    curState = inputt._1
    //    out ++= input._2
  }


  // toIdx is not-inclusive
  def deleteRange(b: LineBasic, start: BoardState, fromIdx: Int, toIdx: Int): BoardState = {
    var curState = start

    for (idx <- Range(fromIdx, toIdx)) {
      if (b.row.isDefined) {
        curState = handle(Algorithm.delete(b.row.get, idx, b.board, curState))
      }
      else {
        curState = handle(Algorithm.delete(idx, b.col.get, b.board, curState))
      }
    }

    curState
  }

  def delete(b: LineBasic, start: BoardState, idx: Int): BoardState = {
    if (b.row.isDefined) {
      handle(Algorithm.delete(b.row.get, idx, b.board, start))
    }
    else {
      handle(Algorithm.delete(idx, b.col.get, b.board, start))
    }
  }

  def mark(b: LineBasic, start: BoardState, idx: Int): BoardState = {
    if (b.row.isDefined) {
      handle(Algorithm.mark(b.row.get, idx, b.board, start))
    }
    else {
      handle(Algorithm.mark(idx, b.col.get, b.board, start))
    }
  }

  // toIdx is not-inclusive
  def markRange(b: LineBasic, start: BoardState, fromIdx: Int, toIdx: Int): BoardState = {
    var curState = start

    for (idx <- Range(fromIdx, toIdx)) {
      if (b.row.isDefined) {
        curState = handle(Algorithm.mark(b.row.get, idx, b.board, curState))
      }
      else {
        curState = handle(Algorithm.mark(idx, b.col.get, b.board, curState))
      }
    }

    curState
  }

  /** Can delete everything else in the range
   * @param b may be pruned
   * @param range has been mapped/depruned
   */
  def rangeContainsOneClueWhichIsFullyMarked(b: LineBasic, start: BoardState, range: MarkRange): BoardState = {
    var out = start

    for (idx <- Range(range.fromIdx, range.toIdx + 1)) {
      if (start.getLine(b).squares(idx).isUntouched()) {
        if (b.row.isDefined) {
          out = handle(Algorithm.delete(b.row.get, idx, b.board, out))
        }
        else {
          out = handle(Algorithm.delete(idx, b.col.get, b.board, out))
        }
      }
    }

    out
  }

  // Know that the first stretch has to match the first clue
  def firstStretchIsFirstClue(b: LineBasic, start: BoardState, stretch: MarkRange): BoardState = {
    if (stretch.len() == b.clues.clues.head) {
      AlgoHelpers.stretchCompletesClue(b, start, stretch)
    }
    else {
      val incomplete = b.clues.clues.head - stretch.len()
      // Stretch needs to be surrounded by 'incomplete' squares
      val toIdx = stretch.fromIdx - incomplete
      if (toIdx > 0) {
        deleteRange(b, start, 0, toIdx)
      }
      else {
        start
      }
    }
  }

  // Know that the last stretch has to match the last clue
  def lastStretchIsLastClue(b: LineBasic, start: BoardState, stretch: MarkRange): BoardState = {
    if (stretch.len() == b.clues.clues.last) {
      AlgoHelpers.stretchCompletesClue(b, start, stretch)
    }
    else {
      val incomplete = b.clues.clues.last - stretch.len()

      // Stretch needs to be surrounded by 'incomplete' squares
      val toIdx = b.lineLength - incomplete
      if (toIdx > 0) {
        deleteRange(b, start, toIdx + 1, b.lineLength)
      }
      else {
        start
      }
    }
  }

  // Sure that this stretch is complete (fully marked)
  def stretchCompletesClue(b: LineBasic, start: BoardState, stretch: MarkRange): BoardState = {
    if (b.clues.clues.length == 1) {
      // The only clue is complete
      rangeContainsOneClueWhichIsFullyMarked(b, start, MarkRange(0, b.lineLength - 1, b.clues.clues.head))
    }
    else {
      // There are other clues do we can't do too much, just delete around it
      var out = start
      if (stretch.fromIdx - 1 >= 0) {
        out = delete(b, out, stretch.fromIdx - 1)
      }
      if (stretch.toIdx + 1 < b.lineLength) {
        out = delete(b, out, stretch.toIdx + 1)
      }
      out
    }
  }

  // We know each range either a) doesn't contain a mark and can be deleted or b) contains a mark AND exactly one clue
  def eachRangeContainsZeroOrOneClues(b: LineBasic, start: BoardState, ranges: Seq[MarkRange]): BoardState = {
    var clueIdx = 0
    var out = start

    for (idx <- ranges.indices) {
      val range = ranges(idx)

      if (range.marked == 0) {
        // This range has no marks in it so we can delete the whole thing
        out = AlgoHelpers.deleteRange(b, out, range.fromIdx, range.toIdx + 1)
      }
      else {
        val clue = b.clues.clues(clueIdx)
        clueIdx += 1
        out = AlgoHelpers.rangeHasExactlyOneClue(b, out, range, clue)
      }
    }

    out
  }

  // Now have a range containing at least one mark, plus corresponding clue
  def rangeHasExactlyOneClue(b: LineBasic, start: BoardState, range: MarkRange, clue: Int): BoardState = {

    if (range.len() == clue) {
      markRange(b, start, range.fromIdx, range.toIdx + 1)
    }
    else if (range.marked == clue) {
      rangeContainsOneClueWhichIsFullyMarked(b, start, range)
    }
    else {
      // Range is bigger than the clue

      if (start.getLine(b).squares(range.fromIdx).isMarked()) {
        // Start of range is filled so we can do the rest of the range
        val out = markRange(b, start, range.fromIdx, range.fromIdx + clue)
        deleteRange(b, out, range.fromIdx + clue, range.toIdx + 1)
      }
      else if (start.getLine(b).squares(range.toIdx).isMarked()) {
        // Similarly if end of range is filled
        val out = markRange(b, start, range.toIdx - clue + 1, range.toIdx + 1)
        deleteRange(b, out, range.fromIdx, range.toIdx - clue + 1)
      }
      else {
        // Clue is somewhere in the middle of the range
        var foundMarker = false
        var out = start

        {
          // Scan from left and see what can be filled in
          var idx = range.fromIdx
          while (idx <= range.toIdx) {
            val square = start.getLine(b).squares(idx)
            if (!square.isDeleted()) {
              assert(!square.isDeleted())
            }
            if (square.isMarked()) {
              foundMarker = true
              val canFill = clue - (idx - range.fromIdx)
              if (canFill > 1) {
                out = markRange(b, out, idx, idx + canFill)
                val safeZone = idx - range.fromIdx
                val deleteFrom = idx + safeZone + canFill
                val deleteTo = range.toIdx + 1
                out = deleteRange(b, out, deleteFrom, deleteTo)
              }
            }
            idx += 1
          }
        }

        {
          // Scan from right and see what can be filled in
          var idx = range.toIdx
          while (idx >= range.fromIdx) {
            val square = start.getLine(b).squares(idx)
            assert(!square.isDeleted())
            if (square.isMarked()) {
              foundMarker = true
              val canFill = clue - (range.toIdx - idx)
              if (canFill > 1) {
                out = markRange(b, out, idx - canFill + 1, idx)
                val safeZone = range.toIdx - idx
                val deleteFrom = range.fromIdx
                val deleteTo = idx - canFill - (safeZone) + 1
                out = deleteRange(b, out, deleteFrom, deleteTo)
              }
            }
            idx -= 1
          }
        }

        out
      }
    }
  }
}
