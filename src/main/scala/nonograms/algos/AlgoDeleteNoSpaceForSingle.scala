package nonograms.algos

import nonograms._

// See what spaces a single clue can't fit into
// "3" "--D---"   -> "DDD---"
case class AlgoDeleteNoSpaceForSingle() extends AlgoMini {

  override def run(p: AlgoMiniParams): BoardState = {
    val line = p.b.line
    val clues = p.b.clues
    var curState = p.startState
    val lineLength = p.b.lineLength

    // Is there space at the edge that cannot fit the first clue?
    p.runs.head match {
      case Untouched(_, _) =>
        val firstRangeLen = p.ranges.head.len()
        if (firstRangeLen < clues.clues.head) {
          curState = AlgoHelpers.deleteRange(p.b, curState, adjust(0, p), adjust(firstRangeLen, p))
        }
      case _ =>
    }

    // Original logic
    if (clues.clues.size == 1) {

      val clueLen = clues.clues.head

      // idx is non-inclusive
      def checkRange(idx: Int, startOfDeletableRange: Int): Unit = {
        val rangeLen = (idx - startOfDeletableRange)
        if (rangeLen > 0 && rangeLen < clueLen) {
          // Found a range we can't fit in, delete
          for (idx <- Range(startOfDeletableRange, idx)) {
            curState = AlgoHelpers.delete(p.b, curState, adjust(idx, p))
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

    curState
  }
}
