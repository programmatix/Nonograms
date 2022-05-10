package nonograms.algos

import nonograms.{BoardState, Deleted, Marked, SquareStateDeleted, SquareStateMarked, SquareStateUntouched, Untouched}

/** - M M M M - M   [5, ...] - the last obviously can't be part of the first group
 * Only looks at the first such match on the line.
 */
case class AlgoCannotReachOverSingleGap() extends AlgoMini {

  def run(p: AlgoMiniParams): BoardState = {
    val firstClue = p.b.clues.clues.head

    val certainLookingAtFirstClue = p.runs.head match {
      case Untouched(_, length) => length <= firstClue
      // Remember if the first run is Deleted then it will have been pruned
      case _: Deleted =>
        assert(false)
        false
      case _ => true
    }

    if (!certainLookingAtFirstClue) {
      p.startState
    }
    else {
      if (p.runs.length >= 4) {
        p.runs.head match {
          case Untouched(_, initialGapLength) =>
            p.runs(1) match {
              case Marked(_, run1Length) if (run1Length < firstClue) =>
                p.runs(2) match {
                  // Could support more than single gaps
                  case Untouched(gapIndex, gapLength) if (gapLength == 1) =>
                    p.runs(3) match {
                      case Marked(_, _) =>
                        if (run1Length + gapLength == firstClue) {
                          // Run1 cannot be connected to run2.  Delete the gap
                          AlgoHelpers.delete(p.b, p.startState, adjust(gapIndex, p))
                        }
                        else if (initialGapLength + run1Length < firstClue) {
                          // There's not enough space for initialGap+run1 to contain firstClue, so run1 and run2 must be connected.
                          AlgoHelpers.mark(p.b, p.startState, adjust(gapIndex, p))
                        }
                        else {
                          // Run1 could be connected to run2.  Unclear.
                          p.startState
                        }
                      case _ => p.startState
                    }
                  case _ => p.startState
                }
              case _ => p.startState
            }
          case _ => p.startState
        }
      }
      else p.startState
    }
  }
}
