package nonograms.algos

import nonograms.{BoardState, Deleted, Marked, Untouched}

/** If it can unambiguously assign a gap to a particular clue it does, and then fills in accordingly
 * X X X M M M D X X - X D - - D M X X D M   -> clearly the initial 6 can be filled
 * It's pretty limited - it only looks at the first gap
 * M M D D M M D D M M - D - D M M M M M M  2  2  2  1  6
 * -> the 3 2s can all be assigned into gaps and so a D could be placed next to the 3rd 2
 * -> there is logic inside the pruner that makes sure not to prune in this situation
 */
case class AlgoGapAssigner() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    // First attempt at logic to handle just 1st gap
    if (p.ranges.nonEmpty
      && p.ranges.head.marked > 0
      && p.ranges.head.len() == p.b.clues.clues.head
      && !p.ranges.head.isComplete) {
      AlgoHelpers.rangeHasExactlyOneClue(p.b, p.startState, adjust(p.ranges.head, p), p.b.clues.clues.head)
    }
    else {
      // More sophisticated logic to handle "MMDDMMDMM-" - e.g. packed sets of marked and deleted
      // Stop at the first Untouched
      var clues = p.b.clues.clues
      var done = false
      var index = 0
      var curState = p.startState

      while (!done && index < p.runs.length) {
        p.runs(index) match {
          case Deleted(index, length) =>
          case Marked(index, length) =>
            if (length == clues.head) {
              curState = AlgoHelpers.delete(p.b, curState, adjust(length + index, p))
              clues = clues.tail
            }
            else {
              // Looking for packed clues
              done = true
            }
          case Untouched(index, length) =>
            done = true
        }
        index += 1
      }

      curState
    }
  }
}
