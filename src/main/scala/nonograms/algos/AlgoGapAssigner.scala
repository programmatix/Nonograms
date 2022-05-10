package nonograms.algos

import nonograms.BoardState

/** If it can unambiguously assign a gap to a particular clue it does, and then fills in accordingly
 * X X X M M M D X X - X D - - D M X X D M   -> clearly the initial 6 can be filled
 * It's pretty limited - it only looks at the first gap
 */
case class AlgoGapAssigner() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    if (p.ranges.nonEmpty
      && p.ranges.head.marked > 0
      && p.ranges.head.len() == p.b.clues.clues.head
      && !p.ranges.head.isComplete) {
      AlgoHelpers.rangeHasExactlyOneClue(p.b, p.startState, adjust(p.ranges.head, p), p.b.clues.clues.head)
    }
    else p.startState
  }
}
