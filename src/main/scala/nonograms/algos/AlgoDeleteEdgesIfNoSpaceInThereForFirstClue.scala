package nonograms.algos

import nonograms.BoardState

/**
 * Given --DMMMMMM- or --D-----  and clues of [6,...], it's clear the initial two squares can be deleted
 */
case class AlgoDeleteEdgesIfNoSpaceInThereForFirstClue() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    if (p.ranges.nonEmpty
      && p.b.clues.clues.nonEmpty
      && p.ranges.head.len < p.b.clues.clues.head) {
      AlgoHelpers.deleteRange(p.b, p.startState, adjust(p.ranges.head.fromIdx, p), adjust(p.ranges.head.toIdx + 1, p))
    }
    else p.startState
  }
}
