package nonograms.algos

import nonograms.BoardState

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
