package nonograms.algos

import nonograms.BoardState

case class AlgoEachRangeContainsZeroOrOneClues() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    // TODO I'm not convinced about this logic..
//    if (p.rangesWithMarks == p.b.clues.clues.length) {
//      AlgoHelpers.eachRangeContainsZeroOrOneClues(p.b, p.startState, adjust(p.ranges, p))
//    }
//    else {
      p.startState
//    }
  }
}
