package nonograms.algos

import nonograms.{BoardState, LineClues, LineState}

// Surround definitely completed clues with blanks.  Currently only supports:
// a) the biggest range only
// b) if all ranges are same size
// "1" "-M--" -> "DMD-"
case class AlgoDeleteNextToCompletedRanges() extends AlgoMini {

  override def run(p: AlgoMiniParams): BoardState = {
    val line = p.b.line
    var curState = p.startState
    val clues = p.b.clues
    val ranges = line.getMarkedStretches()
    val cluesAllSame = clues.clues.length > 1 && clues.areAllSame()

    ranges.foreach(range => {
      if (cluesAllSame && range.len() == clues.clues.head || range.len() == clues.biggest()) {

        val exactClues = clues.exactClues(range.len())
        val possibleClues = clues.possibleClues(range.len())

        if (cluesAllSame || (exactClues == 1 && possibleClues == 1)) {
          // This range could only match one clue, we can fill it in
          if (range.fromIdx - 1 >= 0) {
            curState = AlgoHelpers.delete(p.b, curState, adjust(range.fromIdx - 1, p))
          }

          if (range.toIdx + 1 < p.b.lineLength) {
            curState = AlgoHelpers.delete(p.b, curState, adjust(range.toIdx + 1, p))
          }
        }
      }
    })

    curState
  }
}
