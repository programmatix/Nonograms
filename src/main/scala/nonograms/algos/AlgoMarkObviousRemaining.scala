package nonograms.algos

import nonograms.{BoardState, LineClues, LineState}

// If it's trivially obvious what needs to be marked:
// a) The number of ranges left == the number of blocks
// b) Each range left is the same length as its corresponding block
// "1,2" "D-DXX" -> "DXDXX"
// "1,2" "D-D--D" -> "DXDXXD"
case class AlgoMarkObviousRemaining() extends AlgoMini {
  override def run(p: AlgoMiniParams): BoardState = {
    val line = p.b.line
    val clues = p.b.clues
    var curState = p.startState

    val ranges = line.getRanges()
    if (ranges == clues.clues) {
      for (idx <- Range(0, p.b.lineLength)) {
        if (line.squares(idx).isUntouched()) {
          curState = AlgoHelpers.mark(p.b, curState, adjust(idx, p))
        }
      }
    }

    curState
  }
}
