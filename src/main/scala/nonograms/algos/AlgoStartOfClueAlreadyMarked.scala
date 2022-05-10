package nonograms.algos

import nonograms._

// "3" "M----"   -> "MMM--"
// "3" "MM---"   -> "MMM--"
// "2,1" "M---"  -> "MM---"
// And inverse:
// "3" "----M"   -> "--MMM"
case class AlgoStartOfClueAlreadyMarked() extends AlgoMini {
  override def run(p: AlgoMiniParams): BoardState = {
    val solved = p.b.line.squares.map(v => v.isMarked())
    val clues = p.b.clues
    var curState = p.startState

    if (solved.head) {
      val clueLen = clues.clues.head

      for (idx <- Range(1, clueLen)) {
        curState = AlgoHelpers.mark(p.b, curState, adjust(idx, p))
      }
    }

    curState
  }
}
