package nonograms.algos

import nonograms._

// "3" "-M----"   -> "-MM--"
// "4" "-MM---"   -> "-MMM--"
// "4,1" "--M---"  -> "--MM---"
case class AlgoNearStartOfClueAlreadyMarked() extends AlgoMini {

  override def run(p: AlgoMiniParams): BoardState = {
    var curState = p.startState
    val line = p.b.line

    // Start of row/col
    line.idxOfFirstMark() match {
      case Some(idxFirstMark) =>
        val clueLen = p.b.clues.clues.head
        val distToIdx = idxFirstMark + 1
        if (clueLen > distToIdx) {
          val until = idxFirstMark + (clueLen - idxFirstMark)

          for (idx <- Range(idxFirstMark, until)) {
            curState = AlgoHelpers.mark(p.b, curState, adjust(idx, p))
          }
        }
      case _ =>
    }

    // End of row/col
    line.idxOfLastMark() match {
      case Some(idxLastMark) =>
        val clueLen = p.b.clues.clues.last
        val distToIdx = p.b.lineLength - idxLastMark
        if (clueLen > distToIdx) {
          val until = idxLastMark - (clueLen - distToIdx)

          for (idx <- Range(idxLastMark - 1, until - 1, -1)) {
            curState = AlgoHelpers.mark(p.b, curState, adjust(idx, p))
          }
        }
      case _ =>
    }

    curState
  }
}
