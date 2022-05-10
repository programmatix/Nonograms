package nonograms.algos

import nonograms._

// If there's a 6 marked and the largest clue(s) is 6, can delete either side
case class AlgoDeleteNextToCompletedLargestClues() extends AlgoMini {

  override def run(p: AlgoMiniParams): BoardState = {
    var curState = p.startState
    val biggestClue = p.b.clues.biggest()

    p.runs.foreach {
      case Marked(index, length) if (length == biggestClue) =>
        if (index > 0) curState = AlgoHelpers.delete(p.b, curState, adjust(index - 1, p))
        if (index + length + 1 < p.b.lineLength) curState = AlgoHelpers.delete(p.b, curState, adjust(index + length, p))
      case _ =>
    }

    curState
  }
}
