package nonograms.algos

import nonograms._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// "2,1" "MM--M-"   -> "MMDDMD"
case class AlgoDeleteRemainingFromSolvedLines() extends AlgoMini {
  override def run(p: AlgoMiniParams): BoardState = {
    var curState = p.startState
    val clues = p.b.clues
    val line = p.b.line

    if (clues.totalFilled() == line.totalMarked()) {
      for (idx <- Range(0, p.b.lineLength)) {
        if (line.squares(idx).isUntouched()) {
          curState = AlgoHelpers.delete(p.b, curState, adjust(idx, p))
        }
      }
    }

    curState
  }
}
