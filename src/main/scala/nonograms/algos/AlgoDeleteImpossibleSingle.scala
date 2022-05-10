package nonograms.algos

import nonograms._

import scala.collection.mutable.ArrayBuffer

// "2" "--M---"   -> "D-M-DD"
case class AlgoDeleteImpossibleSingle() extends AlgoMini {

  override def run(p: AlgoMiniParams): BoardState = {
    val clues = p.b.clues
    val line = p.b.line
    val lineLength = p.b.lineLength
    var curState = p.startState

    if (clues.clues.size == 1) {

      line.idxOfFirstMark() match {
        case Some(idxOfFirstMark) =>
          val from = Math.max(idxOfFirstMark - (clues.clues.head - 1), 0)
          val to = Math.min(idxOfFirstMark + (clues.clues.head - 1), lineLength - 1)

          for (idx <- Range(0, lineLength)) {
            if (idx < from || idx > to) {
              curState = AlgoHelpers.delete(p.b, curState, adjust(idx, p))
            }
          }
        case _                    =>
      }
    }

    curState
  }
}
