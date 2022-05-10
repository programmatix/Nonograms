package nonograms.algos

import nonograms.{BoardState, LineClues, LineState}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

// "1,2" "D----" -> "DXDXX"
case class AlgoOnlyInnerSingleGapsAfterDeletesAtEdges() extends AlgoMini {

  override def run(p: AlgoMiniParams): BoardState = {
    val line = p.b.line
    val clues = p.b.clues
    var curState = p.startState

    if (line.areAllDeletedAtEdges() && line.squares.length > 1) {
      val min = clues.minimum()
      val fills = min == line.remainingAfterDeleted()
      if (fills) {
        val startOffset = line.idxOfFirstNotDeleted()
        if (startOffset.isDefined) {
          var idx = startOffset.get
          for (clueIt <- clues.clues.indices) {
            val clueLength = clues.clues(clueIt)
            curState = AlgoHelpers.markRange(p.b, curState, adjust(idx, p), adjust(idx + clueLength, p))
            if (clueIt != clues.clues.length - 1) {
              curState = AlgoHelpers.delete(p.b, curState, adjust(idx + clueLength, p))
            }
            idx += clueLength + 1
          }
        }
      }
    }

    curState
  }
}
