package nonograms.algos

import nonograms.BoardState

// Given "2 4 11" on a 20 row can fill in 19 squares trivially.
// - - - - - - - - - - - - - - - - - - - -  2  4  11 -> 11+4+2 + 2gaps = 19 blocks will be filled.
// If we can fill in 5 squares on a 10 row, any clue of 6+ is going to mark 1+ blocks.
// This algo doesn't do anything very clever like look at gaps/ranges, what's already deleted or marked.  It just does
// the obvious.
case class AlgoMarkObvious() extends AlgoMini {
  def run(p: AlgoMiniParams): BoardState = {
    var curState = p.startState
    p.b.clues.clues.zipWithIndex.foreach(x => {
      val clue = x._1
      val index = x._2

      // Figure what possible range the clue could sit in
      val earlierClues = p.b.clues.clues.take(index)
      val laterClues = p.b.clues.clues.takeRight(p.b.clues.clues.length - index - 1)
      val minimalSpaceTakenByEarlierClues = if (index == 0) 0 else earlierClues.foldLeft(0)((acc, next) => acc + next + 1)
      val minimalSpaceTakenByLaterClues = if (index == p.b.clues.clues.length - 1) 0 else laterClues.foldLeft(0)((acc, next) => acc + next + 1)
      val possibleStartingIdx = minimalSpaceTakenByEarlierClues
      val possibleEndingIdx = p.b.lineLength - minimalSpaceTakenByLaterClues - 1

      curState = fillAsMuchOfRangeAsPossible(possibleStartingIdx,
        possibleEndingIdx,
        clue,
        curState,
        p.b
      )
    })
    curState
  }

  def fillAsMuchOfRangeAsPossible(possibleStartingIdx: Int, possibleEndingIdx: Int, clue: Int, curState: BoardState, ls: LineBasic): BoardState = {
    val possibleRange = possibleEndingIdx - possibleStartingIdx + 1

    val rangeNotCoveredByClue = possibleRange - clue
    val blocksMarkedByClue = clue - rangeNotCoveredByClue
    if (blocksMarkedByClue > 0) {
      val startingIdx = possibleStartingIdx + (clue - blocksMarkedByClue)
      val endingIdx = startingIdx + blocksMarkedByClue // exclusive range
      AlgoHelpers.markRange(ls, curState, adjust(startingIdx, ls), adjust(endingIdx, ls))
    }
    else curState
  }
}
