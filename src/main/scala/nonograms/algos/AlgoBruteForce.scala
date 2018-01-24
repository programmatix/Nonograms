package nonograms.algos

import nonograms._

object AlgoBruteForce {
  // Note this does not check the actual Board - no cheating here!
  // We're checking purely if the marks and deletes match up with the clues
  // Also note that this can pass, but be an illegal move on the board.  It's ok - it's intended to be used during
  // reductio ad absurdum, e.g. we will look at bad paths and reject them later.
  // Remember: we're not checking if it matches every clue, simply that it does't *violate* any
  def isValid(bs: BoardState, cluesAll: Clues, stopOnFail: Boolean = false): Boolean = {
    val numRows = cluesAll.rows.length
    val numCols = cluesAll.cols.length
    var out = true

    def doLine(clues: LineClues, line: LineState, row: Option[Int], col: Option[Int], lineLength: Int): Unit = {
      if (line.totalMarked() > clues.totalFilled()) {
        out = false
        if (stopOnFail) assert(false)
      }
      if (clues.clues.length > 1 && line.totalDeleted() > clues.minimalGaps()) {
        out = false
        if (stopOnFail) assert(false)
      }
      if (line.totalDeleted() > (lineLength - clues.minimum())) {
        out = false
        if (stopOnFail) assert(false)
      }

    }

    for (row <- Range(0, bs.numRows())) {
      val clues = cluesAll.getRow(row)
      val solved = bs.getRow(row)
      doLine(clues, solved, Some(row), None, bs.numCols())
    }

    for (col <- Range(0, bs.numCols())) {
      val clues = cluesAll.getCol(col)
      val solved = bs.getCol(col)
      doLine(clues, solved, None, Some(col), bs.numRows())
    }


    out
  }

  // Checks if the board is completely full of marks and deletes (regardless of whether correctly placed)
  def isFull(bs: BoardState): Boolean = {
    var out = true

    for (row <- Range(0, bs.numRows())) {
      if (out) {
        for (col <- Range(0, bs.numCols())) {
          if (out) {
            bs.get(row, col) match {
              case v: SquareStateUntouched => out = false
              case _                       =>
            }
          }
        }
      }
    }

    out
  }

  // Just for testing, this is a safety check to make sure the board state truly doesn't violate the board
  def isValidBackup(board: Board, bs: BoardState, cluesAll: Clues, shouldBeComplete: Boolean): Boolean = {
    BoardState.doesSolve(board, bs).isSolved
  }
}

// Solves through pure dumb brute force.  It does reductio ad absurdum so a human can use the same algo - this doesn't
// look at hidden board state at all, like all the algos.
// Could be expensive so try to solve as much of the puzzle as poss first.
case class AlgoBruteForce() extends Algorithm {

  case class RecurseResult(out: Option[BoardState], aborting: Boolean, maxLevelReached: Int)

  val TREE_LEVEL_MAX = 4

  // Returns a valid board iff it found a valid solution
  def solveRecurse(board: Board, state: BoardState, clues: Clues, treeLevels: Int): RecurseResult = {
    var out: Option[BoardState] = None
    var aborting = false
    var maxLevelReached = treeLevels

    def handle(newState: BoardState) {
      if (AlgoBruteForce.isFull(newState)) {
        // Board is full of marks/deletes, *should* be correct as we've passed an isValid check before
        assert(AlgoBruteForce.isValid(newState, clues))
        assert(AlgoBruteForce.isValidBackup(board, newState, clues, true))
        out = Some(newState)
      }
      else if (treeLevels < TREE_LEVEL_MAX) {
        val ret = solveRecurse(board, newState, clues, treeLevels + 1)
        if (ret.out.isDefined) out = ret.out
        if (ret.maxLevelReached > maxLevelReached) maxLevelReached = ret.maxLevelReached
      }
      else {
//        println(s"Giving up on brute force after $treeLevels levels")
//        aborting = true
      }
    }


    for (row <- Range(0, board.numRows())) {
      for (col <- Range(0, board.numCols())) {
        if (out.isEmpty && !aborting) {
          val square = state.get(row, col)

          if (square.isUntouched()) {
            val newStateAfterMark = Algorithm.mark(row, col, board, state, allowBadMoves = true)

            if (AlgoBruteForce.isValid(newStateAfterMark, clues)) {
              //println(s"Trying marked ${row} ${col} level=$treeLevels")
              handle(newStateAfterMark)
            }
            else {
              val newStateAfterDelete = Algorithm.delete(row, col, board, state, allowBadMoves = true)

              if (AlgoBruteForce.isValid(newStateAfterDelete, clues)) {
                //println(s"Trying deleted ${row} ${col} level=$treeLevels")
                handle(newStateAfterDelete)
              }
            }
          }
        }
      }
    }

    RecurseResult(out, aborting, maxLevelReached)
  }

  def solve(s: ForSolver): SolverResult = {
    val ret = solveRecurse(s.board, s.state, s.clues, 0)
    ret.out match {
      case Some(v: BoardState) => SolverResult(v, ret.maxLevelReached)
      case _                   => SolverResult(s.state, ret.maxLevelReached)
    }
  }
}
