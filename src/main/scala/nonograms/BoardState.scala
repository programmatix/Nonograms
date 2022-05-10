package nonograms

import nonograms.algos.LineBasic

import scala.collection.mutable.ArrayBuffer

trait SquareState {
  def isMarked() = this.isInstanceOf[SquareStateMarked]

  def isDeleted() = this.isInstanceOf[SquareStateDeleted]

  def isUntouched() = this.isInstanceOf[SquareStateUntouched]
}

case class SquareStateDeleted() extends SquareState

case class SquareStateMarked() extends SquareState

case class SquareStateUntouched() extends SquareState

sealed trait Run {
  val index: Int
  val length: Int
  def end = index + length
}
case class Untouched(index: Int, length: Int) extends Run
case class Marked(index: Int, length: Int) extends Run
case class Deleted(index: Int, length: Int) extends Run


// toIdx is inclusive
case class MarkRange(fromIdx: Int, toIdx: Int, marked: Int) {
  def len(): Int = (toIdx - fromIdx) + 1

  // Remember this only knows if a range between two deletes (or the board edges) was filled with marks - it's not
  // comparing it to clues, which is more complex logic
  def isComplete() = len() == marked
}

case class LineState(squares: Vector[SquareState]) {
  def remainingAfterDeleted(): Int = squares.length - totalDeleted()

  // All deleted squares are contiguous at the edges.  Returns true if no deleted on both edges.  Returns true if no
  // deleted on one edge, but on the other they are contiguous.  Returns true if line is nothing but deletes.
  def areAllDeletedAtEdges(): Boolean = {
    var out = true
    var foundGapInDeleteds = false

    var endOfStartingRangeOfDeleteds = 0
    var done = false

    for (idx <- squares.indices) {
      if (!done) {
        val square = squares(idx)
        if (!square.isDeleted()) {
          endOfStartingRangeOfDeleteds = idx
          done = true
        }
      }
    }

    var startOfEndRangeOfDeleteds = 0
    done = false

    for (idx <- squares.indices.reverse) {
      if (!done) {
        val square = squares(idx)
        if (!square.isDeleted()) {
          startOfEndRangeOfDeleteds = idx
          done = true
        }
      }
    }

    for (idx <- Range(endOfStartingRangeOfDeleteds, startOfEndRangeOfDeleteds)) {
      val square = squares(idx)
      if (square.isDeleted()) out = false

    }

    out
  }

  def totalDeleted(): Int = {
    squares.count {
      case v: SquareStateDeleted => true
      case _                     => false
    }
  }

  def totalMarked(): Int = {
    squares.count {
      case v: SquareStateMarked => true
      case _                    => false
    }
  }

  def isComplete(): Boolean = {
    val incompleteSquares = squares.count {
      case v: SquareStateUntouched => true
      case _                       => false
    }
    incompleteSquares == 0
  }

  def idxOfFirstMark(): Option[Int] = {
    var out: Option[Int] = None
    for (idx <- squares.indices) {
      if (out.isEmpty) {
        val square = squares(idx)
        if (square.isMarked()) {
          out = Some(idx)
        }
      }
    }
    out
  }

  def idxOfFirstNotDeleted(): Option[Int] = {
    var out: Option[Int] = None
    for (idx <- squares.indices) {
      if (out.isEmpty) {
        val square = squares(idx)
        if (!square.isDeleted()) {
          out = Some(idx)
        }
      }
    }
    out
  }

  def idxOfLastMark(): Option[Int] = {
    var out: Option[Int] = None
    for (idx <- squares.indices.reverse) {
      if (out.isEmpty) {
        val square = squares(idx)
        if (square.isMarked()) {
          out = Some(idx)
        }
      }
    }
    out
  }

  // Range = a series of squares between Ds.  E.g. it's a gap.  One or more clues could fit in that range.
  // Stretch = a series of marked squares

  // Returns stretches between Ds and the board edges.  Ignores marked squares
  // "--D--" = [2,2]
  // "-----" = [5]
  // "----D" = [4]
  // "-M--D" = [4]
  // "D-DMD" = [1,1]
  def getRanges(): Vector[Int] = {
    var inRange = false
    var startOfRange = 0
    val out = ArrayBuffer.empty[Int]

    for (idx <- squares.indices) {
      val square = squares(idx)
      square match {
        case v: SquareStateDeleted   =>
          if (inRange) out += idx - startOfRange
          inRange = false
        case v: SquareStateMarked    =>
          if (!inRange) {
            startOfRange = idx
            inRange = true
          }
        case v: SquareStateUntouched =>
          if (!inRange) {
            startOfRange = idx
            inRange = true
          }
      }
    }

    if (inRange) out += squares.length - startOfRange

    out.toVector
  }

  // Returns stretches between Ds and the board edges.  Ignores marked squares
  def getMarkRanges(): Vector[MarkRange] = {
    var inRange = false
    var startOfRange = 0
    val out = ArrayBuffer.empty[MarkRange]
    var marked = 0

    for (idx <- squares.indices) {
      val square = squares(idx)
      square match {
        case v: SquareStateDeleted   =>
          if (inRange) out += MarkRange(startOfRange, idx - 1, marked)
          inRange = false
          marked = 0
        case v: SquareStateMarked    =>
          if (!inRange) {
            startOfRange = idx
            inRange = true
            marked += 1
          }
          else {
            marked += 1
          }
        case v: SquareStateUntouched =>
          if (!inRange) {
            startOfRange = idx
            inRange = true
          }
      }
    }

    if (inRange) out += MarkRange(startOfRange, squares.length - 1, marked)

    out.toVector
  }

  // Returns stretches of marks
  // "-MMMM-" -> [4]
  // "-MMD-M" -> [2,1]
  def getMarkedStretches(): Vector[MarkRange] = {
    var inStretch = false
    var startOfStretch = 0
    val out = ArrayBuffer.empty[MarkRange]
    var marked = 0

    for (idx <- squares.indices) {
      val square = squares(idx)
      square match {
        case v: SquareStateDeleted   =>
          if (inStretch) out += MarkRange(startOfStretch, idx - 1, marked)
          inStretch = false
          marked = 0
        case v: SquareStateMarked    =>
          if (!inStretch) {
            startOfStretch = idx
            inStretch = true
            marked += 1
          }
          else {
            marked += 1
          }
        case v: SquareStateUntouched =>
          if (inStretch) out += MarkRange(startOfStretch, idx - 1, marked)
          inStretch = false
          marked = 0
      }
    }

    if (inStretch) out += MarkRange(startOfStretch, squares.length - 1, marked)

    out.toVector

  }

  override def toString(): String = {
    val sb = new StringBuilder
    squares.foreach {
      case v: SquareStateDeleted   => sb += 'D'
      case v: SquareStateMarked    => sb += 'M'
      case v: SquareStateUntouched => sb += '-'
    }
    sb.toString()
  }
}

// What's been done to the board
case class BoardState(data: Vector2Dim[SquareState]) {
  def getLine(b: LineBasic): LineState = {
    b.row match {
      case Some(row) => getRow(row)
      case _ =>
        getCol(b.col.get)
    }
  }

  def delete(row: Int, col: Int): BoardState = {
    val copied = data.set(row, col, SquareStateDeleted())
    BoardState(copied)
  }

  def mark(row: Int, col: Int): BoardState = {
    val copied = data.set(row, col, SquareStateMarked())
    BoardState(copied)
  }

  def numRows() = data.rows

  def numCols() = data.cols

  def get(row: Int, col: Int): SquareState = data.get(row, col)

  def isNotMarkedOrDeleted(row: Int, col: Int): Boolean = data.get(row, col).isUntouched()

  def getRowMarks(row: Int): Vector[Boolean] = Range(0, data.cols).map(col => get(row, col) == SquareStateMarked()).toVector

  def getColMarks(col: Int): Vector[Boolean] = Range(0, data.rows).map(row => get(row, col) == SquareStateMarked()).toVector

  def getRow(row: Int): LineState = LineState(Range(0, data.cols).map(col => get(row, col)).toVector)

  def getCol(col: Int): LineState = LineState(Range(0, data.rows).map(row => get(row, col)).toVector)

  def numSquaresUnfilled(): Int = {
    var out = 0
    for (row <- Range(0, data.rows)) {
      for (col <- Range(0, data.cols)) {
        get(row, col) match {
          case v: SquareStateUntouched => out += 1
          case _ =>
        }
      }
    }
    out

  }

  def flipHorizontally(): BoardState = {
    BoardState(data.flipHorizontally())
  }

  def flipVertically(): BoardState = {
    BoardState(data.flipVertically())
  }

  override def toString: String = {
    val sb = new StringBuilder
    for (row <- Range(0, data.rows)) {
      for (col <- Range(0, data.cols)) {
        get(row, col) match {
          case v: SquareStateDeleted => sb += 'D'
          case v: SquareStateMarked  => sb += 'M'
          case _                     => sb += '-'
        }
      }
      if (row != data.rows - 1) {
        sb += '\n'
      }
    }
    sb.toString
  }

}


trait SolveFails

case class SolveFailUnfilledMarked(row: Int, col: Int) extends SolveFails

case class SolveFailFilledDeleted(row: Int, col: Int) extends SolveFails

case class SolveFailFilledNotMarked(row: Int, col: Int) extends SolveFails

case class SolveFailUnfilledNotDeleted(row: Int, col: Int) extends SolveFails

case class SolveResult(fails: Seq[SolveFails]) {
  def isSolved = fails.isEmpty
}

object BoardState {
  def initialise(board: Board): BoardState = {
    val state = Vector2Dim.create[SquareState](board.numRows(), board.numCols(), SquareStateUntouched())
    BoardState(state)
  }

  // All empty squares will be marked already deleted.  Good for testing
  def initialiseWithClearedEmpties(board: Board): BoardState = {
    val copied = ArrayBuffer.empty[ArrayBuffer[SquareState]]

    for (r <- Range(0, board.numRows())) {
      val rowData = ArrayBuffer.empty[SquareState]
      copied += rowData

      for (c <- Range(0, board.numCols())) {
        rowData += (if (board.isFilled(r, c)) SquareStateUntouched() else SquareStateDeleted())
      }
    }

    val data = Vector2Dim.arrayBufferToVector(copied)

    BoardState(Vector2Dim[SquareState](data))
  }

  // ignoreUndeletedUntouched added for tests, so they just have to mark the filled tiles and not delete the unfilled
  def doesSolve(board: Board, state: BoardState, ignoreUndeletedUntouched: Boolean = false): SolveResult = {
    val result = ArrayBuffer.empty[SolveFails]

    for (row <- Range(0, board.numRows())) {
      for (col <- Range(0, board.numCols())) {
        val b = board.isFilled(row, col)
        val s = state.get(row, col)

        s match {
          case v: SquareStateMarked =>
            if (!b) {
              result += SolveFailUnfilledMarked(row, col)
            }

          case v: SquareStateDeleted =>
            if (b) {
              result += SolveFailFilledDeleted(row, col)
            }

          case v: SquareStateUntouched =>
            if (b) result += SolveFailFilledNotMarked(row, col)
            else if (!ignoreUndeletedUntouched) result += SolveFailUnfilledNotDeleted(row, col)
        }
      }
    }

    SolveResult(result.toSeq)
  }

}
