package nonograms

import scala.collection.mutable.ArrayBuffer

// E.g. 2,4,1 - block of 2, then 4, then 1. Can be row or col
case class LineClues(clues: Vector[Int]) {
  // Should always have at least "0"
  assert (clues.nonEmpty)

  def flip(): LineClues = copy(clues = clues.reverse)

  // Empty row
  def isEmpty(): Boolean = clues.length == 1 && clues.head == 0

  // Total blocks filled in by clues, not including gaps
  def totalFilled(): Int = clues.sum

  // Minimum space that can be taken by gaps
  def minimalGaps(): Int = clues.length - 1

  // Minimal amount of space taken up, assuming a one-gap between clues
  def minimum(): Int = clues.sum + clues.length - 1

  // Finds the number of clues that are equal to or above the input
  def possibleClues(input: Int): Int = clues.count(v => v >= input)

  // Finds the number of clues that are equal to above the input
  def exactClues(input: Int): Int = clues.count(v => v == input)

  // Returns the length of the largest clue(s)
  def biggest(): Int = clues.max

  def areAllSame(): Boolean = clues.toSet.size == 1

  override def toString = "[" + clues.mkString(",") + "]"
}


case class Clues(rows: Vector[LineClues], cols: Vector[LineClues]) {
  def getRow(row: Int) = rows(row)

  def getCol(col: Int) = cols(col)

  def flipHorizontally(): Clues = Clues(rows.map(v => v.flip()), cols)


  def flipVertically(): Clues = Clues(rows, cols.map(v => v.flip()))
}

object Clues {
  def generateClues(board: Board): Clues = {
    val rowClues = Range(0, board.numRows()).map(row => generateRowClues(board, row))
    val colClues = Range(0, board.numCols()).map(col => generateColClues(board, col))

    Clues(rowClues.toVector, colClues.toVector)
  }

  private def generateRowClues(board: Board, row: Int): LineClues = {
    val data = board.getRowData(row)
    generateClues(data)
  }

  private def generateColClues(board: Board, col: Int): LineClues = {
    val data = board.getColData(col)
    generateClues(data)
  }

  def generateClues(in: Vector[Boolean]): LineClues = {
    val out = ArrayBuffer.empty[Int]
    var len = 0
    for (i <- in.indices) {
      val isFilled = in(i)
      if (isFilled) {
        len += 1
      }
      else {
        if (len != 0) {
          out += len
        }
        len = 0
      }
    }

    if (len != 0) {
      out += len
    }

    if (out.isEmpty) {
      out += 0
    }

    LineClues(out.toVector)
  }
}