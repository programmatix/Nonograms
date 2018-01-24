package nonograms

import scala.collection.mutable.ArrayBuffer




// Initial state of board, never changes
case class Board(board: Vector2Dim[Boolean]) {
  def isFilled(row: Int, col: Int): Boolean = board.get(row, col)

  def numRows(): Int = board.rows

  def numCols(): Int = board.cols

  def getRowData(row: Int): Vector[Boolean] = board.getRow(row)
  def getColData(col: Int): Vector[Boolean] = board.getCol(col)

  def flipHorizontally(): Board = {
    Board(board.flipHorizontally())
  }

  override def toString: String = {
    val sb = new StringBuilder
    for (row <- Range(0, numRows())) {
      for (col <- Range(0, numCols())) {
        if (isFilled(row, col)) sb += 'X'
        else sb += '-'
      }
      sb += '\n'
    }
    sb.toString
  }
}




object Board {
  // input = """XXXOO
  //            XOOXX
  //            OXXXX"""
  def create(inputRaw: String): Board = {
    val input = inputRaw.trim().replace("\r\n", "\n")
    val rows = input.count((c) => c == '\n') + 1
    val cols = if (inputRaw.contains('\n')) input.indexOf('\n') else inputRaw.length
//    val board = Vector2Dim.create[Boolean](rows, cols, false)

    val board = ArrayBuffer.fill(rows, cols)(false)

    val ch = input.iterator
    var row = 0
    var col = 0
    while (ch.hasNext) {
      val c = ch.next()
      val isFilled = c == 'X'

      if (c == '\n') {
        row += 1
        col = 0
      }
      else {
        board(row)(col) = isFilled
        col += 1
      }
    }

    val mapped = Vector2Dim[Boolean](Vector2Dim.arrayBufferToVector(board))
    Board(mapped)
  }

  def attempt(action: Action, board: Board, state: BoardState, allowBadMoves: Boolean = false): ActionResult = {
    action match {
      case v: ActionMark =>
        if (board.isFilled(v.row, v.col) || allowBadMoves) {
          val newState = state.mark(v.row, v.col)
          ActionSuccess(newState)
        }
        else {
          ActionFailure()
        }

      case v: ActionDelete =>
        if (!board.isFilled(v.row, v.col) || allowBadMoves) {
          val newState = state.delete(v.row, v.col)
          ActionSuccess(newState)
        }
        else {
          ActionFailure()
        }
    }
  }




}