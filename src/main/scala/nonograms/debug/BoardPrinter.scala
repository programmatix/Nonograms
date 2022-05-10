package nonograms.debug

import nonograms.{BoardState, Clues, SquareStateDeleted, SquareStateMarked}

// Prints the board to stdout for debugging
object BoardPrinter {
  def print(bs: BoardState, clues: Clues): Unit = {
    val sb = new StringBuilder
    for (row <- Range(0, bs.data.rows)) {
      for (col <- Range(0, bs.data.cols)) {
        bs.get(row, col) match {
          case v: SquareStateDeleted => sb ++= "D "
          case v: SquareStateMarked => sb ++= "M "
          case _ => sb ++= "- "
        }
      }
      clues.getRow(row).clues.foreach(clue => {
        sb += ' '
        sb ++= clue.toString
        sb += ' '
      })
      if (row != bs.data.rows - 1) {
        sb += '\n'
      }
    }
    sb += '\n'

    val maxColClues = clues.cols.maxBy(_.clues.length).clues.length
    for (clueIdx <- Range(0, maxColClues)) {
      for (col <- Range(0, bs.data.cols)) {
        val cl = clues.getCol(col).clues
        if (cl.length > clueIdx) {
          sb ++= cl(clueIdx).toString.padTo(2, ' ')
        }
        else sb ++= "  "
      }
      sb += '\n'
    }

    println(sb.toString)
  }
}
