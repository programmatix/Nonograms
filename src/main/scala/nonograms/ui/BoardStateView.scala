package nonograms.ui

import nonograms._
import org.scalajs.dom.Node
import org.scalajs.dom.html.Div

import scala.collection.mutable.ArrayBuffer
import scalatags.JsDom.all._

case class CluesRowView(clues: LineClues, row: Int) {
  val rendered: Div = div(cls := "clues-row")(
    clues.clues.map(clue =>
      div(cls := "clues-clue", clue.toString)
    )
  ).render
}

case class CluesColView(clues: LineClues, row: Int) {
  val rendered: Div = div(cls := "clues-col")(
    clues.clues.map(clue =>
      div(cls := "clues-clue", clue.toString)
    )
  ).render
}

case class SquareView(state: SquareState, row: Int, col: Int) {
  private val clas: String = state match {
    case v: SquareStateUntouched => "square-untouched"
    case v: SquareStateDeleted   => "square-deleted"
    case v: SquareStateMarked    => "square-marked"
  }
  val rendered: Div = div(cls := clas,
    onclick := (() => {
      System.out.println("Square clicked")
    })).render
}

case class BoardStateView(board: Board, bs: BoardState, clues: Clues) {
  private val boardDebug = div(cls := "board-debug").render
  boardDebug.innerHTML = board.toString.replace("\r\n","\n").replace("\n", "<br>")

  private val data: Vector2Dim[SquareView] = {
    val out = ArrayBuffer.empty[ArrayBuffer[SquareView]]

    for (row <- Range(0, bs.numRows())) {
      val rowData = ArrayBuffer.empty[SquareView]

      for (col <- Range(0, bs.numCols())) {
        val square = bs.get(row, col)
        val view = new SquareView(square, row, col)
        rowData += view
      }

      out += rowData
    }

    Vector2Dim.from(out)
  }

  private val rowsRendered = div(cls := "clues-block-rows")(
    clues.rows.zipWithIndex.map(clue => {
      CluesRowView(clue._1, clue._2).rendered
    })
  )

  private val colsRendered = div(cls := "clues-block-cols")(
    clues.cols.zipWithIndex.map(clue => {
      CluesColView(clue._1, clue._2).rendered
    })
  ).render

  private val squares = div(cls := "squares")(
    Range(0, data.rows).map(row =>
      div(cls := "squares-row")(
        Range(0, data.cols).map(col =>
          data.get(row, col).rendered
        )
      )
    )
  ).render


  val rendered: Node = div(cls := "board")(
    div(cls := "board-squares-rows")(
      squares,
      rowsRendered
    ),
    colsRendered,
    boardDebug
  ).render
}

object BoardStateView {
  def create(board: Board, bs: BoardState, clues: Clues): BoardStateView = {
    BoardStateView(board, bs, clues)
  }
}
