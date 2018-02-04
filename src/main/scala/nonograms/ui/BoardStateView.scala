package nonograms.ui

import nonograms._
import org.scalajs.dom.html.Div
import org.scalajs.dom.{Event, Node}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.timers.setTimeout
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

case class SquareView(state: SquareState, row: Int, col: Int, onLeftClick: (Int, Int) => Unit, onRightClick: (Int, Int) => Unit) {
  private val clas: String = state match {
    case v: SquareStateUntouched => "square-untouched"
    case v: SquareStateDeleted   => "square-deleted"
    case v: SquareStateMarked    => "square-marked"
  }

  val rendered: Div = div(cls := clas,
    onclick := (() => {
      onLeftClick(row, col)
    })).render

  rendered.oncontextmenu = (event: Event) => {
      println("right click")
      onRightClick(row, col)
      // Don't show the context menu
      event.preventDefault()
    }
}

case class BoardStateView(board: Board, bs: BoardState, clues: Clues, handler: BoardActionHandler) {
  val boardDebug = div(cls := "board-debug").render
  boardDebug.innerHTML = "Board:<br>" + board.toString.replace("\r\n", "\n").replace("\n", "<br>")

  private val data: Vector2Dim[SquareView] = {
    val out = ArrayBuffer.empty[ArrayBuffer[SquareView]]

    for (row <- Range(0, bs.numRows())) {
      val rowData = ArrayBuffer.empty[SquareView]

      for (col <- Range(0, bs.numCols())) {
        val square = bs.get(row, col)
        val view = SquareView(square, row, col, handler.onLeftClick, handler.onRightClick)
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
    colsRendered
  ).render

}

trait Animation {
  var done: Boolean
}

case class AnimationFailure(square: SquareView, onFinished: () => Unit) extends Animation {
  var done = false
  square.rendered.className = square.rendered.className + " square-failure-anim"
  setTimeout(1000) {
    println("Ending failure")
    done = true
    square.rendered.className = square.rendered.className.replace(" square-failure-anim", "").trim()
    onFinished()
  }
}


trait BoardActionHandler {
  def onLeftClick(row: Int, col: Int): Unit
  def onRightClick(row: Int, col: Int): Unit
}

object BoardStateView {
  def create(board: Board, bs: BoardState, clues: Clues, handler: BoardActionHandler): BoardStateView = {
    BoardStateView(board, bs, clues, handler)
  }

  def update(view: BoardStateView, bs: BoardState): BoardStateView = {
    create(view.board, bs, view.clues, view.handler)
  }
}
