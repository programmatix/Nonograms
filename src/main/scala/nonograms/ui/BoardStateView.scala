package nonograms.ui

import nonograms._
import org.scalajs.dom
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

  var startCollecting = false
  val sizeOfSquare = UIPlayer.SQUARE_SIZE

  squares.onmousedown = (e) => {
    e.preventDefault()
    startCollecting = true
    val leftMouse = e.button == 0
    dom.console.info(e)

    val squaresBounds = squares.getBoundingClientRect()

    val startX = e.clientX.toInt - squaresBounds.left.toInt
    val startY = e.clientY.toInt - squaresBounds.top.toInt

    val maxSquareX = board.numCols()
    val maxSquareY = board.numRows()

    var lastSquareX = startX
    var lastSquareY = startY

    val startSquareX: Int = startX / sizeOfSquare
    val startSquareY: Int = startY / sizeOfSquare

    var verticalMode = false
    var horizontalMode = false

    println(s"onmousedown x=${startX} idx=${startSquareX} left=${squaresBounds.left} top=${squaresBounds.top}")

    squares.onmouseleave = (e) => {
      println("onleave")
      handler.clearOverlay()

      dom.document.onmousemove = (e) => {}
      squares.onmouseleave = (e) => {}
      squares.onmouseup = (e) => {}

    }

    dom.document.onmousemove = (e) => {
      e.preventDefault()
      val squaresBounds = squares.getBoundingClientRect()

      val x = e.clientX.toInt - squaresBounds.left.toInt
      val y = e.clientY.toInt - squaresBounds.top.toInt

      val squareX: Int = Math.min(Math.max(x / sizeOfSquare, 0), maxSquareX - 1)
      val squareY: Int = Math.min(Math.max(y / sizeOfSquare, 0), maxSquareY - 1)

      if (lastSquareX != squareX || lastSquareY != squareY) {
        lastSquareX = squareX
        lastSquareY = squareY

        if (!verticalMode && !horizontalMode) {
          if (squareX != startSquareX) {
            horizontalMode = true
          }
          else if (squareY != startSquareY) {
            verticalMode = true
          }
        }

        if (horizontalMode) {
          handler.drawOverlayHorizontal(startSquareX, squareX, startSquareY)
          println(s"x ${startSquareX}-${squareX}")
        }
        else if (verticalMode) {
          handler.drawOverlayVertical(startSquareY, squareY, startSquareX)
          println(s"y ${startSquareY}-${squareY}")
        }
        else {
          println(s"x ${startSquareX}-${squareX} y ${startSquareY}-${squareY} vm=${verticalMode} hm=$horizontalMode")
        }
      }
    }

    squares.onmouseup = (e) => {
      println("onmouseup")

      val squaresBounds = squares.getBoundingClientRect()

      val x = e.clientX.toInt - squaresBounds.left.toInt
      val y = e.clientY.toInt - squaresBounds.top.toInt

      val squareX: Int = Math.min(Math.max(x / sizeOfSquare, 0), maxSquareX - 1)
      val squareY: Int = Math.min(Math.max(y / sizeOfSquare, 0), maxSquareY - 1)

      if (horizontalMode) {
        if (squareX >= startSquareX) {
          for (idx <- Range(startSquareX, squareX + 1)) {
            if (leftMouse) handler.onLeftClick(startSquareY, idx)
            else handler.onRightClick(startSquareY, idx)
          }
        }
        else {
          for (idx <- Range(squareX, startSquareX + 1)) {
            if (leftMouse) handler.onLeftClick(startSquareY, idx)
            else handler.onRightClick(startSquareY, idx)
          }
        }
      }
      else if (verticalMode) {
        if (squareY >= startSquareY) {
          for (idx <- Range(startSquareY, squareY + 1)) {
            if (leftMouse) handler.onLeftClick(idx, startSquareX)
            else handler.onRightClick(idx, startSquareX)
          }
        }
        else {
          for (idx <- Range(squareY, startSquareY + 1)) {
            if (leftMouse) handler.onLeftClick(idx, startSquareX)
            else handler.onRightClick(idx, startSquareX)
          }
        }
      }

      handler.clearOverlay()

      dom.document.onmousemove = (e) => {}
      squares.onmouseleave = (e) => {}
      squares.onmouseup = (e) => {}
    }
  }


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

  def drawOverlayHorizontal(start: Int, end: Int, row: Int): Unit = {}
  def drawOverlayVertical(start: Int, end: Int, col: Int): Unit = {}
  def clearOverlay(): Unit = {}
}

object BoardStateView {
  def create(board: Board, bs: BoardState, clues: Clues, handler: BoardActionHandler): BoardStateView = {
    BoardStateView(board, bs, clues, handler)
  }

  def update(view: BoardStateView, bs: BoardState): BoardStateView = {
    create(view.board, bs, view.clues, view.handler)
  }
}
