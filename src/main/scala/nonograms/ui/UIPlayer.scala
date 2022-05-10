package nonograms.ui

import java.util.Date
import scala.scalajs.js.timers.setTimeout

import nonograms._
import org.scalajs.dom
import org.scalajs.dom.html.Div

import scalatags.JsDom.all._


object UIPlayer {
  val BOARD_SIZE = 10
  // Needs to sync up with css
  val SQUARE_SIZE_PX = 40

}

// For someone playing a Nonograms
class UIPlayer(containerId: String) extends BoardActionHandler {
  private val boardSize = UIPlayer.BOARD_SIZE
  private val created = Creator.createRandomSolvable(CreatorParams(boardSize, boardSize))
  println(s"Took ${created.iterations} iterations to create board of size $boardSize")
  private val board = created.board
  private val init = BoardState.initialise(board)
  private val clues = Clues.generateClues(board)
  private val container = dom.document.getElementById(containerId)
  private val log = new MessageLog()
  private val viewWrapper = div(cls := "view-wrapper").render
  private val particlesDevSettingsWrapper = div().render
  private val particlesDevSettingsShow = button(cls := "btn btn-default", "Show dev settings").render
  particlesDevSettingsShow.onclick = (e) => {
    //val debugOptions = new DebugOptions(this)
    val particlesDevSettings = new ParticlesDevSettings((ps) => effects.settingsChanged(ps))
    particlesDevSettingsShow.style.display = "none"
    particlesDevSettingsWrapper.appendChild(particlesDevSettings.rendered)
    particlesDevSettingsWrapper.appendChild(view.boardDebug)
    //particlesDevSettingsWrapper.appendChild(debugOptions.rendered)
  }
  private val mistakes = div(cls := "mistakes")("0").render
  private val time = div(cls := "time")("0:00").render

  // These are what can update based on the user's actions, everything else is immutable
  private var boardState = init
  private var view: BoardStateView = BoardStateView.create(board, init, clues, this)
  private var solved = false
  private var started = false
  private var mistakesCount = 0
  private[ui] val effects = new Effects(squareSizePixels = UIPlayer.SQUARE_SIZE_PX, boardSize, boardSize)

  private val rendered: Div = div(cls := "really-top")(
    particlesDevSettingsWrapper,
    particlesDevSettingsShow,
    log.rendered,
    div(cls := "time-and-mistakes")(
      div(cls := "time-holder")(
        div(cls := "time-label", "Time"),
        time
      ),
      div(cls := "mistakes-holder")(
        div(cls := "mistakes-label", "Mistakes"),
        mistakes
      ),
    ),
    div(cls := "top")(
      effects.rendered,
      viewWrapper
    )
  ).render

  // Enough space for board, max number of clues for that size board (hardcoded to support a 10x10 board size), plus some wiggle room
  private val minRequiredSpacePixels = UIPlayer.SQUARE_SIZE_PX * (UIPlayer.BOARD_SIZE + 5 + 1)
  private val actualSpacePixels = container.getBoundingClientRect().width

  println(s"minRequired=$minRequiredSpacePixels actual=$actualSpacePixels square=$UIPlayer.SQUARE_SIZE_PX")

  if (actualSpacePixels < minRequiredSpacePixels) {
    val tooSmall = div(cls := "too-small-wrapper")(
      div(cls := "too-small", "Sorry, this game isn't mobile or tablet friendly: it requires a larger window to display, plus a mouse for right-clicking")
    ).render
    container.appendChild(tooSmall)
  }
  else {
    viewWrapper.appendChild(view.rendered)
    container.appendChild(rendered)
  }

  override def onLeftClick(row: Int, col: Int): Unit = {
    handleStarted()

    System.out.println(s"Square clicked $row $col")
    // TODO: appears to be unchecked-in code...
//    PlayerActionHandler.squareMarkAttempt(board, boardState, row, col) match {
//
//      case v: PlayerActionSuccess =>
//        boardState = boardState.mark(row, col)
//        view = BoardStateView.update(view, boardState)
//        while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
//        viewWrapper.appendChild(view.rendered)
//        effects.animateSquareMarkSuccess(row, col)
//
//      case v: PlayerActionFailure =>
//        effects.animateSquareMarkFailure(row, col)
//        mistakesCount += 1
//        mistakes.textContent = mistakesCount.toString
//
//      case _ =>
//        println("no op")
//    }

    checkSolved()

  }

  override def onRightClick(row: Int, col: Int): Unit = {
    handleStarted()

    System.out.println(s"Square right clicked $row $col")
    // TODO: appears to be unchecked-in code...
//    PlayerActionHandler.squareDeleteAttempt(board, boardState, row, col) match {
//
//      case v: PlayerActionSuccess =>
//        boardState = boardState.delete(row, col)
//        while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
//        viewWrapper.appendChild(view.rendered)
//        effects.animateSquareDeleteSuccess(row, col)
//
//      case v: PlayerActionFailure =>
//        effects.animateSquareDeleteFailure(row, col)
//        mistakesCount += 1
//        mistakes.textContent = mistakesCount.toString
//
//      case _ =>
//    }

    view = BoardStateView.update(view, boardState)
    while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
    viewWrapper.appendChild(view.rendered)

    checkSolved()
  }


  def checkSolved(): Unit = {
    // Safety check is to prevent a drag across already marked squares completing the puzzle and causing multiple celebrations
    if (!solved) {
      val ret = BoardState.doesSolve(board, boardState)
      if (ret.isSolved) {
        solved = true
        println("Solved!")
        effects.solved()
      }
      else {
        println(ret.fails.mkString(","))
      }
    }
  }

  override def drawOverlayHorizontal(start: Int, end: Int, row: Int): Unit = {
    effects.drawOverlayHorizontal(start, end, row)
  }

  override def drawOverlayVertical(start: Int, end: Int, col: Int): Unit = {
    effects.drawOverlayVertical(start, end, col)
  }

  override def clearOverlay(): Unit = {
    effects.clearOverlay()
  }

  def handleStarted(): Unit = {
    if (!started) {
      started = true
      val startTime = new Date().getTime()
      keepUpdatingTimer(startTime)
    }
  }

  def keepUpdatingTimer(startTime: Long): Unit = {
    val curTime = new Date().getTime()
    val timeElapsedSecs = (curTime - startTime) / 1000
    val timeElapsedMod = timeElapsedSecs % 60
    val timeElapsedMins = timeElapsedSecs / 60
    time.textContent = timeElapsedMins.toString + ":" + timeElapsedMod.formatted("%02d")

    if (!solved) {
      setTimeout(1000) {
        keepUpdatingTimer(startTime)
      }
    }
  }
}


