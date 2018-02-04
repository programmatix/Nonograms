package nonograms.ui

import nonograms._
import nonograms.playing._
import org.scalajs.dom
import org.scalajs.dom.html.Div

import scalatags.JsDom.all._


// For someone playing a Nonograms
class UIPlayer(containerId: String) extends BoardActionHandler {
  private val boardSize = 10
  private val created = Creator.createRandomSolvable(CreatorParams(boardSize, boardSize))
  println(s"Took ${created.iterations} iterations to create board of size $boardSize")
  private val board = created.board
  private val init = BoardState.initialise(board)
  private val clues = Clues.generateClues(board)
  private val container = dom.document.getElementById(containerId)
  private val log = new MessageLog()
  private val viewWrapper = div(cls := "view-wrapper").render
  // 'squareSizePixels' needs to sync up with css
  private val overlay = new Overlay(squareSizePixels = 50, boardSize, boardSize)
  private val particlesDevSettingsWrapper = div().render
  private val particlesDevSettingsShow = button(cls := "btn btn-default", "Show dev settings").render
  particlesDevSettingsShow.onclick = (e) => {
    val particlesDevSettings = new ParticlesDevSettings((ps) => overlay.settingsChanged(ps))
    particlesDevSettingsShow.style.display = "none"
    particlesDevSettingsWrapper.appendChild(particlesDevSettings.rendered)
    particlesDevSettingsWrapper.appendChild(view.boardDebug)
  }


  // These are what can update based on the user's actions
  private var boardState = init
  private var view: BoardStateView = BoardStateView.create(board, init, clues, this)

  private val rendered: Div = div(cls := "really-top")(
    particlesDevSettingsWrapper,
    particlesDevSettingsShow,
    log.rendered,
    div(cls := "top")(
      overlay.rendered,
      viewWrapper
    )
  ).render

  viewWrapper.appendChild(view.rendered)
  container.appendChild(rendered)

  override def onLeftClick(row: Int, col: Int): Unit = {
    System.out.println(s"Square clicked $row $col")
    PlayerActionHandler.squareMarkAttempt(board, boardState, row, col) match {

      case v: PlayerActionSuccess =>
        boardState = boardState.mark(row, col)
        view = BoardStateView.update(view, boardState)
        while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
        viewWrapper.appendChild(view.rendered)
        overlay.animateSquareMarkSuccess(row, col)

      case v: PlayerActionFailure =>
        overlay.animateSquareMarkFailure(row, col)

      case _ =>
        println("no op")
    }

  }

  override def onRightClick(row: Int, col: Int): Unit = {
    System.out.println(s"Square right clicked $row $col")
    PlayerActionHandler.squareDeleteAttempt(board, boardState, row, col) match {

      case v: PlayerActionSuccess =>
        boardState = boardState.delete(row, col)
        while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
        viewWrapper.appendChild(view.rendered)
        overlay.animateSquareDeleteSuccess(row, col)

      case v: PlayerActionFailure =>
        overlay.animateSquareDeleteFailure(row, col)

      case _ =>
    }

    view = BoardStateView.update(view, boardState)
    while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
    viewWrapper.appendChild(view.rendered)
  }

  override def drawOverlayHorizontal(start: Int, end: Int, row: Int): Unit = {
    overlay.drawOverlayHorizontal(start, end, row)
  }

  override def drawOverlayVertical(start: Int, end: Int, col: Int): Unit = {
    overlay.drawOverlayVertical(start, end, col)
  }

  override def clearOverlay(): Unit = {
    overlay.clearOverlay()
  }
}


