package nonograms.ui

import nonograms._
import nonograms.playing._
import org.scalajs.dom
import org.scalajs.dom.html.Div

import scalatags.JsDom.all._


object UIPlayer {
  val BOARD_SIZE = 10
  // Needs to sync up with css
  val SQUARE_SIZE = 40

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
  private[ui] val effects = new Effects(squareSizePixels = UIPlayer.SQUARE_SIZE, boardSize, boardSize)
  private val particlesDevSettingsWrapper = div().render
  private val particlesDevSettingsShow = button(cls := "btn btn-default", "Show dev settings").render
  particlesDevSettingsShow.onclick = (e) => {
    val debugOptions = new DebugOptions(this)
    val particlesDevSettings = new ParticlesDevSettings((ps) => effects.settingsChanged(ps))
    particlesDevSettingsShow.style.display = "none"
    particlesDevSettingsWrapper.appendChild(particlesDevSettings.rendered)
    particlesDevSettingsWrapper.appendChild(view.boardDebug)
    particlesDevSettingsWrapper.appendChild(debugOptions.rendered)
  }


  // These are what can update based on the user's actions
  private var boardState = init
  private var view: BoardStateView = BoardStateView.create(board, init, clues, this)

  private val rendered: Div = div(cls := "really-top")(
    particlesDevSettingsWrapper,
    particlesDevSettingsShow,
    log.rendered,
    div(cls := "top")(
      effects.rendered,
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
        effects.animateSquareMarkSuccess(row, col)

      case v: PlayerActionFailure =>
        effects.animateSquareMarkFailure(row, col)

      case _ =>
        println("no op")
    }

    checkSolved()

  }

  override def onRightClick(row: Int, col: Int): Unit = {
    System.out.println(s"Square right clicked $row $col")
    PlayerActionHandler.squareDeleteAttempt(board, boardState, row, col) match {

      case v: PlayerActionSuccess =>
        boardState = boardState.delete(row, col)
        while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
        viewWrapper.appendChild(view.rendered)
        effects.animateSquareDeleteSuccess(row, col)

      case v: PlayerActionFailure =>
        effects.animateSquareDeleteFailure(row, col)

      case _ =>
    }

    view = BoardStateView.update(view, boardState)
    while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
    viewWrapper.appendChild(view.rendered)

    checkSolved()
  }

  private var solved = false

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
}


