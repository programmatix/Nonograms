package nonograms.ui

import nonograms._
import org.scalajs.dom
import org.scalajs.dom.html.Div

import scala.scalajs.js.timers.setTimeout
import scalatags.JsDom.all._

class MessageLog {
  val rendered: Div = div(cls := "messages-top")(
  ).render

  def addMessage(html: String): Unit = {
    val msg = div(html).render
    rendered.insertBefore(msg, rendered.firstChild)
  }
}

// For an AI auto-solving a puzzle
class UISolver(containerId: String) extends SolverInterface with BoardActionHandler {
  private val boardSize = 10
  private val board = Creator.createRandom(CreatorParams(boardSize, boardSize))
  private var init = BoardState.initialise(board)
  private val clues = Clues.generateClues(board)
  private var view: BoardStateView = BoardStateView.create(board, init, clues, this)

  private val container = dom.document.getElementById(containerId)
  private val log = new MessageLog()

  private val viewWrapper = div().render

  val rendered: Div = div(cls := "top")(
    viewWrapper,
    log.rendered
  ).render

  container.appendChild(rendered)

  private def solveNextStep(lastSolveState: SolveState): Unit = {
    val ret = Solver.solveWithUINonBlocking(board, clues, lastSolveState)
    ret.messages.foreach(m => emit(m))
    if (!ret.terminate) {
      setTimeout(10) {
        solveNextStep(ret.solveState)
      }
    }
  }

  solveNextStep(NotStarted())

  override def emit(command: SolverCommand): Unit = {
    command match {
      case v: StartNewIteration        => log.addMessage("""Starting new iteration""")
      case v: StartingNewIterationSoon => log.addMessage("""Starting new iteration soon""")
      case v: UnableToFindSolution     => log.addMessage(s"""Unable to find solution""")
      case v: FoundSolution            => log.addMessage(s"""Found solution""")
      case v: StartAlgoBlock           => log.addMessage(s"""Starting new block of algorithms: ${v.name}""")
      case v: StartAlgo                => log.addMessage(s"""Starting algorithm: ${v.name}""")
      case v: CompleteAlgo             => log.addMessage(s"""Completed algorithm: ${v.name} ${v.matchsFound}""")
      case v: NewBoardState            =>
        view = BoardStateView.create(board, v.board, clues, this)
        while (viewWrapper.hasChildNodes()) viewWrapper.removeChild(viewWrapper.lastChild)
        viewWrapper.appendChild(view.rendered)
    }
  }

  override def onLeftClick(row: Int, col: Int): Unit = {}

  override def onRightClick(row: Int, col: Int): Unit = {}
}
