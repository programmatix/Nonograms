package nonograms

import java.util.Date
import nonograms.algos._
import nonograms.debug.BoardPrinter

sealed trait SolverCommand

case class StartNewIteration() extends SolverCommand

case class StartingNewIterationSoon() extends SolverCommand

case class UnableToFindSolution() extends SolverCommand

case class FoundSolution() extends SolverCommand

case class StartAlgoBlock(name: String) extends SolverCommand

case class StartAlgo(name: String) extends SolverCommand

case class CompleteAlgo(name: String, matchsFound: Boolean) extends SolverCommand

case class NewBoardState(board: BoardState) extends SolverCommand

trait SolverInterface {
  def emit(command: SolverCommand)
}

case class SolverUIParams(
                           timeBetweenCommandsMsec: Int = 1000
                         )

// AI to solve any puzzle
// Want to be able to produce a hint system from this too, so build along lines of iteratively trying next obvious thing
object Solver {
  val MAX_UNSOLVED_SQUARES_FOR_BRUTE_FORCE = 0

  // These are obvious algos can apply to an initial board
  private val initialAlgos = Vector[Algorithm](
    AlgoClearEmptyLines(),
    AlgoFullLines(),
    AlgoOnlyInnerSingleGaps(),
    AlgoMarkMiddleSingleClue(),
    // AlgoMarkObvious included here since it doesn't look at board state
    AlgoMinis(Seq(AlgoMarkObvious()))
  )

  // These are the algos to keep iterating until the puzzle is solved
  private val iterateAlgos = Vector[Algorithm](
//    AlgoEachCluedStretchHasOwnRange(),
    AlgoMinis(AlgoMini.all),
    AlgoStartOfClueAlreadyMarked(),
    AlgoNearStartOfClueAlreadyMarked(),
    AlgoDeleteRemainingFromSolvedLines(),
    AlgoDeleteImpossibleSingle(),
    AlgoDeleteNoSpaceForSingle(),
    AlgoOnlyInnerSingleGapsAfterDeletesAtEdges(),
    AlgoMarkObviousRemaining(),
    AlgoDeleteNextToCompletedRanges()
  )

  private val lastDitchAlgos = Vector[Algorithm](
    //    AlgoBruteForce()
  )

  def applyAlgo(algo: Algorithm, board: Board): SolverResult = {
    var curState = BoardState.initialise(board)
    applyAlgoWithState(algo, board, curState)
  }

  def applyAlgoWithState(algo: Algorithm, board: Board, curState: BoardState): SolverResult = {
    val clues = Clues.generateClues(board)
    algo.solve(ForSolver(board, clues, curState))
  }

  // Returns true iff solvable
  def solve(board: Board): Boolean = {
    var curState = BoardState.initialise(board)
    val clues = Clues.generateClues(board)
    var done = false

    initialAlgos.foreach(algo => {
      curState = algo.solve(ForSolver(board, clues, curState)).last
    })

    BoardPrinter.print(curState, clues)

    while (!BoardState.doesSolve(board, curState).isSolved && !done) {
      val lastBoardState = curState

      iterateAlgos.foreach(algo => {
        curState = algo.solve(ForSolver(board, clues, curState)).last
        //BoardPrinter.print(curState, clues)
      })

      // Unable to improve the state
      if (curState == lastBoardState) {
        BoardPrinter.print(curState, clues)
        done = true
      }
    }

    val solved = BoardState.doesSolve(board, curState)
    solved.isSolved
  }

  def waitBetweenCommands(params: SolverUIParams) = {
    //    Thread.sleep(params.timeBetweenCommandsMsec)
  }

  def solveWithUI(board: Board, out: SolverInterface, params: SolverUIParams): Unit = {
    val start = (new Date()).getTime
    var curState = BoardState.initialise(board)
    val clues = Clues.generateClues(board)

    def runAlgo(algo: Algorithm): Unit = {
      out.emit(StartAlgo(algo.getClass.getSimpleName))
      val initialState = curState

      curState = algo.solve(ForSolver(board, clues, curState)).last

      waitBetweenCommands(params)
      val anyMatches = initialState != curState
      out.emit(CompleteAlgo(algo.getClass.getSimpleName, anyMatches))

      if (anyMatches) {
        out.emit(NewBoardState(curState))
        waitBetweenCommands(params)
      }
    }

    out.emit(NewBoardState(curState))
    out.emit(StartAlgoBlock("Initial"))
    waitBetweenCommands(params)

    initialAlgos.foreach(algo => {
      runAlgo(algo)
    })

    var done = false

    while (!done) {
      out.emit(StartAlgoBlock("Iterating"))
      waitBetweenCommands(params)

      val initialState = curState

      iterateAlgos.foreach(algo => {
        runAlgo(algo)
      })

      val solved = BoardState.doesSolve(board, curState).isSolved

      if (solved) {
        val end = new Date().getTime
        out.emit(FoundSolution())
        done = true
      }
      else {
        if (initialState == curState) {
          if (curState.numSquaresUnfilled() < MAX_UNSOLVED_SQUARES_FOR_BRUTE_FORCE) {
            // Did a whole iteration through all standard algos and didn't make progress, time to pull out the big guns

            out.emit(StartAlgoBlock("Last Ditch"))
            waitBetweenCommands(params)

            lastDitchAlgos.foreach(algo => {
              runAlgo(algo)
            })

            done = true
            val solved = BoardState.doesSolve(board, curState).isSolved

            val end = new Date().getTime
            if (!solved) {
              out.emit(UnableToFindSolution())
            }
            else {
              out.emit(FoundSolution())
            }
          }
          else {
            val end = new Date().getTime
            out.emit(UnableToFindSolution())
          }
        }
        else {
          out.emit(StartingNewIterationSoon())
          waitBetweenCommands(params)
          out.emit(StartNewIteration())
          waitBetweenCommands(params)
        }
      }
    }
  }

  def solveWithUINonBlocking(board: Board, clues: Clues, lastSolveState: SolveState): SolveRet = {
    val start = new Date().getTime
    println(lastSolveState)

    lastSolveState match {
      case v: NotStarted =>
        val curState = BoardState.initialise(board)
        val messages = Vector(NewBoardState(curState), StartAlgoBlock("Initial"))
        SolveRet(messages, BeforeStartingAlgo(AlgoBlockState(curState, curState, initialAlgos, 0)), new Date().getTime - start)

      case v: BeforeStartingAlgo =>
        val algo = v.in.algoBlock(v.in.algoIdx)
        SolveRet(Vector(StartAlgo(algo.getClass.getSimpleName)), StartingAlgo(v.in), new Date().getTime - start)

      case v: StartingAlgo =>
        val algo = v.in.algoBlock(v.in.algoIdx)

        val newState = algo.solve(ForSolver(board, clues, v.in.boardState)).last

        val anyMatches = v.in.boardState != newState

        val messages: Seq[SolverCommand] =
          Vector(CompleteAlgo(algo.getClass.getSimpleName, anyMatches)) ++
            (if (anyMatches) Vector(NewBoardState(newState)) else Vector())

        val nextState: SolveState = {
          if (v.in.algoIdx >= (v.in.algoBlock.length - 1)) {
            CheckSolvedAndPickNextAlgoBlock(AlgoBlockState(newState, v.in.stateBeforeAlgoBlock, v.in.algoBlock, v.in.algoIdx))
          }
          else {
            BeforeStartingAlgo(AlgoBlockState(newState, v.in.stateBeforeAlgoBlock, v.in.algoBlock, v.in.algoIdx + 1))
          }
        }

        SolveRet(messages, nextState, new Date().getTime - start)

      case v: CheckSolvedAndPickNextAlgoBlock =>
        val solved = BoardState.doesSolve(board, v.in.boardState).isSolved

        if (solved) {
          SolveRet(Vector(FoundSolution()), Terminated(), new Date().getTime - start, terminate = true)
        }
        else if (v.in.algoBlock == initialAlgos) {
          SolveRet(Vector(StartAlgoBlock("Iterating")),
            BeforeStartingAlgo(AlgoBlockState(v.in.boardState, v.in.boardState, iterateAlgos, 0)),
            new Date().getTime - start)
        }
        else {
          assert(v.in.algoBlock == iterateAlgos)
          if (v.in.boardState != v.in.stateBeforeAlgoBlock) {
            SolveRet(Vector(StartAlgoBlock("Iterating")),
              BeforeStartingAlgo(AlgoBlockState(v.in.boardState, v.in.boardState, iterateAlgos, 0)),
              new Date().getTime - start)
          }
          else {
            SolveRet(Vector(UnableToFindSolution()), Terminated(), new Date().getTime - start, terminate = true)
          }
        }


      case v: Terminated =>
        assert(false) // Stop calling me
        SolveRet(Vector(), Terminated(), 0, terminate = true)
    }
  }
}


trait SolveState

case class NotStarted() extends SolveState

case class AlgoBlockState(boardState: BoardState, stateBeforeAlgoBlock: BoardState, algoBlock: Seq[Algorithm], algoIdx: Int)

case class BeforeStartingAlgo(in: AlgoBlockState) extends SolveState

case class StartingAlgo(in: AlgoBlockState) extends SolveState

case class CheckSolvedAndPickNextAlgoBlock(in: AlgoBlockState) extends SolveState

case class Terminated() extends SolveState

case class SolveRet(messages: Seq[SolverCommand],
                    solveState: SolveState,
                    timeTakenMsecs: Long,
                    terminate: Boolean = false)