package nonograms

import nonograms.algos.{AlgoBruteForce, Algorithm, SolverParams}

import scala.collection.mutable.ArrayBuffer

object TestUtils {
  def createBasicBoard() = {
    val input =
      """XOOO
        |OOOX
        |OOOO
      """.stripMargin
    Board.create(input)
  }

  def testBruteForceDelete(input: String, row: Int, col: Int): Boolean = {
    val (board, state) = TestUtils.create(input)
    val clues = Clues.generateClues(board)
    val newState = Algorithm.delete(row, col, board, state, allowBadMoves = true)
    AlgoBruteForce.isValid(newState, clues, stopOnFail = true)
  }

  // Disable pruning for testing as it complicates that
  def testAlgo(algo: Algorithm, input: String, output: String, includeReverse: Boolean = false, params: SolverParams = SolverParams(allowPruning = false)): Unit = {
    val (board, state) = TestUtils.create(input)
    val result = Solver.applyAlgoWithState(algo, board, state, params)
    assert(result.last == TestUtils.create(output)._2, s"${result.last} != ${output.replace(" ","")}")
    if (includeReverse) {
      // Note that AlgoMinis will do this automatically
      testAlgo(algo, input.reverse, output.reverse)
    }
  }

  def testAlgoUntouched(algo: Algorithm, input: String): Unit = {
    val (board, state) = TestUtils.create(input)
    val result = Solver.applyAlgoWithState(algo, board, state)
    assert(result.last == state, s"${result.last} != $state")
  }

  // Handles 'D' and 'M' syntax in addition to standard board 'O' and 'X'
  // Also '-' is 'O'
  def create(inputRaw: String): (Board, BoardState) = {
    val inputProcessed = inputRaw.trim().replace(" ", "")
    val input = inputProcessed.replace("\r\n", "\n")
    val forBoard = inputProcessed
      .replace('D','O')
      .replace('M','X')
      .replace('-', 'O')
    val board = Board.create(forBoard)

    val rows = inputProcessed.count((c) => c == '\n') + 1
    val cols = if (inputProcessed.contains('\n')) input.indexOf('\n') else inputProcessed.length
    //    val board = Vector2Dim.create[Boolean](rows, cols, false)

    val boardState = ArrayBuffer.fill[SquareState](rows, cols)(SquareStateUntouched())

    val ch = inputProcessed.iterator
    var row = 0
    var col = 0
    while (ch.hasNext) {
      val c = ch.next()
      if (c == '\n') {
        row += 1
        col = 0
      }
      else {
        c match {
          case 'M' => boardState(row)(col) = SquareStateMarked()
          case 'D' => boardState(row)(col) = SquareStateDeleted()
          case _ =>
        }
        col += 1
      }
    }

    val mapped = Vector2Dim[SquareState](Vector2Dim.arrayBufferToVector(boardState))
    val bs = BoardState(mapped)

    (board, bs)
  }

  def mark(board: Board, marks: Vector[(Int,Int)], deletes: Vector[(Int,Int)] = Vector.empty): BoardState = {
    var state = BoardState.initialise(board)

    marks.foreach(mark => {
      val row = mark._1
      val col = mark._2
      val newBoardState = Algorithm.mark(row, col, board, state)
      state = newBoardState
    })

    deletes.foreach(delete => {
      val row = delete._1
      val col = delete._2
      val newBoardState = Algorithm.delete(row, col, board, state)
      state = newBoardState
    })

    state
  }

  def mrkd(boardState: BoardState, row: Int, col: Int): Boolean = {
    boardState.get(row, col) == SquareStateMarked()
  }

  def untd(boardState: BoardState, row: Int, col: Int): Boolean = {
    boardState.get(row, col) == SquareStateUntouched()
  }

  def deld(boardState: BoardState, row: Int, col: Int): Boolean = {
    boardState.get(row, col) == SquareStateDeleted()
  }
}
