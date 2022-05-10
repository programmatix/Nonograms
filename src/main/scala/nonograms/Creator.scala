package nonograms

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// 'probOfFilled' is 0-1 percentage chance of a square being filled
case class CreatorParams(rows: Int, cols: Int, probOfFilled: Float = 0.7f)
case class CreatorResult(board: Board, iterations: Int)

// Makes up puzzles
object Creator {
  // Doesn't guarantee solvability
  def createRandom(params: CreatorParams): Board = {
    val board = ArrayBuffer.fill(params.rows, params.cols)(false)
    val threshold = params.probOfFilled * 1000

    for (row <- Range(0, params.rows)) {
      for (col <- Range(0, params.cols)) {
        val isFilled = Random.nextInt(1000) <= threshold
          board(row)(col) = isFilled
      }
    }

    val mapped = Vector2Dim[Boolean](Vector2Dim.arrayBufferToVector(board))
    Board(mapped)
  }

  // Does guarantee solvability
  def createRandomSolvable(params: CreatorParams): CreatorResult = {
    var out: Option[Board] = Option.empty
    var iterations = 0

    while (out.isEmpty) {
      val board = createRandom(params)

      val solvable = Solver.solve(board)
      if (solvable) {
        out = Some(board)
      }
      else {
        iterations += 1
      }
    }

    CreatorResult(out.get, iterations)
  }
}
