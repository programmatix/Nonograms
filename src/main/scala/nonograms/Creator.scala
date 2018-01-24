package nonograms

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class CreatorParams(rows: Int, cols: Int)
case class CreatorResult(board: Board, iterations: Int)

// Makes up puzzles
object Creator {
  def createRandom(params: CreatorParams): Board = {
    val board = ArrayBuffer.fill(params.rows, params.cols)(false)

    for (row <- Range(0, params.rows)) {
      for (col <- Range(0, params.cols)) {
        val isFilled = Random.nextBoolean()
        board(row)(col) = isFilled
      }
    }

    val mapped = Vector2Dim[Boolean](Vector2Dim.arrayBufferToVector(board))
    Board(mapped)
  }

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
