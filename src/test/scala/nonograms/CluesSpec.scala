package nonograms

import org.scalatest.FunSuite

class CluesSpec extends FunSuite {
  test("generate clues") {
    val board = TestUtils.createBasicBoard()
    val clues = Clues.generateClues(board)

    assert (clues.rows.size == board.numRows())
    assert (clues.cols.size == board.numCols())

    assert (clues.rows(0).clues == Vector(1))
    assert (clues.rows(1).clues == Vector(1))
    assert (clues.rows(2).clues == Vector(0))

    assert (clues.cols(0).clues == Vector(1))
    assert (clues.cols(1).clues == Vector(0))
    assert (clues.cols(2).clues == Vector(0))
    assert (clues.cols(3).clues == Vector(1))
  }

}
