package nonograms

import org.scalatest.funsuite.AnyFunSuite

class FullBoardSpec extends AnyFunSuite {
  test("1") {
    val raw = """-XX---X---
                |X-X-XX-XXX
                |X-X--XX---
                |-XX---XX-X
                |XXXX-XXXX-
                |-XX-X-XX-X
                |X-----X---
                |XXXXX-X--X
                |-XXXX-X-XX
                |XX--XX---X
                |""".stripMargin
    val board = Board.create(raw)
    val solved = Solver.solve(board)
  }

  test("2") {
    val raw = """XXXXXXXXXX
                |X-XX--X-XX
                |XX-XXX---X
                |X-XXXXXXXX
                |X-XX-XXXXX
                |XXX----XXX
                |XXX-XXXXXX
                |X-XXXX-XX-
                |XXXXXXXXXX
                |XXXXXX-X-X""".stripMargin
    val board = Board.create(raw)
    val clues = Clues.generateClues(board)
    assert (clues.getCol(2).clues == Vector(2,7))
  }

}
