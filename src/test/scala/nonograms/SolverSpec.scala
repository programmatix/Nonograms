package nonograms

import org.scalatest.funsuite.AnyFunSuite

class SolverSpec extends AnyFunSuite {
  test("1") {
    val input = """X-X--
                  |-XX--
                  |-X--X
                  |--XX-
                  |XXX--
                  |""".stripMargin
    val board = Board.create(input)
    val solved = Solver.solve(board)
    assert(!solved)
  }

  test("2") {
    val input = """XXX-X
                  |--XX-
                  |XX--X
                  |XX-X-
                  |X-XX-
                  |""".stripMargin
    val board = Board.create(input)
    val solved = Solver.solve(board)

    // Is solvable one day
    assert(!solved)
  }

  test("3") {
    val input = """X--XX
                  |-XXXX
                  |---X-
                  |XX-X-
                  |X-XX-
                  |""".stripMargin
    val board = Board.create(input)
    val solved = Solver.solve(board)

    // Is solvable one day
    assert(!solved)
  }
}
