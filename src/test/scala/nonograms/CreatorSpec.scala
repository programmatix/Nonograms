package nonograms

import org.scalatest.FunSuite

class CreatorSpec extends FunSuite {
  test("random 5x5") {
    val board = Creator.createRandomSolvable(CreatorParams(5,5))
    assert (board.iterations < 10000)
    println("5x5: " + board.iterations)
  }

  test("random 8x8") {
    val board = Creator.createRandomSolvable(CreatorParams(8,8))
    assert (board.iterations < 10000)
    println("8x8: " + board.iterations)
  }

  test("random 10x10") {
    val board = Creator.createRandomSolvable(CreatorParams(10,10))
    assert (board.iterations < 100000)
    println("10x10: " + board.iterations)
  }

  // Does a good job of finding edge case bugs
  test("lotsa randoms 5x5") {
    for(i <- Range(0,500)) {
      val board = Creator.createRandom(CreatorParams(5, 5))
      Solver.solve(board)
    }
  }

  // Does a good job of finding edge case bugs
  test("lotsa randoms 8x8") {
    for(i <- Range(0,500)) {
      val board = Creator.createRandom(CreatorParams(8, 8))
      Solver.solve(board)
    }
  }
}
