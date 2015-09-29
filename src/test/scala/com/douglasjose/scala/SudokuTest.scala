package com.douglasjose.scala

/**
 * @author Douglas José (@douglasjose)
 */
object SudokuTest {

  def main(args: Array[String]): Unit = {

    for (i <- 0 to 8) {
      println(i + " = " + Solver.squareIndexes(i))
    }

    assert(Solver.squareIndexes(0) == List(0, 1, 2))
    assert(Solver.squareIndexes(1) == List(0, 1, 2))
    assert(Solver.squareIndexes(2) == List(0, 1, 2))
    assert(Solver.squareIndexes(3) == List(3, 4, 5))
    assert(Solver.squareIndexes(4) == List(3, 4, 5))
    assert(Solver.squareIndexes(5) == List(3, 4, 5))
    assert(Solver.squareIndexes(6) == List(6, 7, 8))
    assert(Solver.squareIndexes(7) == List(6, 7, 8))
    assert(Solver.squareIndexes(8) == List(6, 7, 8))
  }

}
