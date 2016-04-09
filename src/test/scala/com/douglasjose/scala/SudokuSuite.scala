package com.douglasjose.scala

import org.scalatest.FunSuite

/**
  * Created by douglas on 9/4/16.
  */
class SudokuSuite extends FunSuite {

  test("Utility functions") {

    assert(Solver.sectorIndexes(0) == List(0, 1, 2))
    assert(Solver.sectorIndexes(1) == List(0, 1, 2))
    assert(Solver.sectorIndexes(2) == List(0, 1, 2))
    assert(Solver.sectorIndexes(3) == List(3, 4, 5))
    assert(Solver.sectorIndexes(4) == List(3, 4, 5))
    assert(Solver.sectorIndexes(5) == List(3, 4, 5))
    assert(Solver.sectorIndexes(6) == List(6, 7, 8))
    assert(Solver.sectorIndexes(7) == List(6, 7, 8))
    assert(Solver.sectorIndexes(8) == List(6, 7, 8))

  }

  def solveAndVerify(board: Board): Unit = {
    Solver.solve(board)
    Solver.verify(board)
  }


  test("Solver - easy") {
    solveAndVerify(Sudoku.init(
      List(
        (0, 4, 5),
        (0, 5, 1),
        (0, 6, 3),
        (1, 1, 1),
        (1, 3, 9),
        (1, 6, 5),
        (1, 7, 8),
        (2, 2, 9),
        (2, 4, 4),
        (2, 6, 2),
        (2, 7, 1),
        (3, 0, 5),
        (3, 4, 8),
        (3, 5, 9),
        (3, 6, 1),
        (4, 0, 7),
        (4, 1, 3),
        (4, 7, 4),
        (4, 8, 5),
        (5, 2, 1),
        (5, 3, 4),
        (5, 4, 7),
        (5, 8, 3),
        (6, 1, 2),
        (6, 2, 6),
        (6, 4, 3),
        (6, 6, 4),
        (7, 1, 9),
        (7, 2, 5),
        (7, 5, 6),
        (7, 7, 3),
        (8, 2, 3),
        (8, 3, 1),
        (8, 4, 9)
      )
    ))
  }

  test("Solver - medium #01") {
    solveAndVerify(Sudoku.init(
      List(
        (0,0,8),
        (0,2,6),
        (0,8,3),
        (1,3,8),
        (2,3,7),
        (2,4,1),
        (2,5,5),
        (2,8,8),
        (3,2,2),
        (3,6,9),
        (3,7,8),
        (4,2,3),
        (4,6,6),
        (5,1,5),
        (5,2,8),
        (5,6,7),
        (6,0,2),
        (6,3,9),
        (6,4,3),
        (6,5,1),
        (7,5,6),
        (8,0,7),
        (8,6,4),
        (8,8,6)
      )
    ))
  }

  test("Solver - medium #02") {
    solveAndVerify(Sudoku.init(
      List(
        (0,0,1),
        (0,1,9),
        (0,5,7),
        (0,8,6),
        (1,5,3),
        (1,6,8),
        (1,8,4),
        (2,1,7),
        (3,0,4),
        (3,1,6),
        (3,4,2),
        (4,3,5),
        (4,5,4),
        (5,4,7),
        (5,7,3),
        (5,8,8),
        (6,7,8),
        (7,0,5),
        (7,2,1),
        (7,3,8),
        (8,0,3),
        (8,3,6),
        (8,7,5),
        (8,8,1)
      )
    ))
  }

}
