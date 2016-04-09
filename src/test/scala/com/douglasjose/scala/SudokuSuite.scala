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


  test("Solver - easy") {
    val game = Sudoku.init(
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
    )

    Solver.solve(game)

    Solver.verify(game)
  }



}
