package com.douglasjose.scala

import scala.collection.mutable

/**
 * @author Douglas José (@douglasjose)
 */
object Sudoku {

  def init(positions: Seq[(Int, Int, Int)]): Board = {
    val board = new Board
    positions.foreach{ case (i, j, k) => {
      board(i,j,k)
    }}
    board
  }

}

object Board {
  val size = 9
}

class Board {

  import Board._

  // Board of known values; the value 0 means the answer is not known for that position
  private val state: Array[Array[Int]] = Array.fill(size, size){ 0 }

  // Sets a value in a specific board position
  def apply(i: Int, j: Int, k:Int): Unit = {
    state(i)(j) = k
  }

  // Returns the value from a position
  def apply(i: Int, j: Int): Int = {
    state(i)(j)
  }


  /**
   * Sequence of tuples of known values in the board
   * @return Sequence of row/column/value for known values
   */
  def values(): Seq[(Int, Int, Int)] = {
    val ret = mutable.MutableList[(Int, Int, Int)]()

    for (i <- 0 to size - 1; j <- 0 to size -1) {
      if (apply(i, j) != 0) {
        ret += ((i, j, apply(i, j)))
      }
    }

    ret.toSeq
  }

  /**
   * @return If there are no undetermined values in the board
   */
  def gameSolved(): Boolean = !state.exists(row => row.contains(0))

  /**
   * Prints the state of the game board
   */
  def display(): Unit = {
    state.foreach(row => {
      row.foreach(v => print(v))
      println()
    })
  }

}



object Solver {

  import Board._

  type Values = Array[Array[mutable.BitSet]]

  /**
   * Returns a 'cube' of all possible values a board position can have
   * @return Matrix of BitSet where the set has all values a position can have
   */
  def allValues(): Values = Array.fill(size, size){
    val bs = new mutable.BitSet(size)
    for (i <- 1 to size) {
      bs.+=(i)
    }
    bs
  }


  /**
   * Calculates the possible values for all positions in the board, based on the already know solutions
   * @param board Current game board
   * @return Values 'cube' containing only the possible values given the board configuration
   */
  private def iterate(board: Board): Values = {
    val eligibleValues = allValues()

    board.values().foreach{ case (i, j, k) => {
      eligibleValues(i)(j).clear()
      invalidateInRow(eligibleValues, i, k)
      invalidateInColumn(eligibleValues, j, k)
      invalidateInSquare(eligibleValues, i, j, k)
    }}

    eligibleValues

  }

  /**
   * Returns a sequence of tuples where only one value is possible in a position, making it a solution
   * @param board Current game configuration
   * @return Sequence of solutions found
   */
  private def findSolutions(board: Board): Seq[(Int, Int, Int)] = {
    val ret = mutable.MutableList[(Int, Int, Int)]()
    val values = iterate(board)

    for (i <- 0 to size - 1; j <- 0 to size - 1) {
      val solutions = values(i)(j)
      if (solutions.size == 1) {
        ret += ((i, j, values(i)(j).head))
      }
    }
    ret
  }

  /**
   * Solves the Sudoku game.
   *
   * @param board Game to be solved
   */
  def solve(board: Board): Unit = {
    var iteration = 0
    while (!board.gameSolved()) {
      iteration = iteration + 1
      val solutions = findSolutions(board)
      if (solutions.isEmpty) {
        board.display()
        throw new IllegalStateException(s"No more solutions could be found after $iteration iterations")
      }
      solutions.foreach{ case (i,j,v) =>
        println(s"Iteration $iteration: found solution ($i,$j) = $v")
        board(i,j,v)
      }
    }
    board.display()
    println(s"Game solved in $iteration iterations.")
  }

  // Marks the given value as an invalid solution in the whole row
  private def invalidateInRow(values: Values, rowIndex: Int, value: Int): Values = {
    for (i <- 0 to size - 1) {
      values(rowIndex)(i) -= value
    }
    values
  }

  // Marks the given value as an invalid solution in the whole column
  private def invalidateInColumn(values: Values, columnIndex: Int, value: Int): Values = {
    for (i <- 0 to size - 1) {
      values(i)(columnIndex) -= value
    }
    values
  }

  // Marks the given value as an invalid solution in the whole square
  private def invalidateInSquare(values: Values, row: Int, column: Int, value: Int): Values = {
    for (i <- squareIndexes(row) ; j <- squareIndexes(column)) {
      values(i)(j) -= value
    }
    values
  }


  def squareIndexes(i: Int): List[Int] = {
    val squareSize = Math.sqrt(size).toInt
    val range = i / squareSize
    (range * squareSize to ((range + 1) * squareSize) - 1).toList
  }

  /**
   * Verifies the game's invariant
   * @param board Game to be checked
   */
  def verify(board: Board): Unit = {

    val referenceList = 1 to size

    def checkRow(row: Int): Unit = {
      val rowValues = for (i <- 0 to size - 1) yield board(row, i)
      if (!referenceList.forall(rowValues.contains)) {
        throw new IllegalStateException(s"Invalid row $row: $rowValues")
      }
    }

    def checkColumn(column: Int): Unit = {
      val columnValues = for (i <- 0 to size - 1) yield board(i, column)
      if (!referenceList.forall(columnValues.contains)) {
        throw new IllegalStateException(s"Invalid column $column: $columnValues")
      }
    }

    def checkSquare(square: Int): Unit = {
      val numSquares = Math.sqrt(size).toInt
      for (i <- 0 to numSquares - 1 ; j <- 0 to numSquares - 1) {
        val squareValues= for (r <- squareIndexes(i * numSquares) ; c <- squareIndexes(j * numSquares)) yield board(r,c)
        if (!referenceList.forall(squareValues.contains)) {
          throw new IllegalStateException(s"Invalid square $square: $squareValues")
        }
      }

    }


    if (!board.gameSolved()) {
      throw new IllegalStateException("The game is not yet completely solved")
    }

    for (i <- 0 to size -1) {
      checkRow(i)
      checkColumn(i)
      checkSquare(i)
    }
  }

}

object Game {

  def main(args: Array[String]): Unit = {

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