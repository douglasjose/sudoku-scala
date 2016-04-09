package com.douglasjose.scala

import scala.collection.mutable

/**
  * @author Douglas José (@douglasjose)
  */
object Sudoku {

  def init(positions: Seq[(Int, Int, Int)]): Board = {
    val board = new Board
    positions.foreach { case (i, j, k) =>
      board(i, j, k)
    }
    board
  }

}

object Board {
  val size = 9
}

class Board {

  import Board._

  // Board of known values; the value 0 means the answer is unknown for that position
  private val state: Array[Array[Int]] = Array.fill(size, size) {
    0
  }

  // Sets a value in a specific board position
  def apply(i: Int, j: Int, k: Int): Unit = {
    state(i)(j) = k
  }

  // Returns the value from a position
  def apply(i: Int, j: Int): Int = {
    state(i)(j)
  }


  /**
    * Sequence of tuples of known values in the board
    *
    * @return Sequence of row/column/value for known values
    */
  def values(): Seq[(Int, Int, Int)] = {
    val ret = mutable.MutableList[(Int, Int, Int)]()

    for (i <- 0 until size; j <- 0 until size) {
      if (apply(i, j) != 0) {
        ret += ((i, j, apply(i, j)))
      }
    }

    ret
  }

  /**
    * @return If there are no undetermined values in the board
    */
  def gameSolved(): Boolean = !state.exists(_.contains(0))

  /**
    * Prints the state of the game board
    */
  def display(): Unit = {
    state.foreach(row => {
      row.foreach {
        print
      }
      println()
    })
  }

}


object Solver {

  import Board._

  type Values = Array[Array[mutable.BitSet]]

  /**
    * Returns a 'cube' of all possible values a board position can have
    *
    * @return Matrix of BitSet where the set has all values a position can have
    */
  def allValues(): Values = Array.fill(size, size) {
    val bs = new mutable.BitSet(size)
    for (i <- 1 to size) {
      bs.+=(i)
    }
    bs
  }


  /**
    * Calculates the possible values for all positions in the board, based on the already know solutions
    *
    * @param board Current game board
    * @return Values 'cube' containing only the possible values given the board configuration
    */
  private def iterate(board: Board): Values = {
    val eligibleValues = allValues()

    board.values().foreach { case (i, j, k) =>
      eligibleValues(i)(j).clear()
      invalidateInRow(eligibleValues, i, k)
      invalidateInColumn(eligibleValues, j, k)
      invalidateInSector(eligibleValues, i, j, k)
    }

    eligibleValues

  }

  /**
    * Returns a sequence of tuples where only one value is possible in a position, making it a solution
    *
    * @param board Current game configuration
    * @return Sequence of solutions found
    */
  private def findSolutions(board: Board): Seq[(Int, Int, Int)] = {
    val ret = mutable.MutableList[(Int, Int, Int)]()
    val values = iterate(board)

    for (i <- 0 until size; j <- 0 until size) {
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
      solutions.foreach { case (i, j, v) =>
        println(s"Iteration $iteration: found solution ($i,$j) = $v")
        board(i, j, v)
      }
    }
    board.display()
    println(s"Game solved in $iteration iterations.")
  }

  // Marks the given value as an invalid solution in the whole row
  private def invalidateInRow(values: Values, rowIndex: Int, value: Int): Values = {
    for (i <- 0 until size) {
      values(rowIndex)(i) -= value
    }
    values
  }

  // Marks the given value as an invalid solution in the whole column
  private def invalidateInColumn(values: Values, columnIndex: Int, value: Int): Values = {
    for (i <- 0 until size) {
      values(i)(columnIndex) -= value
    }
    values
  }

  // Marks the given value as an invalid solution in the whole sector
  private def invalidateInSector(values: Values, row: Int, column: Int, value: Int): Values = {
    for (i <- sectorIndexes(row); j <- sectorIndexes(column)) {
      values(i)(j) -= value
    }
    values
  }


  def sectorIndexes(i: Int): List[Int] = {
    val sectorSize = Math.sqrt(size).toInt
    val range = i / sectorSize
    (range * sectorSize until (range + 1) * sectorSize).toList
  }

  /**
    * Verifies the game's invariants
    *
    * @param board Game to be checked
    */
  def verify(board: Board): Unit = {

    val referenceList = 1 to size

    def checkRow(row: Int): Unit = {
      val rowValues = for (i <- 0 until size) yield board(row, i)
      if (!referenceList.forall(rowValues.contains)) {
        throw new IllegalStateException(s"Invalid row $row: $rowValues")
      }
    }

    def checkColumn(column: Int): Unit = {
      val columnValues = for (i <- 0 until size) yield board(i, column)
      if (!referenceList.forall(columnValues.contains)) {
        throw new IllegalStateException(s"Invalid column $column: $columnValues")
      }
    }

    def checkSector(square: Int): Unit = {
      val numSectors = Math.sqrt(size).toInt
      for (i <- 0 until numSectors; j <- 0 until numSectors) {
        val squareValues = for (r <- sectorIndexes(i * numSectors); c <- sectorIndexes(j * numSectors)) yield board(r, c)
        if (!referenceList.forall(squareValues.contains)) {
          throw new IllegalStateException(s"Invalid square $square: $squareValues")
        }
      }

    }


    if (!board.gameSolved()) {
      throw new IllegalStateException("The game is not yet completely solved")
    }

    for (i <- 0 until size) {
      checkRow(i)
      checkColumn(i)
      checkSector(i)
    }
  }

}
