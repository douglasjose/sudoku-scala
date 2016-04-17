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
  val boardSize = 9

  val sectorSize = Math.sqrt(boardSize).toInt

  /**
    * List of coordinates of the first (top left) position in each sector of the board
    * E.g., if boardSize = 9, then sectorHeads = List((0,0), (0,3), (0,6), (3,0), (3,3), (3,6), (6,0), (6,3), (6,6))
    */
  val sectorHeads: List[(Int, Int)] = {
    val heads = for (
      i <- 0 until sectorSize;
      j <- 0 until sectorSize
    ) yield (i * sectorSize, j * sectorSize)
    heads.toList
  }

}

class Board {

  import Board._

  // Board of known values; the value 0 means the answer is unknown for that position
  private val state: Array[Array[Int]] = Array.fill(boardSize, boardSize) {
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

    for (i <- 0 until boardSize; j <- 0 until boardSize) {
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
  def display() = println(toString)

  override def toString: String = {
    state.map(row => {
      row.mkString.grouped(sectorSize).mkString("|")
    }).grouped(sectorSize).map(_.mkString("\n")).mkString(
      "\n" + List.fill(sectorSize){"-" * sectorSize}.mkString("+") +  "\n")
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
  def allValues(): Values = Array.fill(boardSize, boardSize) {
    val bs = new mutable.BitSet(boardSize)
    for (i <- 1 to boardSize) {
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

    for (i <- 0 until boardSize; j <- 0 until boardSize) {
      val solutions = values(i)(j)
      if (solutions.size == 1) {
        ret += ((i, j, values(i)(j).head))
      }
    }
    ret
  }

  /**
    * Look for solutions where a value can be accommodated in a single position in their corresponding line/row/sector
    *
    * @param board Sudoku board
    * @return Sequence of solutions found using this strategy
    */
  private def findAdvancedSolutions(board: Board): Seq[(Int, Int, Int)] = {
    val eligibleValues = iterate(board)

    val eligibleRows: List[(Int, Array[mutable.BitSet])] =
      (0 until boardSize).map(r => (r, eligibleValues(r))).toList

    val solutionsByRow = eligibleRows.flatMap(e => {
      uniqueValueInLine(e._2).map(u => (e._1, u._1, u._2))
    })

    val eligibleColumns: List[(Int, Array[mutable.BitSet])] =
      (0 until boardSize).map(c => (c, eligibleColumn(c, eligibleValues))).toList

    val solutionsByColumn = eligibleColumns.flatMap(e => {
      uniqueValueInLine(e._2).map(u => (u._1, e._1, u._2))
    })

    // List of pairs ('sector head', 'eligible values in sector')
    val eligibleSectors: List[((Int, Int), Values)] =
      sectorHeads.map(h => (h, eligibleValues.map(_.slice(h._2, h._2 + sectorSize)).slice(h._1, h._1 + sectorSize)))

    val solutionsBySector = eligibleSectors.flatMap(e => {
      uniqueValueInSector(e._2).map(u => (e._1._1 + u._1._1, e._1._2 + u._1._2, u._2))
    })

    solutionsBySector ++ solutionsByRow ++ solutionsByColumn
  }

  def eligibleColumn(col: Int, eligibleBoard: Values): Array[mutable.BitSet] = {
    eligibleBoard.map(r => r(col))
  }

  /**
    * Searches a sector for values that have only one possible position in it.
    *
    * @param sector The sector of the board to search for unique values
    * @return List of positions in the sector and the unique value associated with them
    */
  private def uniqueValueInSector(sector: Values): List[((Int, Int), Int)] = {

    val sectorMap = sectorAsMap(sector)

    // A value is unique in a sector if there is no other position in the sector that could accept it
    def isUniqueInSector(positionToValue: ((Int, Int), Int)): Boolean = {
      sectorMap.forall(o => positionToValue._1 == o._1 || o._2.isEmpty || !o._2.contains(positionToValue._2))
    }

    // Creating a list of ('position', 'value') for all values unique in the sector
    sectorMap.flatMap(sec => {
      sec._2.map((sec._1, _)).filter(isUniqueInSector(_))
    }).toList
  }

  /**
    * Searches a row or column for a values that have one possible position in it
    * @param line The row or column to search for unique values
    * @return List of positions in the row or column and the unique values associated with them
    */
  private def uniqueValueInLine(line: Array[mutable.BitSet]): List[(Int,Int)] = {

    def isUniqueInLine(positionAndValue: (Int, Int), line: Array[mutable.BitSet]) =
      line.indices.forall(o => o == positionAndValue._1 || !line(o).contains(positionAndValue._2))

    line.indices.flatMap(i => {
      line(i).map(v=> (i, v)).filter(p => isUniqueInLine(p, line))
    }).toList

  }

  /**
    * Converts the bi-dimensional array of bitsets in a map of bitsets indexed by (row, column)
    *
    * @param sector Matrix of bitsets
    * @return Map of bitsets
    */
  private def sectorAsMap(sector: Values): mutable.Map[(Int, Int), mutable.BitSet] = {
    val result = mutable.Map[(Int, Int), mutable.BitSet]()

    sector.indices.foreach(i =>
      sector(i).indices.foreach(j =>
        result += ((i, j) -> sector(i)(j))
      )
    )

    result
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
      val advancedSolutions = solutions match {
        case Nil =>
          println(s"Iteration $iteration: Searching for advanced solutions")
          findAdvancedSolutions(board)
        case _ => Nil
      }
      val allSolutions = solutions ++ advancedSolutions
      if (allSolutions.isEmpty) {
        board.display()
        throw new IllegalStateException(s"No more solutions could be found after $iteration iterations")
      }
      allSolutions.foreach { case (i, j, v) =>
        println(s"Iteration $iteration: found solution ($i,$j) = $v")
        board(i, j, v)
      }
    }
    board.display()
    println(s"Game solved in $iteration iterations.")
  }

  // Marks the given value as an invalid solution in the whole row
  private def invalidateInRow(values: Values, rowIndex: Int, value: Int): Values = {
    for (i <- 0 until boardSize) {
      values(rowIndex)(i) -= value
    }
    values
  }

  // Marks the given value as an invalid solution in the whole column
  private def invalidateInColumn(values: Values, columnIndex: Int, value: Int): Values = {
    for (i <- 0 until boardSize) {
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
    val range = i / sectorSize
    (range * sectorSize until (range + 1) * sectorSize).toList
  }

  /**
    * Verifies the game's invariants
    *
    * @param board Game to be checked
    */
  def verify(board: Board): Unit = {

    val referenceList = 1 to boardSize

    def checkRow(row: Int): Unit = {
      val rowValues = for (i <- 0 until boardSize) yield board(row, i)
      if (!referenceList.forall(rowValues.contains)) {
        throw new IllegalStateException(s"Invalid row $row: $rowValues")
      }
    }

    def checkColumn(column: Int): Unit = {
      val columnValues = for (i <- 0 until boardSize) yield board(i, column)
      if (!referenceList.forall(columnValues.contains)) {
        throw new IllegalStateException(s"Invalid column $column: $columnValues")
      }
    }

    def checkSector(square: Int): Unit = {
      val numSectors = Math.sqrt(boardSize).toInt
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

    for (i <- 0 until boardSize) {
      checkRow(i)
      checkColumn(i)
      checkSector(i)
    }
  }

}
