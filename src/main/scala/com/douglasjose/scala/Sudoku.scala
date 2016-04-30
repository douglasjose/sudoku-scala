package com.douglasjose.scala

import scala.collection.mutable

/**
  * <a href="https://en.wikipedia.org/wiki/Sudoku">Sudoku</a> solver
  *
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

  type Values = Array[Array[mutable.BitSet]]

  val boardSize = 9

  val sectorSize = Math.sqrt(boardSize).toInt

  assert(sectorSize * sectorSize == boardSize)

  /**
    * List of coordinates of the first (top left) position in each sector of the board
    * E.g., if boardSize = 9, then sectorHeads = List((0,0), (0,3), (0,6), (3,0), (3,3), (3,6), (6,0), (6,3), (6,6))
    */
  val sectorHeads: List[(Int, Int)] = {
    (for (i <- 0 until sectorSize; j <- 0 until sectorSize) yield (i * sectorSize, j * sectorSize)).toList
  }

}

class Board {

  import Board._

  // Board of known values; the value 0 means the answer is unknown for that position
  private val state: Array[Array[Int]] = Array.fill(boardSize, boardSize) {
    0
  }

  val eligibleValues: Values = Array.fill(boardSize, boardSize) {
    mutable.BitSet(1 to boardSize: _*)
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
  def knownValues(): Seq[(Int, Int, Int)] = {
    (for (i <- 0 until boardSize; j <- 0 until boardSize) yield (i, j, apply(i, j))).filter(_._3 != 0)
  }

  /**
    * @return If there are no undetermined values in the board
    */
  def gameSolved(): Boolean = !state.exists(_.contains(0))

  override def toString: String = {
    state.map(row => {
      row.mkString.grouped(sectorSize).mkString("|")
    }).grouped(sectorSize).map(_.mkString("\n")).mkString(
      "\n" + List.fill(sectorSize) {
        "-" * sectorSize
      }.mkString("+") + "\n")
  }
}


object Solver {

  import Board._
  
  /**
    * Generates all permutations given a list of possible values in each position, returning only permutations where
    * each number appears only once
    *
    * @param groups List of possible values for each position
    * @return List of unique permutations
    */
  private def listPermutations(groups: List[List[Int]]): List[List[Int]] = {

    def listPermutationsRec(prefix: List[Int], groups: List[List[Int]]): List[List[Int]] = {
      groups match {
        case Nil => List(prefix)
        case g :: gs => g.map(prefix ++ List(_)).flatMap(listPermutationsRec(_, gs))
      }
    }

    listPermutationsRec(Nil, groups).filter(l => l.distinct.size == l.size)

  }

  /**
    * Reduces the list of possible values in a position by comparing them with the values of a list of unique
    * permutations
    *
    * @param permutations List of unique permutations
    * @return List of possible values in each position constrained by the permutations provided
    */
  private def reducePermutations(permutations: List[List[Int]]): List[List[Int]] = {
    permutations match {
      case ps if ps.head.nonEmpty => ps.map(_.head).distinct :: reducePermutations(ps.map(_.tail))
      case _ => Nil
    }
  }

  /**
    * Invalidates eligible values if no permutations in a row/column/sector allows a number to occur in a particular
    * position
    *
    * @param board Sudoku board
    * @return List of eligible values after invalidation
    */
  def invalidateByPermutations(board: Board): Unit = {
    println("Invalidating eligibles by permutations")
    val eligibleValues = board.eligibleValues

    for (r <- 0 until boardSize) {
      val rowEligibles = eligibleValues(r).zipWithIndex.map(e => (e._2, e._1.toList)).filter(_._2.nonEmpty).toList

      val simplifiedRowEligibles = rowEligibles.map(_._1).zip(
        reducePermutations(listPermutations(rowEligibles.map(_._2))))

      if (rowEligibles != simplifiedRowEligibles) {
        simplifiedRowEligibles.foreach(e => {
          eligibleValues(r)(e._1) = mutable.BitSet(e._2: _*)
        })
      }
    }

    for (c <- 0 until boardSize) {
      val columnEligibles = eligibleColumn(c, eligibleValues).zipWithIndex.map(
        e => (e._2, e._1.toList)).filter(_._2.nonEmpty).toList

      val simplifiedColumnEligibles = columnEligibles.map(_._1).zip(
        reducePermutations(listPermutations(columnEligibles.map(_._2))))

      if (columnEligibles != simplifiedColumnEligibles) {
        simplifiedColumnEligibles.foreach(e => {
          eligibleValues(e._1)(c) = mutable.BitSet(e._2: _*)
        })
      }
    }

    val eligibleSectors: List[((Int, Int), Values)] =
      sectorHeads.map(h => (h, eligibleValues.map(_.slice(h._2, h._2 + sectorSize)).slice(h._1, h._1 + sectorSize)))

    eligibleSectors.foreach(s => {
      val sectorMap = sectorAsMap(s._2)

      val sectorEligibles = sectorMap.toList.map(e => (e._1, e._2.toList)).filter(_._2.nonEmpty)

      val simplifiedSectorEligibles = sectorEligibles.map(_._1).zip(
        reducePermutations(listPermutations(sectorEligibles.map(_._2))))

      if (sectorEligibles != simplifiedSectorEligibles) {
        simplifiedSectorEligibles.foreach(e => {
          val pos = (s._1._1 + e._1._1, s._1._2 + e._1._2)
          eligibleValues(pos._1)(pos._2) = mutable.BitSet(e._2: _*)
        })

      }
    })

  }

  /**
    * Returns a sequence of tuples where only one value is possible in a position, making it a solution
    *
    * @param board Current game configuration
    * @return Sequence of solutions found
    */
  private def findSolutions(board: Board): Seq[(Int, Int, Int)] = {
    val eligibleValues = board.eligibleValues

    board.knownValues().foreach { case (i, j, k) =>
      eligibleValues(i)(j).clear()
      invalidateInRow(eligibleValues, i, k)
      invalidateInColumn(eligibleValues, j, k)
      invalidateInSector(eligibleValues, i, j, k)
    }

    for (i <- 0 until boardSize; j <- 0 until boardSize; if eligibleValues(i)(j).size == 1)
      yield (i, j, eligibleValues(i)(j).head)
  }

  /**
    * Look for solutions where a value can be accommodated in a single position in their corresponding line/row/sector
    *
    * @param board Sudoku board
    * @return Sequence of solutions found using this strategy
    */
  private def findAdvancedSolutions(board: Board): Seq[(Int, Int, Int)] = {
    println("Searching for advanced solutions")
    val eligibleValues = board.eligibleValues

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
    *
    * @param line The row or column to search for unique values
    * @return List of positions in the row or column and the unique values associated with them
    */
  private def uniqueValueInLine(line: Array[mutable.BitSet]): List[(Int, Int)] = {

    def isUniqueInLine(positionAndValue: (Int, Int), line: Array[mutable.BitSet]) =
      line.indices.forall(o => o == positionAndValue._1 || !line(o).contains(positionAndValue._2))

    line.indices.flatMap(i => {
      line(i).map(v => (i, v)).filter(p => isUniqueInLine(p, line))
    }).toList

  }

  /**
    * Converts the bi-dimensional array of BitSets in a map of BitSets indexed by (row, column)
    *
    * @param sector Matrix of bitsets
    * @return Map of bitsets
    */
  private def sectorAsMap(sector: Values): Map[(Int, Int), mutable.BitSet] = {
    Map((for (i <- sector.indices; j <- sector(i).indices) yield (i, j) -> sector(i)(j)): _*)
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
      computeSolutions(board) match {
        case Nil =>
          println(board)
          printEligible(board)
          throw new IllegalStateException(s"No more solutions could be found after $iteration iterations")
        case s =>
          s.foreach { case (i, j, v) =>
            println(s"Iteration $iteration: found solution ($i,$j) = $v")
            board(i, j, v)
          }
      }
    }
    println(board)
    println(s"Game solved in $iteration iterations.")
  }

  /**
    * Applies a chain of solution identification strategies, using the most inexpensive ones first.
    *
    * @param board Sudoku boards
    * @return List of solutions found
    */
  def computeSolutions(board: Board): Seq[(Int, Int, Int)] = {
    findSolutions(board) match {
      case Nil =>
        findAdvancedSolutions(board) match {
          case Nil =>
            invalidateByPermutations(board)
            findSolutions(board) match {
              case Nil =>
                findAdvancedSolutions(board)
              case s => s
            }
          case s => s
        }
      case s => s
    }
  }

  private def printEligible(board: Board): Unit = {
    val eligibleValues = board.eligibleValues


    val eligibleRows: List[(Int, Array[mutable.BitSet])] =
      (0 until boardSize).map(r => (r, eligibleValues(r))).toList

    eligibleRows.foreach(r => {
      println(s"Row ${r._1}")
      r._2.zipWithIndex.foreach { e => println(s"[${e._2}] = ${e._1}") }
    })

    val eligibleColumns: List[(Int, Array[mutable.BitSet])] =
      (0 until boardSize).map(c => (c, eligibleColumn(c, eligibleValues))).toList

    eligibleColumns.foreach(c => {
      println(s"Column ${c._1}")
      c._2.zipWithIndex.foreach { e => println(s"[${e._2}] = ${e._1}") }
    })

    val eligibleSectors: List[((Int, Int), Values)] =
      sectorHeads.map(h => (h, eligibleValues.map(_.slice(h._2, h._2 + sectorSize)).slice(h._1, h._1 + sectorSize)))

    eligibleSectors.foreach(s => {
      println(s"Sector ${s._1}")
      s._2.zipWithIndex.foreach(r => {
        r._1.zipWithIndex.foreach(c => {
          println(s"[${r._2},${c._2}] = ${c._1}")
        })
      })
    })

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

    def checkSectors(): Unit = {
      for (h <- sectorHeads) {
        val sectorValues = for (r <- h._1 until h._1 + sectorSize; c <- h._2 until h._2 + sectorSize) yield board(r, c)
        if (!referenceList.forall(sectorValues.contains)) {
          throw new IllegalStateException(s"Invalid sector $h: $sectorValues")
        }

      }
    }

    if (!board.gameSolved()) {
      throw new IllegalStateException("The game is not yet completely solved")
    }

    for (i <- 0 until boardSize) {
      checkRow(i)
      checkColumn(i)
    }
    checkSectors()
  }

}
