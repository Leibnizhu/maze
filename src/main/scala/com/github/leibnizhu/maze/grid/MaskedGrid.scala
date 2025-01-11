package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.cell.{Cell, MatrixCell}

import scala.util.Random

class MaskedGrid(mask: Mask) extends MatrixGrid(mask.rows, mask.columns) {

  override def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        if (mask.isEnabled(row, column)) {
          grid(row)(column) = new MatrixCell(row, column)
        }
      }
    }
    grid
  }

  override def randomCell(): Cell = {
    val (row, column) = mask.randomLocation()
    cell(row, column)
  }

  override def size(): Int = mask.size()

  override def centerCell(): Cell = {
    var row = rows / 2
    var column = columns / 2
    // 随机游走找中心附近可用的点
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))
    while (!mask.isEnabled(row, column)) {
      val direction = directions(Random.nextInt(directions.size))
      row = Math.min(Math.max(0, row + direction._1), rows - 1)
      column = Math.min(Math.max(0, column + direction._2), columns - 1)
    }
    cell(row, column)
  }
}
