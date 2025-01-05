package com.github.leibnizhu.maze

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

import scala.util.Random

class Grid(rows: Int, columns: Int) {

  private val _grid: Array[Array[Cell]] = initGrid()

  private def initGrid(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    // 初始化所有单元格
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new Cell(row, column)
      }
    }
    // 链接单元格
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val cell = grid(row)(column)
        if (row > 0) {
          cell.north = Some(grid(row - 1)(column))
        }
        if (row < rows - 1) {
          cell.south = Some(grid(row + 1)(column))
        }
        if (column > 0) {
          cell.west = Some(grid(row)(column - 1))
        }
        if (column < columns - 1) {
          cell.east = Some(grid(row)(column + 1))
        }
      }
    }
    grid
  }

  def randomCell(): Cell = {
    // 随机选择一个单元格
    val row = Random.nextInt(rows)
    val column = Random.nextInt(columns)
    _grid(row)(column)
  }

  def cell(row: Int, column: Int): Cell =
    if (row >= 0 && row < rows && column >= 0 && column < columns)
      _grid(row)(column)
    else
      null

  def size(): Int = rows * columns

  /** 遍历所有行
   *
   * @param f
   * 对每一行执行的函数
   */
  def eachRow()(f: (Int, Array[Cell]) => Unit): Unit = {
    for ((row, index) <- _grid.zipWithIndex) {
      f(index, row)
    }
  }

  /** 遍历所有单元格
   *
   * @param f
   * 对每一个单元格执行的函数
   */
  def eachCell()(f: (Int, Int, Cell) => Unit): Unit = {
    for ((row, rowIndex) <- _grid.zipWithIndex) {
      for ((cell, columnIndex) <- row.zipWithIndex) {
        f(rowIndex, columnIndex, cell)
      }
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
    // 最顶上的边界
    sb.append("+" + "---+" * columns + "\n")
    eachRow() { (_, row) =>
      // 每一行先打印格子，然后是下边界；这两部分最左边是固定的
      sb.append("|")
      val bottom = new StringBuilder("+")
      row.foreach { cell =>
        val safeCell = Option(cell).getOrElse(new Cell(-1, -1))
        // 每个格子先加入自己的空，然后是东边墙壁
        sb.append("   ")
        sb.append(if (safeCell.linked(safeCell.east)) " " else "|")
        // 下边界
        bottom.append(if (safeCell.linked(safeCell.south)) "   +" else "---+")
      }
      sb.append("\n").append(bottom.toString()).append("\n")
    }
    sb.toString()
  }

  def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances] = None): Unit = {
    // 按距离染色
    distances match {
      case Some(distances) =>
        val maxDist = distances.max()._2
        eachCell() { (rowIndex, columnIndex, cell) =>
          val safeCell = Option(cell).getOrElse(new Cell(-1, -1))
          val intensity = (maxDist - distances.distance(safeCell).getOrElse(0).toDouble) / maxDist
          val dark = (255 * intensity).toInt
          val bright = 128 + (127 * intensity).toInt
          gc.setFill(Color.rgb(dark, bright, bright))
          gc.fillRect(columnIndex * cellSize, rowIndex * cellSize, cellSize, cellSize)
        }
      case None =>
    }
    gc.stroke = Color.Black
    // 最顶上的边界
    gc.setLineWidth(2)
    gc.strokeLine(0, 0, columns * cellSize, 0)
    eachRow() { (rowIndex, row) =>
      // 每一行先打印格子，然后是下边界；这两部分最左边是固定的
      val (topY, bottomY) = (rowIndex * cellSize, (rowIndex + 1) * cellSize)
      gc.strokeLine(0, topY, 0, bottomY)
      for ((cell, columnIndex) <- row.zipWithIndex) {
        val (leftX, rightX) = (columnIndex * cellSize, (columnIndex + 1) * cellSize)
        val safeCell = Option(cell).getOrElse(new Cell(-1, -1))
        distances match {
          case Some(distances) =>
            gc.setFill(Color.Black)
            // 文字居中，需要优化，按一个字5*5来计算，不一定准确
            val distStr = distances.distance(safeCell).map(_.toString).getOrElse("")
            gc.fillText(distStr, leftX + cellSize / 2 - distStr.length * 5, topY + cellSize / 2 + 5)
          case None =>
        }
        // 每个格子如果没有连着东边，则画东边墙壁
        if (!safeCell.linked(safeCell.east)) {
          gc.strokeLine(rightX, topY, rightX, bottomY)
        }
        // 每个格子如果没有连着南边，则画下边界
        if (!safeCell.linked(safeCell.south)) {
          gc.strokeLine(leftX, bottomY, rightX, bottomY)
        }
      }
    }
  }
}


