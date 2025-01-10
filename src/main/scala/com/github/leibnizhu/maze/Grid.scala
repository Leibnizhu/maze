package com.github.leibnizhu.maze

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}

import scala.util.Random

class Grid(val rows: Int, val columns: Int) {

  protected val _grid: Array[Array[Cell]] = initGrid()

  private def initGrid(): Array[Array[Cell]] = {
    // 初始化所有单元格
    val grid = initializeCells()
    // 链接单元格
    linkCells(grid)
    grid
  }

  protected def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new Cell(row, column)
      }
    }
    grid
  }

  protected def linkCells(grid: Array[Array[Cell]]): Unit = {
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val cell = grid(row)(column)
        if (cell != null) {
          if (row > 0) {
            cell.north = Option(grid(row - 1)(column))
          }
          if (row < rows - 1) {
            cell.south = Option(grid(row + 1)(column))
          }
          if (column > 0) {
            cell.west = Option(grid(row)(column - 1))
          }
          if (column < columns - 1) {
            cell.east = Option(grid(row)(column + 1))
          }
        }
      }
    }
  }

  def randomCell(): Cell = {
    // 随机选择一个单元格
    val row = Random.nextInt(rows)
    val column = Random.nextInt(columns)
    _grid(row)(column)
  }

  def cell(row: Int, column: Int, grid: Array[Array[Cell]] = _grid): Cell =
    if (row >= 0 && row < rows && column >= 0 && column < columns)
      grid(row)(column)
    else
      null

  def size(): Int = rows * columns

  def centerCell(): Cell = cell(rows / 2, columns / 2)

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
  def eachCell(grid: Array[Array[Cell]] = _grid)(f: Cell => Unit): Unit = {
    for (row <- grid) {
      for (cell <- row) {
        f(cell)
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
        eachCell() { cell =>
          // 如果为空，则是遮罩里面的，无需渲染
          if (cell != null) {
            val (row, column) = (cell.row, cell.column)
            val intensity = (maxDist - distances.distance(cell).getOrElse(0).toDouble) / maxDist
            val dark = (255 * intensity).toInt
            val bright = 160 + (95 * intensity).toInt
            gc.setFill(Color.rgb(dark, bright, dark))
            // FIXME 四边形，对第0、1圈会填不满
            gc.fillRect(column * cellSize, row * cellSize, cellSize, cellSize)
          }
        }
      case None =>
    }
    eachCell() { cell =>
      if (cell != null) {
        val (row, column) = (cell.row, cell.column)
        val (topY, bottomY) = (row * cellSize, (row + 1) * cellSize)
        val (leftX, rightX) = (column * cellSize, (column + 1) * cellSize)
        // 距离显示
        distances match {
          case Some(distances) =>
            // 文字居中
            val distStr = distances.distance(cell).map(_.toString).getOrElse("")
            val font = new Font("System Regular", cellSize / 2)
            gc.setFill(Color.Black)
            gc.setFont(font)
            val textNode = new Text(distStr)
            textNode.setFont(font)
            val textWidth = textNode.getLayoutBounds.getWidth
            val textHeight = textNode.getLayoutBounds.getHeight
            gc.fillText(distStr, leftX + cellSize / 2 - textWidth / 2, topY + cellSize / 2 + textHeight / 4)
          case None =>
        }
        gc.stroke = Color.Black
        gc.setLineWidth(2)
        // 每个格子如果没有连着东边，则画东边界
        if (!cell.linked(cell.east)) {
          gc.strokeLine(rightX, topY, rightX, bottomY)
        }
        // 如果格子没有连着西边，则画西边界
        if (!cell.linked(cell.west)) {
          gc.strokeLine(leftX, topY, leftX, bottomY)
        }
        // 每个格子如果没有连着南边，则画下边界
        if (!cell.linked(cell.south)) {
          gc.strokeLine(leftX, bottomY, rightX, bottomY)
        }
        // 如果格子没有连着北边，则画上边界
        if (!cell.linked(cell.north)) {
          gc.strokeLine(leftX, topY, rightX, topY)
        }
      }
    }
  }
}


