package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.{Cell, OverCell, UnderCell}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

class WeaveGrid(override val rows: Int, override val columns: Int) extends MatrixGrid(rows, columns) {
  private var underCells = List[UnderCell]()

  override protected def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new OverCell(row, column, this)
      }
    }
    grid
  }

  override protected def linkCells(grid: Array[Array[Cell]]): Unit =
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val cell = grid(row)(column).asInstanceOf[OverCell]
        if (cell != null) {
          if (row > 0) {
            cell.north = Option(grid(row - 1)(column).asInstanceOf[OverCell])
          }
          if (row < rows - 1) {
            cell.south = Option(grid(row + 1)(column).asInstanceOf[OverCell])
          }
          if (column > 0) {
            cell.west = Option(grid(row)(column - 1).asInstanceOf[OverCell])
          }
          if (column < columns - 1) {
            cell.east = Option(grid(row)(column + 1).asInstanceOf[OverCell])
          }
        }
      }
    }

  def tunnelUnder(overCell: OverCell): Unit = {
    val underCell = new UnderCell(overCell)
    underCells = underCell :: underCells
  }

  override def eachCell(grid: Array[Array[Cell]])(f: Cell => Unit): Unit = {
    super.eachCell(grid)(f)
    underCells.foreach(f)
  }

  override def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances], playMode: Boolean): Unit = {
    val inset = cellSize.doubleValue / 10
    gc.translate(2, 2);
    // 按距离染色


    // 画边框
    eachCell() {
      case curCell: UnderCell =>
        val (row, column) = (curCell.row, curCell.column)
        val (x1, x2, x3, x4, y1, y2, y3, y4) = insetXy(row, column, cellSize, inset)
        gc.stroke = Color.Black
        gc.setLineWidth(2)
        if (curCell.verticalPassage()) {
          gc.strokeLine(x2, y1, x2, y2)
          gc.strokeLine(x3, y1, x3, y2)
          gc.strokeLine(x2, y3, x2, y4)
          gc.strokeLine(x3, y3, x3, y4)
        } else {
          gc.strokeLine(x1, y2, x2, y2)
          gc.strokeLine(x1, y3, x2, y3)
          gc.strokeLine(x3, y2, x4, y2)
          gc.strokeLine(x3, y3, x4, y3)
        }
      case cell@(curCell: OverCell) =>
        val (row, column) = (curCell.row, curCell.column)
        val (x1, x2, x3, x4, y1, y2, y3, y4) = insetXy(row, column, cellSize, inset)
        gc.stroke = Color.Black
        gc.setLineWidth(2)
        // 画东边界或东通道
        if curCell.linked(curCell.east) then {
          gc.strokeLine(x3, y2, x4, y2)
          gc.strokeLine(x3, y3, x4, y3)
        } else {
          gc.strokeLine(x3, y2, x3, y3)
        }
        // 画西边界或西通道
        if (curCell.linked(curCell.west)) {
          gc.strokeLine(x1, y2, x2, y2)
          gc.strokeLine(x1, y3, x2, y3)
        } else {
          gc.strokeLine(x2, y2, x2, y3)
        }
        // 画南边界或南通道
        if (curCell.linked(curCell.south)) {
          gc.strokeLine(x2, y3, x2, y4)
          gc.strokeLine(x3, y3, x3, y4)
        } else {
          gc.strokeLine(x2, y3, x3, y3)
        }
        // 画北边界或北通道
        if (curCell.linked(curCell.north)) {
          gc.strokeLine(x2, y1, x2, y2)
          gc.strokeLine(x3, y1, x3, y2)
        } else {
          gc.strokeLine(x2, y2, x3, y2)
        }
    }
    gc.translate(-2, -2);

  }

  private def insetXy(row: Int, column: Int, cellSize: Int, inset: Double) = {
    val (x1, y1) = (column * cellSize, row * cellSize)
    val (x4, y4) = (x1 + cellSize, y1 + cellSize)
    (x1, x1 + inset, x4 - inset, x4, y1, y1 + inset, y4 - inset, y4)
  }
}
