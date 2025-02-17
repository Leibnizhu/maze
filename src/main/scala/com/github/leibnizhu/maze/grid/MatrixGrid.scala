package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.{Cell, MatrixCell}
import com.github.leibnizhu.maze.grid.Grid.{MAX_CELL_SIZE, MIN_CELL_SIZE}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}

class MatrixGrid(override val rows: Int, override val columns: Int) extends Grid {

  override protected def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new MatrixCell(row, column)
      }
    }
    grid
  }

  override protected def linkCells(grid: Array[Array[Cell]]): Unit =
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val cell = grid(row)(column).asInstanceOf[MatrixCell]
        if (cell != null) {
          if (row > 0) {
            cell.north = Option(grid(row - 1)(column).asInstanceOf[MatrixCell])
          }
          if (row < rows - 1) {
            cell.south = Option(grid(row + 1)(column).asInstanceOf[MatrixCell])
          }
          if (column > 0) {
            cell.west = Option(grid(row)(column - 1).asInstanceOf[MatrixCell])
          }
          if (column < columns - 1) {
            cell.east = Option(grid(row)(column + 1).asInstanceOf[MatrixCell])
          }
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
        val safeCell = Option(cell).getOrElse(new MatrixCell(-1, -1)).asInstanceOf[MatrixCell]
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

  def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances] = None, playMode: Boolean = false): Unit = {
    gc.translate(2, 2);
    // 按距离染色
    if (!playMode) {
      val font = new Font("System Regular", cellSize / 2)
      distances match {
        case Some(distances) =>
          val maxDist = distances.max()._2
          eachCell() { cell =>
            // 如果为空，则是遮罩里面的，无需渲染
            if (cell != null) {
              val (row, column) = (cell.row, cell.column)
              gc.setFill(distances.cellRgb(cell, maxDist))
              // FIXME 四边形，对第0、1圈会填不满
              gc.fillRect(column * cellSize, row * cellSize, cellSize, cellSize)

              // 距离值显示,文字居中
              val distStr = distances.distance(cell).map(_.toString).getOrElse("")
              gc.setFill(Color.Black)
              gc.setFont(font)
              val (textWidth, textHeight) = textSize(distStr, font)
              val (leftX, topY) = (column * cellSize, row * cellSize)
              gc.fillText(distStr, leftX + cellSize / 2 - textWidth / 2, topY + cellSize / 2 + textHeight / 4)
            }
          }
        case None =>
      }
    } else {
      val font = new Font("System Regular", cellSize)
      gc.setFill(Color.Red)
      gc.setFont(font)
      val (textWidth, textHeight) = textSize("⭐️", font)
      val (entry, exit) = entryAndExit(distances.get)
      Array(entry, exit).foreach(curCell => {
        val (row, column) = (curCell.row, curCell.column)
        val (leftX, topY) = (column * cellSize, row * cellSize)
        gc.fillText("⭐️", leftX + cellSize / 2 - textWidth / 2, topY + cellSize / 2 + textHeight / 4)
      })
    }

    // 画边框
    eachCell() { cell =>
      if (cell != null) {
        val (row, column) = (cell.row, cell.column)
        val (topY, bottomY) = (row * cellSize, (row + 1) * cellSize)
        val (leftX, rightX) = (column * cellSize, (column + 1) * cellSize)
        gc.stroke = Color.Black
        gc.setLineWidth(2)
        // 每个格子如果没有连着东边，则画东边界
        if (!cell.linked(cell.asInstanceOf[MatrixCell].east)) {
          gc.strokeLine(rightX, topY, rightX, bottomY)
        }
        // 如果格子没有连着西边，则画西边界
        if (!cell.linked(cell.asInstanceOf[MatrixCell].west)) {
          gc.strokeLine(leftX, topY, leftX, bottomY)
        }
        // 每个格子如果没有连着南边，则画下边界
        if (!cell.linked(cell.asInstanceOf[MatrixCell].south)) {
          gc.strokeLine(leftX, bottomY, rightX, bottomY)
        }
        // 如果格子没有连着北边，则画上边界
        if (!cell.linked(cell.asInstanceOf[MatrixCell].north)) {
          gc.strokeLine(leftX, topY, rightX, topY)
        }
      }
    }
    gc.translate(-2, -2);
  }

  override def cellSize(canvasWidth: Double, canvasHeight: Double): Int = {
    val cellSize = Math.min(canvasHeight / rows, canvasWidth / columns).toInt
    Math.min(Math.max(MIN_CELL_SIZE, cellSize), MAX_CELL_SIZE)
  }
}
