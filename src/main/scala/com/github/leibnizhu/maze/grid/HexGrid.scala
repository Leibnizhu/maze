package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.{Cell, HexCell, UpsilonCell}
import com.github.leibnizhu.maze.grid.Grid.{MAX_CELL_SIZE, MIN_CELL_SIZE}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

class HexGrid(override val rows: Int, override val columns: Int) extends Grid {

  override protected def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new HexCell(row, column)
      }
    }
    grid
  }

  override protected def linkCells(grid: Array[Array[Cell]]): Unit =
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val curCell = grid(row)(column).asInstanceOf[HexCell]
        // 南北对角线的行，相对当前单元格的变化；如果是偶数列，那么东北和西北邻居都在上一行；反之，东南和西南邻居都在本行
        val (northDiagonal, southDiagonal) = if (column % 2 == 0) {
          (row - 1, row)
        } else {
          (row, row + 1)
        }
        curCell.northWest = Option(cell(northDiagonal, column - 1, grid).asInstanceOf[HexCell])
        curCell.north = Option(cell(row - 1, column, grid).asInstanceOf[HexCell])
        curCell.northEast = Option(cell(northDiagonal, column + 1, grid).asInstanceOf[HexCell])
        curCell.southWest = Option(cell(southDiagonal, column - 1, grid).asInstanceOf[HexCell])
        curCell.south = Option(cell(row + 1, column, grid).asInstanceOf[HexCell])
        curCell.southEast = Option(cell(southDiagonal, column + 1, grid).asInstanceOf[HexCell])
      }
    }

  override def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances], playMode: Boolean = false): Unit = {
    // 1/4宽度
    val aSize = cellSize / 2.0
    // 1/2高度
    val bSize = cellSize * Math.sqrt(3) / 2.0
    val width = cellSize * 2
    val height = bSize * 2
    gc.translate(2, 2);

    // 按距离染色
    if (!playMode) {
      val font = new Font("System Regular", cellSize * 3 / 5)
      distances match {
        case Some(distances) =>
          val maxDist = distances.max()._2
          eachCell() {
            case curCell: HexCell =>
              val (row, column) = (curCell.row, curCell.column)
              gc.setFill(distances.cellRgb(curCell, maxDist))
              val (xfw, xnw, xne, xfe, yn, ym, ys) = hexPointXy(cellSize, aSize, bSize, height, row, column)
              gc.fillPolygon(List((xfw, ym), (xnw, yn), (xne, yn), (xfe, ym), (xne, ys), (xnw, ys)))

              // 距离值的文字，文字居中
              val distStr = distances.distance(curCell).map(_.toString).getOrElse("")
              gc.setFill(Color.Black)
              gc.setFont(font)
              val (textWidth, textHeight) = textSize(distStr, font)
              gc.fillText(distStr, (xfw + xfe) / 2 - textWidth / 2, (yn + ys) / 2 + textHeight / 4)
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
        val (xfw, xnw, xne, xfe, yn, ym, ys) = hexPointXy(cellSize, aSize, bSize, height, row, column)
        gc.fillText("⭐️", (xfw + xfe) / 2 - textWidth / 2, (yn + ys) / 2 + textHeight / 4)
      })
    }

    // 画边
    eachCell() {
      case curCell: HexCell =>
        val (row, column) = (curCell.row, curCell.column)
        val (xfw, xnw, xne, xfe, yn, ym, ys) = hexPointXy(cellSize, aSize, bSize, height, row, column)
        gc.stroke = Color.Black
        gc.setLineWidth(2)
        if (!curCell.linked(curCell.northWest)) {
          gc.strokeLine(xfw, ym, xnw, yn)
        }
        if (!curCell.linked(curCell.north)) {
          gc.strokeLine(xnw, yn, xne, yn)
        }
        if (!curCell.linked(curCell.northEast)) {
          gc.strokeLine(xne, yn, xfe, ym)
        }
        if (!curCell.linked(curCell.southEast)) {
          gc.strokeLine(xfe, ym, xne, ys)
        }
        if (!curCell.linked(curCell.south)) {
          gc.strokeLine(xne, ys, xnw, ys)
        }
        if (!curCell.linked(curCell.southWest)) {
          gc.strokeLine(xnw, ys, xfw, ym)
        }
    }
    gc.translate(-2, -2);
  }

  private def hexPointXy(cellSize: Int, aSize: Double, bSize: Double, height: Double, row: Int, column: Int) = {
    // 六边形的中心点,偶数列需要下移半个高度
    val cx = cellSize + 3 * column * aSize
    // 奇数列要下移半格
    val cy = bSize + row * height + (if (column % 2 == 1) bSize else 0)
    // 六边形6个点，总共的有4个x取值，3个y取值， 按方位缩写，news=北东西南, 多个东/西用 f/n=远/近 区分, m=中间
    val (xfw, xnw, xne, xfe) = (cx - cellSize, cx - aSize, cx + aSize, cx + cellSize)
    val (yn, ym, ys) = (cy - bSize, cy, cy + bSize)
    (xfw, xnw, xne, xfe, yn, ym, ys)
  }

  override def cellSize(canvasWidth: Double, canvasHeight: Double): Int = {
    val sizeByHeight = canvasHeight / (Math.sqrt(3) * (rows + 1.0 / 2))
    val sizeByWidth = canvasWidth * 2 / (3 * columns + 1)
    val cellSize = Math.min(sizeByHeight, sizeByWidth).toInt
    Math.min(Math.max(MIN_CELL_SIZE, cellSize), MAX_CELL_SIZE)
  }
}

