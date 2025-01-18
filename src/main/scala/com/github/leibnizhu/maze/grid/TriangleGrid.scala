package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.{Cell, HexCell, TriangleCell}
import com.github.leibnizhu.maze.grid.Grid.{MAX_CELL_SIZE, MIN_CELL_SIZE}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

class TriangleGrid(override val rows: Int, override val columns: Int) extends Grid {

  override protected def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new TriangleCell(row, column)
      }
    }
    grid
  }

  override protected def linkCells(grid: Array[Array[Cell]]): Unit =
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val curCell = grid(row)(column).asInstanceOf[TriangleCell]
        curCell.west = Option(cell(row, column - 1, grid).asInstanceOf[TriangleCell])
        curCell.east = Option(cell(row, column + 1, grid).asInstanceOf[TriangleCell])
        if (curCell.upright()) {
          curCell.south = Option(cell(row + 1, column, grid).asInstanceOf[TriangleCell])
        } else {
          curCell.north = Option(cell(row - 1, column, grid).asInstanceOf[TriangleCell])
        }
      }
    }

  override def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances], playMode: Boolean = false): Unit = {
    val halfWidth = cellSize / 2.0
    val height = cellSize * Math.sqrt(3) / 2.0
    gc.translate(2, 2);

    // 按距离染色
    if (!playMode) {
      val font = new Font("System Regular", cellSize / 3)
      distances match {
        case Some(distances) =>
          val maxDist = distances.max()._2
          eachCell() {
            case curCell: TriangleCell =>
              val (row, column) = (curCell.row, curCell.column)
              gc.setFill(distances.cellRgb(curCell, maxDist))
              val (wx, mx, ex, apexY, baseY) = triangleCellXy(halfWidth, height, curCell, row, column)
              gc.fillPolygon(List((wx, baseY), (ex, baseY), (mx, apexY)))

              // 距离值的文字，文字居中
              val distStr = distances.distance(curCell).map(_.toString).getOrElse("")
              gc.setFill(Color.Black)
              gc.setFont(font)
              val (textWidth, textHeight) = textSize(distStr, font)
              gc.fillText(distStr, (wx + ex) / 2 - textWidth / 2, (apexY / 3 + baseY * 2 / 3) + textHeight / 4)
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
        val (wx, mx, ex, apexY, baseY) = triangleCellXy(halfWidth, height, curCell.asInstanceOf[TriangleCell], row, column)
        gc.fillText("⭐️", (wx + ex) / 2 - textWidth / 2, (apexY / 3 + baseY * 2 / 3) + textHeight / 4)
      })
    }

    // 画边
    eachCell() {
      case curCell: TriangleCell =>
        val (row, column) = (curCell.row, curCell.column)
        val (wx, mx, ex, apexY, baseY) = triangleCellXy(halfWidth, height, curCell, row, column)

        gc.stroke = Color.Black
        gc.setLineWidth(2)
        if (!curCell.linked(curCell.west)) {
          gc.strokeLine(wx, baseY, mx, apexY)
        }
        if (!curCell.linked(curCell.east)) {
          gc.strokeLine(ex, baseY, mx, apexY)
        }
        // 如果三角形朝上，那么有南边邻居，判断是否连南边邻居；如果朝下，那么有北边邻居，判断是否连北边邻居
        val upUnlinkedSouth = curCell.upright() && !curCell.linked(curCell.south)
        val downUnlinkedNorth = !curCell.upright() && !curCell.linked(curCell.north)
        if (upUnlinkedSouth || downUnlinkedNorth) {
          gc.strokeLine(wx, baseY, ex, baseY)
        }
    }
    gc.translate(-2, -2);
  }

  private def triangleCellXy(halfWidth: Double, height: Double, curCell: TriangleCell, row: Int, column: Int) = {
    val halfHeight = height / 2
    // 三角形的中心点（高度和宽度的中间）
    val cx = halfWidth + column * halfWidth
    val cy = halfHeight + row * height
    // 三角形三个顶点的x、y取值;apex是上下的顶点，base是上下的底边
    val (wx, mx, ex) = (cx - halfWidth, cx, cx + halfWidth)
    val (apexY, baseY) = if (curCell.upright()) {
      (cy - halfHeight, cy + halfHeight)
    } else {
      (cy + halfHeight, cy - halfHeight)
    }
    (wx, mx, ex, apexY, baseY)
  }

  override def cellSize(canvasWidth: Double, canvasHeight: Double): Int = {
    val sizeByHeight = canvasHeight / rows / (Math.sqrt(3) / 2)
    val sizeByWidth = canvasWidth * 2 / (columns + 1)
    val cellSize = Math.min(sizeByHeight, sizeByWidth).toInt
    Math.min(Math.max(MIN_CELL_SIZE, cellSize), 60)
  }
}
