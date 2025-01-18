package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.{Cell, UpsilonCell}
import com.github.leibnizhu.maze.grid.Grid.{MAX_CELL_SIZE, MIN_CELL_SIZE}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

class UpsilonGrid(override val rows: Int, override val columns: Int) extends Grid {

  override protected def initializeCells(): Array[Array[Cell]] = {
    val grid = Array.ofDim[Cell](rows, columns)
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        grid(row)(column) = new UpsilonCell(row, column)
      }
    }
    grid
  }

  override protected def linkCells(grid: Array[Array[Cell]]): Unit =
    for (row <- 0 until rows) {
      for (column <- 0 until columns) {
        val curCell = grid(row)(column).asInstanceOf[UpsilonCell]
        curCell.north = Option(cell(row - 1, column, grid).asInstanceOf[UpsilonCell])
        curCell.east = Option(cell(row, column + 1, grid).asInstanceOf[UpsilonCell])
        curCell.west = Option(cell(row, column - 1, grid).asInstanceOf[UpsilonCell])
        curCell.south = Option(cell(row + 1, column, grid).asInstanceOf[UpsilonCell])
        // 四边形的cell只有东南西北邻居，而八边形还有四个对角方向的邻居
        if (curCell.isOctagon()) {
          curCell.northWest = Option(cell(row - 1, column - 1, grid).asInstanceOf[UpsilonCell])
          curCell.northEast = Option(cell(row - 1, column + 1, grid).asInstanceOf[UpsilonCell])
          curCell.southEast = Option(cell(row + 1, column + 1, grid).asInstanceOf[UpsilonCell])
          curCell.southWest = Option(cell(row + 1, column - 1, grid).asInstanceOf[UpsilonCell])
        }
      }
    }

  override def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances], playMode: Boolean = false): Unit = {
    gc.translate(2, 2);

    // 按距离染色
    if (!playMode) {
      val font = new Font("System Regular", cellSize / 2)
      distances match {
        case Some(distances) =>
          val maxDist = distances.max()._2
          eachCell() {
            case curCell: UpsilonCell =>
              val (row, column) = (curCell.row, curCell.column)
              gc.setFill(distances.cellRgb(curCell, maxDist))
              val (xfw, xnw, xm, xne, xfe, yfn, ynn, ym, yns, yfs) = upsilonPointXy(cellSize, row, column, curCell.isOctagon())
              if (curCell.isOctagon()) {
                gc.fillPolygon(List((xfw, ynn), (xnw, yfn), (xne, yfn), (xfe, ynn), (xfe, yns), (xne, yfs), (xnw, yfs), (xfw, yns)))
              } else {
                gc.fillPolygon(List((xnw, ynn), (xne, ynn), (xne, yns), (xnw, yns)))
              }

              // 距离值的文字，文字居中
              val distStr = distances.distance(curCell).map(_.toString).getOrElse("")
              gc.setFill(Color.Black)
              gc.setFont(font)
              val (textWidth, textHeight) = textSize(distStr, font)
              gc.fillText(distStr, xm - textWidth / 2, ym + textHeight / 4)
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
        val xys = upsilonPointXy(cellSize, row, column, curCell.asInstanceOf[UpsilonCell].isOctagon())
        gc.fillText("⭐️", xys._3 - textWidth / 2, xys._8 + textHeight / 4)
      })
    }

    // 画边
    eachCell() {
      case curCell: UpsilonCell =>
        val (row, column) = (curCell.row, curCell.column)
        val (xfw, xnw, xm, xne, xfe, yfn, ynn, ym, yns, yfs) = upsilonPointXy(cellSize, row, column, curCell.isOctagon())
        gc.stroke = Color.Black
        gc.setLineWidth(2)
        if (!curCell.linked(curCell.west)) {
          gc.strokeLine(xfw, ynn, xfw, yns)
        }
        if (!curCell.linked(curCell.north)) {
          gc.strokeLine(xnw, yfn, xne, yfn)
        }
        if (!curCell.linked(curCell.east)) {
          gc.strokeLine(xfe, ynn, xfe, yns)
        }
        if (!curCell.linked(curCell.south)) {
          gc.strokeLine(xnw, yfs, xne, yfs)
        }
        if (curCell.isOctagon()) {
          if (!curCell.linked(curCell.northWest)) {
            gc.strokeLine(xfw, ynn, xnw, yfn)
          }
          if (!curCell.linked(curCell.northEast)) {
            gc.strokeLine(xne, yfn, xfe, ynn)
          }
          if (!curCell.linked(curCell.southEast)) {
            gc.strokeLine(xfe, yns, xne, yfs)
          }
          if (!curCell.linked(curCell.southWest)) {
            gc.strokeLine(xnw, yfs, xfw, yns)
          }
        }
    }

    gc.translate(-2, -2);
  }

  private def upsilonPointXy(cellSize: Int, row: Int, column: Int, isOctagon: Boolean) = {
    val near = cellSize / 2
    val far = near + cellSize / Math.sqrt(2)
    val xm = (near + far) * column + far
    val ym = (near + far) * row + far
    val xnw = xm - near
    val xne = xm + near
    val ynn = ym - near
    val yns = ym + near
    if (isOctagon) {
      val xfw = xm - far
      val xfe = xm + far
      val yfn = ym - far
      val yfs = ym + far
      (xfw, xnw, xm, xne, xfe, yfn, ynn, ym, yns, yfs)
    } else {
      (xnw, xnw, xm, xne, xne, ynn, ynn, ym, yns, yns)
    }
  }

  override def cellSize(canvasWidth: Double, canvasHeight: Double): Int = {
    val sizeByHeight = canvasHeight / (Math.sqrt(2) + 1 + (rows - 1) * (1 + 1 / Math.sqrt(2)))
    val sizeByWidth = canvasWidth / (Math.sqrt(2) + 1 + (columns - 1) * (1 + 1 / Math.sqrt(2)))
    val cellSize = Math.min(sizeByHeight, sizeByWidth).toInt
    Math.min(Math.max(MIN_CELL_SIZE, cellSize), MAX_CELL_SIZE)
  }
}
