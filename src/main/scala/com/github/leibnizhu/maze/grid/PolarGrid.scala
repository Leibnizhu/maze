package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.{Cell, PolarCell}
import com.github.leibnizhu.maze.grid.Grid.{MAX_CELL_SIZE, MIN_CELL_SIZE}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}

import scala.util.Random

class PolarGrid(override val rows: Int) extends Grid {
  override val columns: Int = 1

  override protected def initializeCells(): Array[Array[Cell]] = {
    val allRows = new Array[Array[Cell]](rows)
    // 当作单位圆来处理，方便扩展尺寸
    val rowHeight = 1.0 / rows
    // 第一行，就是中间一个圆
    allRows(0) = Array(new PolarCell(0, 0))

    for (row <- 1 until rows) {
      // 这一行的内径
      val radius = row.toDouble / rows
      // 内径周长
      val perimeter = 2 * Math.PI * radius
      val prevCount = allRows(row - 1).length
      // 内径、或者说上一行，平均的单元格长度
      val estimateCellWidth = perimeter / prevCount
      // 上一行每一格长度，如果超过1.5倍的行高，则四舍五入就变成2以上，即需要拆分多个格子，因此用这个比例做拆分多个个格子的倍数
      val ratio = (estimateCellWidth / rowHeight).round.toInt
      val curCellCount = prevCount * ratio
      allRows(row) = (0 until curCellCount).map(new PolarCell(row, _)).toArray
    }
    allRows
  }

  override protected def linkCells(grid: Array[Array[Cell]]): Unit = eachCell(grid) {
    case curCell: PolarCell =>
      val (row, column) = (curCell.row, curCell.column)
      // 第0行只有一个格子，没有邻居，也没有上一层，所以不处理
      if (row > 0) {
        curCell.cw = Option(cell(row, column + 1, grid).asInstanceOf[PolarCell])
        curCell.ccw = Option(cell(row, column - 1, grid).asInstanceOf[PolarCell])
        val ratio = grid(row).length / grid(row - 1).length
        // 这行的ratio个格子会对应上一行一个格子
        val parentCell = cell(row - 1, column / ratio, grid).asInstanceOf[PolarCell]
        parentCell.outward = curCell :: parentCell.outward
        curCell.inward = Option(parentCell)
      }
  }

  override def randomCell(): Cell = {
    val column = _grid(Random.nextInt(rows))
    column(Random.nextInt(column.length))
  }

  override def cell(row: Int, column: Int, grid: Array[Array[Cell]] = _grid): Cell =
    if (row >= 0 && row < rows)
      // 同一行是一个圈，取模就闭环了
      val maxColumn = grid(row).length
      grid(row)(((column % maxColumn) + maxColumn) % maxColumn)
    else
      null

  override def centerCell(): Cell = cell(0, 0)

  override def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances], playMode: Boolean = false): Unit = {
    // 整体直径和中心位置（x,y都是一个值）
    val imgSize = 2 * rows * cellSize
    val center = imgSize / 2
    gc.translate(2, 2);
    // 按距离染色
    if (!playMode) {
      val font = new Font("System Regular", cellSize / 2)
      distances match {
        case Some(distances) =>
          val maxDist = distances.max()._2
          eachCell() {
            case curCell: PolarCell =>
              val (row, column) = (curCell.row, curCell.column)
              // 涂色
              val ((ax, ay), (bx, by), (cx, cy), (dx, dy)) = calCellPoints(cellSize, center, curCell, row)
              gc.setFill(distances.cellRgb(curCell, maxDist))
              // FIXME 第1圈的折线太明显
              // gc.fillRect(column * cellSize, row * cellSize, cellSize, cellSize)
              gc.fillPolygon(List((ax, ay), (bx, by), (dx, dy), (cx, cy)))

              // 距离值的文字，文字居中
              val distStr = distances.distance(curCell).map(_.toString).getOrElse("")
              gc.setFill(Color.Black)
              gc.setFont(font)
              val maxX = Array(ax, bx, cx, dx).max
              val minX = Array(ax, bx, cx, dx).min
              val maxY = Array(ay, by, cy, dy).max
              val minY = Array(ay, by, cy, dy).min
              val (textWidth, textHeight) = textSize(distStr, font)
              gc.fillText(distStr, (minX + maxX) / 2 - textWidth / 2, (minY + maxY) / 2 + textHeight / 4)
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
        val ((ax, ay), (bx, by), (cx, cy), (dx, dy)) = calCellPoints(cellSize, center, curCell, row)
        val maxX = Array(ax, bx, cx, dx).max
        val minX = Array(ax, bx, cx, dx).min
        val maxY = Array(ay, by, cy, dy).max
        val minY = Array(ay, by, cy, dy).min
        gc.fillText("⭐️", (minX + maxX) / 2 - textWidth / 2, (minY + maxY) / 2 + textHeight / 4)
      })
    }

    // 画边
    eachCell() {
      case curCell: PolarCell =>
        // 中间不用处理
        if (curCell.row != 0) {
          val (row, column) = (curCell.row, curCell.column)
          val ((ax, ay), (bx, by), (cx, cy), (dx, dy)) = calCellPoints(cellSize, center, curCell, row)
          gc.stroke = Color.Black
          gc.setLineWidth(2)
          // 由于是圆环，每个格子画向内（圆心）一格和顺时针方向的邻居边即可
          if (!curCell.linked(curCell.inward)) {
            // FIXME 画圆弧
            gc.strokeLine(ax, ay, cx, cy)
          }
          if (!curCell.linked(curCell.cw)) {
            gc.strokeLine(cx, cy, dx, dy)
          }
          // 最外层，画底边
          if (row == rows - 1) {
            // FIXME 画圆弧
            gc.strokeLine(bx, by, dx, dy)
          }
        }
    }
    gc.translate(-2, -2);
  }

  private def calCellPoints(cellSize: Int, center: Int, curCell: Cell, row: Int) = {
    // 每个格子的夹角、内径、外径、顺时针逆时针的边的角度
    val theta = 2 * Math.PI / _grid(row).length
    val innerRadius = curCell.row * cellSize
    val outerRadius = (curCell.row + 1) * cellSize
    val ccwTheta = curCell.column * theta
    val cwTheta = (curCell.column + 1) * theta
    // 格子四个顶点的座标
    val ax = center + (innerRadius * Math.cos(ccwTheta))
    val ay = center + (innerRadius * Math.sin(ccwTheta))
    val bx = center + (outerRadius * Math.cos(ccwTheta))
    val by = center + (outerRadius * Math.sin(ccwTheta))
    val cx = center + (innerRadius * Math.cos(cwTheta))
    val cy = center + (innerRadius * Math.sin(cwTheta))
    val dx = center + (outerRadius * Math.cos(cwTheta))
    val dy = center + (outerRadius * Math.sin(cwTheta))
    ((ax, ay), (bx, by), (cx, cy), (dx, dy))
  }

  override def cellSize(canvasWidth: Double, canvasHeight: Double): Int = {
    val size = Math.min(canvasHeight, canvasWidth).toInt / 2 / rows
    Math.min(Math.max(MIN_CELL_SIZE, size), MAX_CELL_SIZE)
  }
}
