package com.github.leibnizhu.maze

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.shape.ArcType

import scala.util.Random

class PolarGrid(override val rows: Int) extends Grid(rows, 1) {

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
      val circumferrence = 2 * Math.PI * radius
      val prevCount = allRows(row - 1).length
      // 内径、或者说上一行，平均的单元格长度
      val estimateCellWidth = circumferrence / prevCount
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

  override def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances]): Unit = {
    // 整体直径和中心位置（x,y都是一个值）
    val imgSize = 2 * rows * cellSize
    val center = imgSize / 2
    // 按距离染色
    distances match {
      case Some(distances) =>
        val maxDist = distances.max()._2
      // TODO
      case None =>
    }
    eachCell() {
      case curCell: PolarCell =>
        // 中间不用处理
        if (curCell.row != 0) {
          val (row, column) = (curCell.row, curCell.column)
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
          // TODO 距离的文字
          gc.stroke = Color.Black
          gc.setLineWidth(2)
          // 由于是圆环，每个格子画向内（圆心）一格和顺时针方向的邻居边即可
          if (curCell.linked(curCell.inward)) {
            gc.strokeLine(ax, ay, cx, cy)
            //            gc.strokeArc(Math.min(ax,cx), Math.min(ay,cy), Math.abs(ax - cx), Math.abs(ay - cy), ccwTheta * 180 / Math.PI, theta * 180 / Math.PI, ArcType.Open)
          }
          if (curCell.linked(curCell.cw)) {
            gc.strokeLine(cx, cy, dx, dy)
          }
          // 最外层，画底边
          if (row == rows - 1) {
            gc.strokeLine(bx, by, dx, dy)
          }
        }
    }
  }
}
