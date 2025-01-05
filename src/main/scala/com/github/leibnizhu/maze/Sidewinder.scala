package com.github.leibnizhu.maze

import scala.util.Random

object Sidewinder {

  def on(grid: Grid): Grid = {
    grid.eachRow() { (_, row) =>
      // 每一行向东铺设，run用于维护当前铺展
      var run = List[Cell]()
      row.foreach { cell =>
        run = run :+ cell
        val atEasternBoundary = cell.east.isEmpty
        val atNorthernBoundary = cell.north.isEmpty
        // 需要关闭run的情况：已经到了最东边，或者在北边，且随机数小于0.5
        val shouldCloseOut = atEasternBoundary || (!atNorthernBoundary && Math.random() < 0.5)
        if (shouldCloseOut) {
          // 随机选择run中的一个单元格，与当前单元格的东边墙壁链接
          val randomCell = run(Random.nextInt(run.length))
          randomCell.north.foreach(randomCell.link(_))
          run = List()
        } else {
          // 否则继续铺展
          cell.east.foreach(cell.link(_))
        }
      }
    }
    grid
  }
}
