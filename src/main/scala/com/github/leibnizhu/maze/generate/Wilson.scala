package com.github.leibnizhu.maze.generate

import com.github.leibnizhu.maze.Grid

import scala.util.Random

/**
 * Wilson算法
 * 随机选择一个未访问的单元格，开始随机游走，如果遇到当前路径的单元格，则消除环路继续；
 * 遇到当前路径已访问的单元格，则打通从开始的单元格到现在的已访问单元格之间的路径；然后继续随机选择未访问单元格
 */
object Wilson {

  def on(grid: Grid): Grid = {
    // 记录所有单元格是否访问，把二维数组展开为一维数组了，方便随机选取
    val unvisited = Array.fill(grid.rows * grid.columns)(true)
    // 随机选择一个单元格设为已访问，这样才能保证有路径
    val first: Int = randomUnvisited(unvisited)
    unvisited(first) = false
    while (unvisited.count(u => u) > 0) {
      // 随机选择一个未访问的单元格，加入访问中的路径，开始随机游走
      val curPos = randomUnvisited(unvisited)
      var (curRow, curColumn) = (curPos / grid.columns, curPos % grid.columns)
      // println(s"Start from $curPos, $curRow, $curColumn")
      var curCell = grid.cell(curRow, curColumn)
      var path = List[(Int, Int)]((curRow, curColumn))
      // 随机游走，直到遇到已访问的单元格
      while (unvisited(curCell.row * grid.columns + curCell.column)) {
        curCell = curCell.randomNeighbor()
        curRow = curCell.row
        curColumn = curCell.column
        val position = path.indexOf((curRow, curColumn))
        if (position == -1) {
          // 如果邻居没有在路径中，则加入路径
          path = path :+ (curRow, curColumn)
        } else {
          // 遇到当前路径的单元格，则消除环路(回到之前遇到这个cell的位置)继续
          path = path.take(position + 1)
        }
      }
      // 到这里就是遇到已访问的单元格，则打通从开始的单元格到现在的已访问单元格之间的路径
      for (i <- 0 until path.length - 1) {
        val curCell = grid.cell(path(i)._1, path(i)._2)
        val nextCell = grid.cell(path(i + 1)._1, path(i + 1)._2)
        curCell.link(nextCell)
        unvisited(curCell.row * grid.columns + curCell.column) = false
      }
    }
    grid
  }

  private def randomUnvisited(unvisited: Array[Boolean]): Int = {
    // 要选取第 targetIndex 个未访问的单元格；从0开始算的
    val targetIndex = Random.nextInt(unvisited.count(u => u))
    var index = 0
    // 当前是第几个未访问的单元格，一开始还没有，所以是-1；当变成targetIndex的时候就应该停止了
    var curPos = -1
    while (index < unvisited.length && curPos < targetIndex) {
      if (unvisited(index)) {
        curPos += 1
      }
      // curPos==targetIndex的时候，就找到了，不应该继续增加index
      if (curPos < targetIndex) {
        index += 1
      }
    }
    index
  }

  private def sample[T](list: List[T]): T = list(Random.nextInt(list.length))
}
