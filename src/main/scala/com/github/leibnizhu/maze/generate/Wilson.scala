package com.github.leibnizhu.maze.generate

import com.github.leibnizhu.maze.cell.Cell
import com.github.leibnizhu.maze.grid.Grid

import scala.util.Random

/**
 * Wilson算法
 * 随机选择一个未访问的单元格，开始随机游走，如果遇到当前路径的单元格，则消除环路继续；
 * 遇到当前路径已访问的单元格，则打通从开始的单元格到现在的已访问单元格之间的路径；然后继续随机选择未访问单元格
 */
object Wilson {

  def on(grid: Grid): Grid = {
    // 记录所有单元格是否访问
    var unvisited = List[Cell]()
    grid.eachCell() { cell => if (cell != null) {
      unvisited = unvisited :+ cell
    }
    }
    // 随机选择一个单元格设为已访问，这样才能保证有路径
    val first = unvisited(Random.nextInt(unvisited.length))
    unvisited = unvisited.filter(c => c != first)
    while (unvisited.nonEmpty) {
      // 随机选择一个未访问的单元格，加入访问中的路径，开始随机游走
      var curCell = unvisited(Random.nextInt(unvisited.length))
      var path = List[Cell](curCell)
      // 随机游走，直到遇到已访问的单元格
      while (unvisited.contains(curCell)) {
        curCell = curCell.randomNeighbor()
        val position = path.indexOf(curCell)
        if (position == -1) {
          // 如果邻居没有在路径中，则加入路径
          path = path :+ (curCell)
        } else {
          // 遇到当前路径的单元格，则消除环路(回到之前遇到这个cell的位置)继续
          path = path.take(position + 1)
        }
      }
      // 到这里就是遇到已访问的单元格，则打通从开始的单元格到现在的已访问单元格之间的路径
      for (i <- 0 until path.length - 1) {
        val curCell = path(i)
        val nextCell = path(i + 1)
        curCell.link(nextCell)
        unvisited = unvisited.filter(c => c != curCell)
      }
    }
    grid
  }
}
