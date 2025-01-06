package com.github.leibnizhu.maze.generate

import com.github.leibnizhu.maze.Grid

import scala.util.Random

/**
 * 猎杀算法
 * 1. 从随机点开始
 * 2. 避开已访问cell，随机游走，直到不能继续（被已访问cell包围）
 * 3. 进入猎杀模式，从上到下、从左到右遍历找到第一个未访问、且有已访问的邻居的cell，作为当前cell，并与任意已访问邻居相连
 * 4. 继续第2步随机游走，直到所有cell被访问
 */
object HuntAndKill {

  def on(grid: Grid): Grid = {
    var curCell = grid.randomCell()
    while (curCell != null) {
      val unvisitedNeighbors = curCell.neighbors().filter(c => c.links().isEmpty)
      if (unvisitedNeighbors.nonEmpty) {
        // 有未访问邻居则随机游走过去，打通连接
        val neighbor = unvisitedNeighbors(Random.nextInt(unvisitedNeighbors.length))
        curCell.link(neighbor)
        curCell = neighbor
      } else {
        // 邻居都访问过了，进入猎杀模式，首先curCell置空为了有机会退出循环
        curCell = null
        grid.eachCell() { (rowIndex, columnIndex, cell) =>
          val visitedNeighbors = cell.neighbors().filter(c => c.links().nonEmpty)
          val cellIsUnvisited = cell.links().isEmpty
          if (cellIsUnvisited && visitedNeighbors.nonEmpty) {
            // 当前格子未访问过，且有已访问过的邻居，则可以作为新的开始游走的格子
            curCell = cell
            val neighbor = visitedNeighbors(Random.nextInt(visitedNeighbors.length))
            curCell.link(neighbor)
          }
        }
      }
    }
    grid
  }
}
