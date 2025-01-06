package com.github.leibnizhu.maze.generate

import com.github.leibnizhu.maze.Grid

import scala.util.Random

/**
 * 递归回溯算法/DFS
 * 深度优先遍历整个迷宫，移动的时候连接格子，直到所有被连上
 */
object RecursiveBacktracker {


  def on(grid: Grid): Grid = {
    val startAt = grid.randomCell()
    // 记录当前访问路径的栈，head是栈顶
    var stack = List(startAt)
    while (stack.nonEmpty) {
      val curCell = stack.head
      // 未访问的邻居
      val neighbors = curCell.neighbors().filter(c => c.links().isEmpty)
      if (neighbors.isEmpty) {
        //邻居全访问完，可以出栈，即回溯上一个格子
        stack = stack.tail
      } else {
        // 随机选择一个未访问邻居，移动过去，然后入栈
        val neighbor = neighbors(Random.nextInt(neighbors.length))
        curCell.link(neighbor)
        stack = neighbor :: stack
      }
    }
    grid
  }
}
