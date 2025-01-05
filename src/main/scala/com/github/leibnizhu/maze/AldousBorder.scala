package com.github.leibnizhu.maze

/**
  * Aldous-Border算法
  * 随机选择一个单元格，然后随机选择一个邻居，如果邻居没有链接过则打通；然后移动过去，继续随机选择邻居处理
  */
object AldousBorder {
    def on(grid: Grid): Unit = {
        var curCell = grid.randomCell()
        var unvisited = grid.size() - 1
        while (unvisited > 0) {
            val neighbor = curCell.randomNeighbor()
            if (neighbor.links().isEmpty) {
                // 如果邻居没有链接，则这个邻居是第一次被访问到
                curCell.link(neighbor)
                unvisited -= 1
            }
            curCell = neighbor
        }
    }
}
