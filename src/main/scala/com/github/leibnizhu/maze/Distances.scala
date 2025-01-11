package com.github.leibnizhu.maze

class Distances(root: Cell) {
  private var cells: Map[Cell, Int] = Map()

  /**
   * 计算从起点到所有单元格的距离
   *
   * @return this
   */
  def distances(): Distances = {
    cells = cells + (root -> 0)
    // 广度优先搜索，frontier是当前正在探索的单元格列表
    var frontier = List[Cell](root)
    while (frontier.nonEmpty) {
      // 下一次要探索的单元格列表
      var newFrontier = List[Cell]()
      for (cell <- frontier) {
        for (linkedCell <- cell.links()) {
          if (!cells.contains(linkedCell)) {
            // 新探索的单元格，距离为当前单元格距离加1
            cells = cells + (linkedCell -> (cells(cell) + 1))
            newFrontier = newFrontier :+ linkedCell
          }
        }
      }
      frontier = newFrontier
    }
    this
  }

  def distance(another: Cell): Option[Int] = cells.get(another)

  /**
   * 添加一个单元格到距离表中
   *
   * @param cell 单元格
   * @param distance 距离
   * @return this
   */
  def +(cell: Cell, distance: Int): Distances = {
    cells = cells + (cell -> distance)
    this
  }

  /**
   * 找到从起点到目标点的路径
   *
   * @param goal 目标点
   * @return 包含路径的Distances
   */
  def pathTo(goal: Cell): Distances = {
    var curCell = goal
    // 记录路径用
    var breadcrumbs = Distances(root)
    breadcrumbs = breadcrumbs + (curCell, cells(curCell))
    // 从终点往回走，每次找距离减少的点，直到走到起点
    while (curCell != root) {
      for (neighbor <- curCell.links()) {
        if (cells(neighbor) < cells(curCell)) {
          breadcrumbs = breadcrumbs + (neighbor, cells(neighbor))
          curCell = neighbor
        }
      }
    }
    breadcrumbs
  }

  /**
   * 找到距离最远的单元格
   *
   * @return (距离最远的单元格, 距离)
   */
  def max(): (Cell, Int) = {
    var maxDist = 0
    var maxCell = root
    for ((cell, dist) <- cells) {
      if (dist > maxDist) {
        maxDist = dist
        maxCell = cell
      }
    }
    (maxCell, maxDist)
  }
}
