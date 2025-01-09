package com.github.leibnizhu.maze.generate

import com.github.leibnizhu.maze.Grid

import scala.util.Random

object BinaryTree {

  /**
   * 构建二叉树迷宫
   * 这种二叉树，每一层都是对应一个迷宫节点，两个叉，分别代表往迷宫的东和北打通（链接）
   * 对于最北的一行，只能向东走；最东的一行，只能向北走
   *
   * @param grid 要构建的迷宫
   * @return 构建后的迷宫
   */
  def on(grid: Grid): Grid = {
    grid.eachCell() {  cell =>
      // 每个单元格，选择其北或东的邻居出来，随机选择一个，进行链接
      val neighbors = List(cell.east, cell.north).flatten
      if (neighbors.nonEmpty) {
        cell.link(neighbors(Random.nextInt(neighbors.size)))
      }
    }
    grid
  }

}
