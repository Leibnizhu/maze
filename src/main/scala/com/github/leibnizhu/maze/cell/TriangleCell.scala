package com.github.leibnizhu.maze.cell

class TriangleCell(override val row: Int, override val column: Int) extends Cell {
  var north: Option[TriangleCell] = None
  var south: Option[TriangleCell] = None
  var east: Option[TriangleCell] = None
  var west: Option[TriangleCell] = None

  override def neighbors(): List[TriangleCell] = List(north, south, east, west).flatten

  /**
   * @return 三角形是否是尖尖朝上的
   */
  def upright(): Boolean = (row + column) % 2 == 0

  override def isEdge: Boolean = List(if (upright()) south else north, east, west).exists(_.isEmpty)
}
