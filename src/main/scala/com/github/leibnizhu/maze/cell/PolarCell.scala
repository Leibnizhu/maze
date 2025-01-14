package com.github.leibnizhu.maze.cell

class PolarCell(override val row: Int, override val column: Int) extends Cell {
  // 顺时针方向的邻居
  var cw: Option[PolarCell] = None
  // 逆时针方向的邻居
  var ccw: Option[PolarCell] = None
  // 向内（圆心）的邻居
  var inward: Option[PolarCell] = None
  // 向外的邻居，可能有多个
  var outward: List[PolarCell] = List()

  override def neighbors(): List[PolarCell] = List(cw, ccw, inward).flatten ++ outward

  override def isEdge: Boolean = outward.isEmpty
}
