package com.github.leibnizhu.maze.cell

import com.github.leibnizhu.maze.grid.WeaveGrid

class OverCell(override val row: Int, override val column: Int, val grid: WeaveGrid) extends Cell {
  var north: Option[OverCell] = None
  var south: Option[OverCell] = None
  var east: Option[OverCell] = None
  var west: Option[OverCell] = None


  override def isEdge: Boolean = List(north, south, east, west).exists(_.isEmpty)

  override def neighbors(): List[OverCell] = {
    List(
      north, south, east, west,
      if (canTunnelNorth) north.flatMap(_.north) else None,
      if (canTunnelSouth) south.flatMap(_.south) else None,
      if (canTunnelEast) east.flatMap(_.east) else None,
      if (canTunnelWest) west.flatMap(_.west) else None
    ).flatten
  }

  override def link(another: Cell, bidi: Boolean = true): Cell = another match {
    case anotherOver: OverCell =>
      // 检查当前格子与目标格子之间是否有共同邻居，如果有，那么需要一个UnderCell来连接
      val neighbor = if (north.isDefined && north == anotherOver.south) {
        north
      } else if (south.isDefined && south == anotherOver.north) {
        south
      } else if (east.isDefined && east == anotherOver.west) {
        east
      } else if (west.isDefined && west == anotherOver.east) {
        west
      } else {
        None
      }
      neighbor match {
        case Some(n) => grid.tunnelUnder(n)
        case None => super.link(another, bidi)
      }
      this
    case _ => this
  }

  /**
   * @return 是否是水平（东西）贯通的通道
   */
  def horizontalPassage(): Boolean =
    linked(east) && linked(west) && !linked(north) && !linked(south)

  /**
   * @return 是否是垂直（南北）贯通的通道
   */
  def verticalPassage(): Boolean =
    !linked(east) && !linked(west) && linked(north) && linked(south)

  /**
   * @return 是否可以跟北边连成上方隧道，要求北边紧邻的是水平通道
   */
  def canTunnelNorth: Boolean =
    north.filter(_.horizontalPassage()).flatMap(_.north).isDefined

  /**
   * @return 是否可以跟南边连成下方隧道，要求南边紧邻的是水平通道
   */
  def canTunnelSouth: Boolean =
    south.filter(_.horizontalPassage()).flatMap(_.south).isDefined

  /**
   * @return 是否可以跟东边连成下方隧道，要求东边紧邻的是垂直通道
   */
  def canTunnelEast: Boolean =
    east.filter(_.verticalPassage()).flatMap(_.east).isDefined

  /**
   * @return 是否可以跟西边连成下方隧道，要求西边紧邻的是垂直通道
   */
  def canTunnelWest: Boolean =
    west.filter(_.verticalPassage()).flatMap(_.west).isDefined
}
