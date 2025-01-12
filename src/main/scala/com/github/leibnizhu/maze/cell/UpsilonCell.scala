package com.github.leibnizhu.maze.cell

class UpsilonCell(override val row: Int, override val column: Int) extends Cell(row, column) {
  var north: Option[UpsilonCell] = None
  var south: Option[UpsilonCell] = None
  var east: Option[UpsilonCell] = None
  var west: Option[UpsilonCell] = None
  var northWest: Option[UpsilonCell] = None
  var northEast: Option[UpsilonCell] = None
  var southWest: Option[UpsilonCell] = None
  var southEast: Option[UpsilonCell] = None


  override def neighbors(): List[UpsilonCell] = List(north, south, east, west, northWest, northEast, southWest, southEast).flatten

  def isOctagon(): Boolean = (row + column) % 2 == 0
}
