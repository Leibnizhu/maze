package com.github.leibnizhu.maze.cell

class HexCell(override val row: Int, override val column: Int) extends Cell(row, column) {
  var north: Option[HexCell] = None
  var northEast: Option[HexCell] = None
  var northWest: Option[HexCell] = None
  var south: Option[HexCell] = None
  var southEast: Option[HexCell] = None
  var southWest: Option[HexCell] = None

  override def neighbors(): List[HexCell] = List(north, northEast, northWest, south, southEast, southWest).flatten
}
