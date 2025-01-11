package com.github.leibnizhu.maze.cell

class MatrixCell(override val row: Int, override val column: Int) extends Cell(row, column) {
  var north: Option[MatrixCell] = None
  var south: Option[MatrixCell] = None
  var east: Option[MatrixCell] = None
  var west: Option[MatrixCell] = None

  override def neighbors(): List[MatrixCell] = List(north, south, east, west).flatten
}
