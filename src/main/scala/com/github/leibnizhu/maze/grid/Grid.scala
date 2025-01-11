package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.Cell
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text}

import scala.util.Random

trait Grid(val rows: Int, val columns: Int) {

  protected val _grid: Array[Array[Cell]] = initGrid()

  private def initGrid(): Array[Array[Cell]] = {
    // 初始化所有单元格
    val grid = initializeCells()
    // 链接单元格
    linkCells(grid)
    grid
  }

  protected def initializeCells(): Array[Array[Cell]]

  protected def linkCells(grid: Array[Array[Cell]]): Unit

  def randomCell(): Cell = {
    // 随机选择一个单元格
    val row = Random.nextInt(rows)
    val column = Random.nextInt(columns)
    _grid(row)(column)
  }

  def cell(row: Int, column: Int, grid: Array[Array[Cell]] = _grid): Cell =
    if (row >= 0 && row < rows && column >= 0 && column < columns)
      grid(row)(column)
    else
      null

  def size(): Int = _grid.map(_.length).sum

  def centerCell(): Cell = cell(rows / 2, columns / 2)

  /** 遍历所有行
   *
   * @param f
   * 对每一行执行的函数
   */
  def eachRow()(f: (Int, Array[Cell]) => Unit): Unit = {
    for ((row, index) <- _grid.zipWithIndex) {
      f(index, row)
    }
  }

  /** 遍历所有单元格
   *
   * @param f
   * 对每一个单元格执行的函数
   */
  def eachCell(grid: Array[Array[Cell]] = _grid)(f: Cell => Unit): Unit = {
    for (row <- grid) {
      for (cell <- row) {
        f(cell)
      }
    }
  }

  def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances] = None): Unit
}


