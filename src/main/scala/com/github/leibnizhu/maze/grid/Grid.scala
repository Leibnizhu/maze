package com.github.leibnizhu.maze.grid

import com.github.leibnizhu.maze.Distances
import com.github.leibnizhu.maze.cell.Cell
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.text.{Font, Text}

import scala.util.Random

trait Grid() {
  val rows: Int
  val columns: Int

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

  def randomEdge(): Cell = {
    var cell: Cell = null
    while {
      cell = randomCell()
      !cell.isEdge
    } do ()
    cell
  }

  def entryAndExit(path: Distances): (Cell, Cell) = {
    val (entry, exit) = path.entryAndExit()
    (entry.getOrElse(randomEdge()), exit.getOrElse(randomEdge()))
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

  def paintCanvas(gc: GraphicsContext, cellSize: Int, distances: Option[Distances] = None, playMode: Boolean = false): Unit

  def textSize(text: String, font: Font): (Double, Double) = {
    val textNode = new Text(text)
    textNode.setFont(font)
    (textNode.getLayoutBounds.getWidth, textNode.getLayoutBounds.getHeight)
  }

  def cellSize(canvasWidth: Double, canvasHeight: Double): Int

  def longestPath(): Distances = {
    val start = randomCell()
    val distances = start.distances()
    val (newStart, maxDist) = distances.max()
    val newDistances = newStart.distances()
    val (goal, _) = newDistances.max()
    val path = newDistances.pathTo(goal)
    path
  }

  /**
   *
   * @return 所有死角格子的列表
   */
  def deadends(): List[Cell] = {
    var list = List[Cell]()
    eachCell() { cell => {
      if (cell.isDeadEnd) {
        list = cell :: list
      }
    }
    }
    list
  }

  /**
   * 编排迷宫
   *
   * @param p 剔除死角的比例，1=全剔除，0=不剔除
   */
  def braid(p: Double = 1.0): Unit = {
    Random.shuffle(deadends()).foreach(cell => {
      // 是死角（可能前面遍历其他死角已经连了当前死角）且满足剔除概率条件
      if (cell.isDeadEnd && Random.nextDouble() <= p) {
        // 未连接到当前格子的邻居
        val neighbors = cell.neighbors().filter(c => !c.linked(cell))
        // 优先连接也是死角的未连接邻居
        val bestNeighbor = neighbors.filter(_.isDeadEnd)
        val neighbor = if (bestNeighbor.isEmpty) neighbors(Random.nextInt(neighbors.length))
        else bestNeighbor(Random.nextInt(bestNeighbor.length))
        cell.link(neighbor)
      }
    })
  }
}

object Grid {
  val MAX_CELL_SIZE = 30
  val MIN_CELL_SIZE = 10
}
