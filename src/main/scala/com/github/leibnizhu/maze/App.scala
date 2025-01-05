package com.github.leibnizhu.maze

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{BorderPane, FlowPane}

import scala.util.Try

object App extends JFXApp3 {

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "迷宫"
      scene = new Scene(800, 450) {
        root = new BorderPane() {
          private var grid: Grid = _
          private var curDist: Distances = _
          private val cellSize = 40
          private val canvasWidth = 800
          private val canvasHeight = 400
          private var canvasClick = 0
          private val rowNumInput: TextField = new TextField {
            promptText = "迷宫行数"
          }
          private val columnNumInput: TextField = new TextField {
            promptText = "迷宫列数"
          }

          private def getRowColumn: (Int, Int) = {
            (Try(rowNumInput.text.value.toInt).getOrElse(10), Try(columnNumInput.text.value.toInt).getOrElse(10))
          }

          private val centerCanvas = new Canvas(canvasWidth, canvasHeight) {
            onMouseClicked = event => {
              val (rows, columns) = getRowColumn
              val (x, y) = (event.getX, event.getY)
              if (grid != null && x <= columns * cellSize && y <= rows * cellSize) {
                val (row, column) = ((y / cellSize).toInt, (x / cellSize).toInt)
                println(f"点击格子: ($row, $column)")
                canvasClick += 1
                canvasClick match
                  case 1 =>
                    // 第一次点击，选择了起点
                    val cell = grid.cell(row, column)
                    curDist = cell.distances()
                    graphicsContext2D.clearRect(0, 0, canvasWidth, canvasHeight)
                    grid.paintCanvas(graphicsContext2D, cellSize, Some(curDist))
                  case 2 =>
                    // 第二次点击，选择了终点
                    val goal = grid.cell(row, column)
                    val path = curDist.pathTo(goal)
                    graphicsContext2D.clearRect(0, 0, canvasWidth, canvasHeight)
                    grid.paintCanvas(graphicsContext2D, cellSize, Some(path))
                    canvasClick = 0
                    curDist = null
                  case _ =>
                    canvasClick = 0
                    curDist = null
                    graphicsContext2D.clearRect(0, 0, canvasWidth, canvasHeight)
                    grid.paintCanvas(graphicsContext2D, cellSize)
              } else {
                println(f"迷宫以外的点击座标: ($x, $y)")
              }
            }
          }
          top = new FlowPane() {
            private val binaryTreeButton = new Button("二叉树算法") {
              onAction = _ => {
                // 生成Binary Tree迷宫
                val (rows, columns) = getRowColumn
                grid = new Grid(rows, columns)
                BinaryTree.on(grid)
                // 将迷宫绘制到画布上
                val gc: GraphicsContext = centerCanvas.graphicsContext2D
                gc.clearRect(0, 0, canvasWidth, canvasHeight)
                grid.paintCanvas(gc, cellSize)
              }
            }
            private val sidewinderButton = new Button("Sidewinder算法") {
              onAction = _ => {
                // 生成Sidewinder迷宫
                val (rows, columns) = getRowColumn
                grid = new Grid(rows, columns)
                Sidewinder.on(grid)
                // 将迷宫绘制到画布上
                val gc: GraphicsContext = centerCanvas.graphicsContext2D
                gc.clearRect(0, 0, canvasWidth, canvasHeight)
                grid.paintCanvas(gc, cellSize)
              }
            }
            private val longestPath = new Button("最长路径") {
              onAction = _ => {
                val start = grid.cell(0, 0)
                val distances = start.distances()
                val (newStart, maxDist) = distances.max()
                val newDistances = newStart.distances()
                val (goal, _) = newDistances.max()
                val path = newDistances.pathTo(goal)
                val gc: GraphicsContext = centerCanvas.graphicsContext2D
                gc.clearRect(0, 0, canvasWidth, canvasHeight)
                grid.paintCanvas(gc, cellSize, Some(path))
              }
            }
            hgap = 10
            children = List(
              rowNumInput,
              columnNumInput,
              binaryTreeButton,
              sidewinderButton,
              longestPath
            )
          }
          center = centerCanvas
        }
      }
    }
  }

}
