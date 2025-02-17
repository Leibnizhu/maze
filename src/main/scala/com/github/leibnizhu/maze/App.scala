package com.github.leibnizhu.maze

import com.github.leibnizhu.maze.generate.*
import com.github.leibnizhu.maze.grid.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.{Button, ComboBox, TextField}
import scalafx.scene.layout.{BorderPane, FlowPane}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import java.io.File
import scala.util.Try

object App extends JFXApp3 {
  private val CANVAS_WIDTH = 800
  private val CANVAS_HEIGHT = 600
  private val DEFAULT_ROW_NUM = 10
  private val DEFAULT_COLUMN_NUM = 10

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "迷宫"
      scene = new Scene(CANVAS_WIDTH, CANVAS_HEIGHT + 50) {
        root = new BorderPane() {
          private var grid: Grid = _
          private var mask: Mask = _
          private var curDist: Distances = _
          private var cellSize = Grid.MAX_CELL_SIZE
          private val canvasWidth = CANVAS_WIDTH
          private val canvasHeight = CANVAS_HEIGHT
          private var canvasClick = 0

          private val rowNumInput: TextField = new TextField {
            promptText = "迷宫行数"
            prefWidth = 75
          }
          private val columnNumInput: TextField = new TextField {
            promptText = "迷宫列数"
            prefWidth = 75
          }
          private val algorithmSelector = new ComboBox(List("递归回溯算法", "猎杀算法", "Wilson算法", "Aldous-Border算法", "Sidewinder算法", "二叉树算法")) {
            this.getSelectionModel.selectFirst()
          }
          private val shapeSelector = new ComboBox(List("方形", "方形遮罩", "圆形", "六边形", "三角形", "经典马赛克", "交织型")) {
            this.getSelectionModel.selectFirst()

            onAction = _ => {
              if ("方形遮罩" == this.value.value) {
                val fileChooser = new FileChooser() {
                  title = "选择遮罩文件"
                  initialDirectory = new File(System.getProperty("user.home"))
                  extensionFilters.addAll(new FileChooser.ExtensionFilter("PNG Files", "*.png"))
                }
                val maskFile = fileChooser.showOpenDialog(stage)
                if (maskFile != null) {
                  mask = Mask(maskFile)
                  rowNumInput.setText(mask.rows.toString)
                  columnNumInput.setText(mask.columns.toString)
                }
              }
            }
          }

          private def getRowColumn: (Int, Int) = {
            (Try(rowNumInput.text.value.toInt).getOrElse(DEFAULT_ROW_NUM),
              Try(columnNumInput.text.value.toInt).getOrElse(DEFAULT_COLUMN_NUM))
          }

          private val centerCanvas = new Canvas(canvasWidth, canvasHeight) {
            onMouseClicked = event => doCanvasClick(event.getX, event.getY, graphicsContext2D)
          }

          private def doCanvasClick(x: Double, y: Double, gc: GraphicsContext): Unit = {
            if (shapeSelector.value.value == "方形" || shapeSelector.value.value == "方形遮罩") {
              val (rows, columns) = getRowColumn
              if (grid != null && x <= columns * cellSize && y <= rows * cellSize) {
                val (row, column) = ((y / cellSize).toInt, (x / cellSize).toInt)
                println(f"点击格子: ($row, $column)")
                canvasClick += 1
                canvasClick match
                  case 1 =>
                    // 第一次点击，选择了起点
                    val cell = grid.cell(row, column)
                    curDist = Distances(cell)
                    gc.clearRect(0, 0, canvasWidth, canvasHeight)
                    grid.paintCanvas(gc, cellSize, Some(curDist))
                  case 2 =>
                    // 第二次点击，选择了终点
                    val goal = grid.cell(row, column)
                    val path = curDist.pathTo(goal)
                    gc.clearRect(0, 0, canvasWidth, canvasHeight)
                    grid.paintCanvas(gc, cellSize, Some(path))
                    canvasClick = 0
                    curDist = null
                  case _ =>
                    canvasClick = 0
                    curDist = null
                    gc.clearRect(0, 0, canvasWidth, canvasHeight)
                    grid.paintCanvas(gc, cellSize)
              } else {
                println(f"迷宫以外的点击座标: ($x, $y)")
              }
            }
          }

          private def genMaze(): Unit = {
            // 生成Binary Tree迷宫
            val (rows, columns) = shapeSelector.value.value match {
              case "方形" =>
                val shape = getRowColumn
                grid = new MatrixGrid(shape._1, shape._2)
                shape
              case "方形遮罩" =>
                grid = new MaskedGrid(mask)
                (mask.rows, mask.columns)
              case "圆形" =>
                val shape = getRowColumn
                grid = new PolarGrid(shape._1)
                shape
              case "六边形" =>
                val shape = getRowColumn
                grid = new HexGrid(shape._1, shape._2)
                shape
              case "三角形" =>
                val shape = getRowColumn
                grid = new TriangleGrid(shape._1, shape._2)
                shape
              case "经典马赛克" =>
                val shape = getRowColumn
                // 保持长宽是奇数
                val rows = shape._1 / 2 * 2 + 1
                val columns = shape._2 / 2 * 2 + 1
                grid = new UpsilonGrid(rows, columns)
                (rows, columns)
              case "交织型" =>
                val shape = getRowColumn
                grid = new WeaveGrid(shape._1, shape._2)
                shape
            }
            cellSize = grid.cellSize(centerCanvas.getWidth - 2, centerCanvas.getHeight - 2)
            val algorithm = algorithmSelector.value.value
            algorithm match {
              case "二叉树算法" => BinaryTree.on(grid)
              case "Sidewinder算法" => Sidewinder.on(grid)
              case "Aldous-Border算法" => AldousBorder.on(grid)
              case "Wilson算法" => Wilson.on(grid)
              case "猎杀算法" => HuntAndKill.on(grid)
              case "递归回溯算法" => RecursiveBacktracker.on(grid)
              case _ => println("未支持的算法")
            }
            // 将迷宫绘制到画布上
            val gc = centerCanvas.graphicsContext2D
            gc.clearRect(0, 0, canvasWidth, canvasHeight)
            val middle = grid.centerCell()
            grid.braid(0.3)
            grid.paintCanvas(gc, cellSize, Option(middle).map(_.distances()))
            canvasClick = 0
            curDist = null
          }

          private def genLongestPath(): Unit = {
            if (grid != null) {
              val path = grid.longestPath()
              val gc = centerCanvas.graphicsContext2D
              gc.clearRect(0, 0, canvasWidth, canvasHeight)
              grid.paintCanvas(gc, cellSize, Some(path))
              canvasClick = 0
              curDist = null
            }
          }

          private def genPlay(): Unit = {
            genMaze()
            val path = grid.longestPath()
            val gc = centerCanvas.graphicsContext2D
            gc.clearRect(0, 0, canvasWidth, canvasHeight)
            grid.paintCanvas(gc, cellSize, distances = Some(path), playMode = true)
            canvasClick = 0
            curDist = null
          }

          top = new FlowPane() {
            private val genMazeButton = new Button("随机生成迷宫") {
              onAction = _ => genMaze()
            }
            private val longestPath = new Button("最长路径") {
              onAction = _ => genLongestPath()
            }
            private val play = new Button("游玩模式") {
              onAction = _ => genPlay()
            }
            hgap = 10
            children = List(
              rowNumInput,
              columnNumInput,
              shapeSelector,
              algorithmSelector,
              genMazeButton,
              longestPath,
              play,
            )
          }
          center = centerCanvas
        }
      }
    }
  }

}

