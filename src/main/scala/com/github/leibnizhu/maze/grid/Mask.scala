package com.github.leibnizhu.maze.grid

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.util.Random

class Mask(val rows: Int, val columns: Int) {
  private val bits: Array[Array[Boolean]] = Array.fill(rows, columns)(true)

  def isEnabled(row: Int, column: Int): Boolean =
    if (row >= 0 && row < rows && column >= 0 && column < columns) {
      bits(row)(column)
    } else {
      false
    }

  def set(row: Int, column: Int, value: Boolean): Unit = {
    if (row >= 0 && row < rows && column >= 0 && column < columns) {
      bits(row)(column) = value
    }
  }

  def disable(row: Int, column: Int): Unit = set(row, column, false)

  def size(): Int = bits.map(row => row.count(b => b)).sum

  def randomLocation(): (Int, Int) = {
    var row, column = 0
    while {
      row = Random.nextInt(rows)
      column = Random.nextInt(columns)
      !isEnabled(row, column)
    } do ()
    (row, column)
  }
}

object Mask {
  def apply(pngFile: File): Mask = {
    val image = ImageIO.read(pngFile)
    val width = image.getWidth
    val height = image.getHeight
    val mask = new Mask(height, width)

    for (x <- 0 until width) {
      for (y <- 0 until height) {
        // 获取像素的ARGB值
        val pixel = image.getRGB(x, y)
        val isBlack = (pixel & 0xFFFFFF) == 0
        if (isBlack) {
          mask.disable(x, y)
        }
      }
    }
    mask
  }
}
