package com.github.leibnizhu.maze.cell

import com.github.leibnizhu.maze.grid.WeaveGrid

class UnderCell(val overCell: OverCell) extends OverCell(overCell.row, overCell.column, overCell.grid) {

  if(overCell.horizontalPassage()){
    north = overCell.north
    overCell.north.foreach(_.south = Some(this))
    south = overCell.south
    overCell.south.foreach(_.north = Some(this))
    north.foreach(link(_))
    south.foreach(link(_))
  } else {
    east = overCell.east
    overCell.east.foreach(_.west = Some(this))
    west = overCell.west
    overCell.west.foreach(_.east = Some(this))
    east.foreach(link(_))
    west.foreach(link(_))
  }

  override def horizontalPassage(): Boolean = east.isDefined || west.isDefined

  override def verticalPassage(): Boolean = north.isDefined || south.isDefined
}
