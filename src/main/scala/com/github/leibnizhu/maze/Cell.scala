package com.github.leibnizhu.maze

class Cell(row: Int, column: Int) {
  var north: Option[Cell] = None
  var south: Option[Cell] = None
  var east: Option[Cell] = None
  var west: Option[Cell] = None
  private var _links: Map[Cell, Boolean] = Map.empty

  /**
    * 链接另一个单元格
    *
    * @param another 另一个单元格
    * @param bidi 是否双向链接
    * @return 当前单元格
    */
  def link(another: Cell, bidi: Boolean = true): Cell = {
    _links = _links + (another -> true)
    if (bidi) {
      another.link(this, false)
    }
    this
  }

  /**
    * 取消链接另一个单元格
    *
    * @param another 另一个单元格
    * @param bidi 是否双向取消链接
    * @return 当前单元格
    */
  def unlink(another: Cell, bidi: Boolean = true): Cell = {
    _links = _links - another
    if (bidi) {
      another.unlink(this, false)
    }
    this
  }

  def links(): List[Cell] = _links.keys.toList

  def linked(another: Option[Cell]): Boolean = another.exists(_links.getOrElse(_, false))

  def linked(another: Cell): Boolean = _links.getOrElse(another, false)

  /**
    * 获取所有相邻的单元格
    *
    * @return 相邻的单元格列表
    */
  def neighbors(): List[Cell] = List(north, south, east, west).flatten

  def distances(): Distances = Distances(this).distances()
}
