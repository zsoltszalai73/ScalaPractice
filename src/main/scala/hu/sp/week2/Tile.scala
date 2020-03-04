package hu.sp.week2

import hu.sp.week2.TileType.TileType

case class Tile(tileType: TileType) {

  override def toString: String = tileType.toString
}
