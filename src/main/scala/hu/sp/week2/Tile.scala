package hu.sp.week2

import hu.sp.week2.TileType.TileType

import scala.collection.mutable

case class Tile(tileType: TileType, distanceMap: mutable.Map[Coordinate, Int] = mutable.Map.empty) {

  override def toString: String = tileType.toString

  def toStringWithDistance(withDistanceKey: Coordinate) = {
    tileType match {
      case TileType.PATH => Console.WHITE_B + "%02d".format(distanceMap.get(withDistanceKey).get).takeRight(2) + Console.RESET
      case _ => toString
    }
  }
  def toStringWithDistance(withDistanceKey: Coordinate, pathColor: String ) = {
    tileType match {
      case TileType.PATH => pathColor + "%02d".format(distanceMap.get(withDistanceKey).get).takeRight(2) + Console.RESET
      case _ => toString
    }
  }
}
