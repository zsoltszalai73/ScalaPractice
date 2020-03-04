package hu.sp.week2

import scala.collection.mutable

case class Maze(tileMap: Map[Coordinate, Tile], width: Int, height: Int, entry: Coordinate, exit: Coordinate) {

  def asString = {
    val sb = new StringBuilder
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        tileMap.get(Coordinate(x, y)) match {
          case None => sb ++= "  "
          case Some(t) => sb ++= t.toStringWithDistance(entry)
        }
      }
      sb ++= "\n"
    }
    sb.toString()
  }

  def getPathNeighbours(coordinate: Coordinate): List[Coordinate] = {
    coordinate.getNeighbours.
      filter(tileMap.keySet.contains(_)).
      filter(tileMap(_).tileType match {
        case TileType.PATH | TileType.PATH_WITH_DIAMOND => true
        case _ => false
      })
  }

  def addDistanceMap(baseCoordinate: Coordinate) = {

    def getNotProcessedPathNeighbours(coordinate: Coordinate) = {
      getPathNeighbours(coordinate).filter(tileMap(_).distanceMap.get(baseCoordinate).isEmpty)
    }

    var distance = 0
    tileMap(baseCoordinate).distanceMap.put(baseCoordinate, distance)
    var tilesToFill = mutable.Set(getNotProcessedPathNeighbours(baseCoordinate).toSeq: _*)
    while (tilesToFill.nonEmpty) {
      distance += 1
      tilesToFill = tilesToFill.flatMap(c => {
        tileMap(c).distanceMap.put(baseCoordinate, distance)
        getNotProcessedPathNeighbours(c)
      })
      println(s"distance = $distance tilesToFill = $tilesToFill")
    }

  }

}

