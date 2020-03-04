package hu.sp.week2

import scala.collection.mutable

case class Maze(tileMap: Map[Coordinate, Tile], width: Int, height: Int, entry: Coordinate, exit: Coordinate) {

  def asString(path: List[Coordinate] = List[Coordinate]()) = {
    val sb = new StringBuilder
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val coordinate = Coordinate(x, y)
        val color = path.contains(coordinate) match {
          case true => Console.RED_B
          case false => Console.WHITE_B
        }


        tileMap.get(coordinate) match {
          case None => sb ++= "  "
          case Some(t) => sb ++= t.toStringWithDistance(exit, color)
        }
      }
      sb ++= "\n"
    }
    sb.toString()
  }

  def asStringWithPath(path: List[Coordinate]) = {
    val sb = new StringBuilder
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        tileMap.get(Coordinate(x, y)) match {
          case None => sb ++= "  "
          case Some(t) => sb ++= t.toStringWithDistance(exit)
        }
      }
      sb ++= "\n"
    }
    sb.toString()
  }

  def addDistanceMap(baseCoordinate: Coordinate) = {

    def getNotProcessedPathNeighbours(coordinate: Coordinate) = {
      getPathNeighbours(coordinate).filter(tileMap(_).distanceMap.get(baseCoordinate).isEmpty)
    }

    var distance = 0
    //    tileMap(baseCoordinate).distanceMap.put(baseCoordinate, distance)
    var tilesToFill = mutable.Set(baseCoordinate)
    while (tilesToFill.nonEmpty) {
      tilesToFill = tilesToFill.flatMap(c => {
        tileMap(c).distanceMap.put(baseCoordinate, distance)
        getNotProcessedPathNeighbours(c)
      })
      distance += 1
      //      println(s"distance = $distance tilesToFill = $tilesToFill")
    }
  }

  def getPathNeighbours(coordinate: Coordinate): List[Coordinate] = {
    coordinate.getNeighbours.
      filter(tileMap.keySet.contains(_)).
      filter(tileMap(_).tileType match {
        case TileType.PATH | TileType.PATH_WITH_DIAMOND => true
        case _ => false
      })
  }

  def findPathByDMapKey(start: Coordinate, targetKey: Coordinate) = {
    val path = mutable.ListBuffer(start)
    var next = start
    for (dist <- tileMap(start).distanceMap(targetKey) - 1 to 0 by -1) {
      next = getPathNeighbours(next).find(tileMap(_).distanceMap(targetKey) == dist).get
      path += next
    }
    path.toList
  }

}

