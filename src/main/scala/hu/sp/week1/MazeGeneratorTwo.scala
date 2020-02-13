package hu.sp.week1

import scala.util.Random

class MazeGeneratorTwo extends MazeGenerator {

  def generate(width: Int, height: Int) = {

    val (pT, cT) = Random.shuffle(for (x <- 1 to width - 2 by 2; y <- 1 to height - 2 by 2) yield Coordinate(x, y)).splitAt(1)
    var pathTiles = pT.toSet
    val coreTiles = cT.toSet

    def getCoreNeighbours(c: Coordinate) = {
      Set(Coordinate(0, 2), Coordinate(0, -2), Coordinate(-2, 0), Coordinate(2, 0)).map(c + _).filter(isValidCoordinate(_))
    }

    def isValidCoordinate(c: Coordinate) = {
      c.x >= 0 && c.x < width && c.y >= 0 && c.y < height
    }

    def iteration = {
      coreTiles.find(c => !pathTiles.contains(c) && getCoreNeighbours(c).intersect(pathTiles).nonEmpty) match {
        case Some(c) => {
          val n = getCoreNeighbours(c).intersect(pathTiles).head
          pathTiles ++= Set(c, Coordinate((n.x + c.x) / 2, (n.y + c.y) / 2))
          true
        }
        case _ => false
      }
    }

    def buildMazeMap = {
      val allTiles = (for (x <- 0 until width; y <- 0 until height) yield Coordinate(x, y)).toList
      val mazeMap = allTiles.map(c => pathTiles.contains(c) match {
        case true => c -> Tile.PATH
        case false => c -> Tile.WALL
      }).toMap
      Maze(mazeMap, width, height)
    }

    while (iteration) None
    buildMazeMap
  }


}
