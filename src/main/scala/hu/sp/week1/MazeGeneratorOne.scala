package hu.sp.week1

import scala.util.Random

object MazeGeneratorOne extends MazeGenerator {

  override def generate(width: Int, height: Int) = {
    var path = Set[Coordinate]()
    var border = Set(Coordinate(Random.nextInt(width / 2) * 2 + 1, Random.nextInt(height / 2) * 2 + 1))
    //var border = Set(Coordinate(1,1))

    def getTwoNeighboursInAllDirections(coordinate: Coordinate) = {
      List(Coordinate(0, 1), Coordinate(0, -1), Coordinate(1, 0), Coordinate(-1, 0)).map(c => (coordinate + c, coordinate + (c * 2)))
    }

    def filterAvailableNeighbours(t: (Coordinate, Coordinate)) = {
      isValidCoordinate(t._1) && isValidCoordinate(t._2) && isAvailableCoordinate(t._1) && isAvailableCoordinate(t._2)
    }

    def isValidCoordinate(c: Coordinate) = {
      c.x >= 0 && c.x < width && c.y >= 0 && c.y < height
    }

    def isAvailableCoordinate(c: Coordinate) = {
      !path.contains(c) && !border.contains(c)
    }

    def oneIteration = {
      val c = border.head
      val next = getTwoNeighboursInAllDirections(c).filter(filterAvailableNeighbours(_))
      next.size match {
        case 0 => {
          border = border - c
          path = path + c
        }
        case _ => {
          path = path + next.head._1
          border = border + next.head._2
        }
      }
    }

    def buildMazeMap = {
      val allTiles = (for (x <- 0 until width; y <- 0 until height) yield Coordinate(x, y)).toList
      val mazeMap = allTiles.map(c => path.contains(c) match {
        case true => c -> Tile.PATH
        case false => c -> Tile.WALL
      }).toMap
      Maze(mazeMap, width, height)
    }


    while (border.nonEmpty) oneIteration
    path = path union Set(Coordinate(1, 0), Coordinate(width - 2, height - 1))
    buildMazeMap
  }
}
