package hu.sp.week1

import scala.util.Random

// recursive, does not work
object MazeGeneratorThree extends MazeGenerator {

  override def generate(width: Int, height: Int): Maze = {

    def rec(toProcess: List[Coordinate], toCheck: List[Coordinate], results: List[Coordinate]): List[Coordinate] = {
      val contains = toProcess.flatMap(c => {
        val n = getCoreNeighbours(c).intersect(toCheck).headOption
        n match {
          case None => None
          case Some(nc) => List(nc, Coordinate((c.x + nc.x)/2, (c.y + nc.y) / 2))
        }
      })
      val notContains = toProcess.filter(contains.contains(_))
      notContains.isEmpty match {
        case true => results
        case false => rec(notContains, toCheck ++ contains, results ++ contains)
      }
    }

    def getCoreNeighbours(c: Coordinate) = {
      List(Coordinate(0, 2), Coordinate(0, -2), Coordinate(-2, 0), Coordinate(2, 0)).map(c + _).filter(isValidCoordinate(_))
    }

    def isValidCoordinate(c: Coordinate) = {
      c.x >= 0 && c.x < width && c.y >= 0 && c.y < height
    }

    def buildMazeMap(pathTiles: List[Coordinate]) = {
      val allTiles = (for (x <- 0 until width; y <- 0 until height) yield Coordinate(x, y)).toList
      val mazeMap = allTiles.map(c => pathTiles.contains(c) match {
        case true => c -> Tile.PATH
        case false => c -> Tile.WALL
      }).toMap
      Maze(mazeMap, width, height)
    }

    val coreCoordinates = Random.shuffle(for (x <- 1 to width - 2 by 2; y <- 1 to height - 2 by 2) yield Coordinate(x, y))
    val toCheck = List(coreCoordinates.head)
    val toProcess = coreCoordinates.tail.toList
    val results = rec(toProcess, toCheck, List.empty)




    buildMazeMap(results)

  }
}
