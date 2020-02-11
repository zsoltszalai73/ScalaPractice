package hu.sp.week1

import hu.sp.week1.Tile.Tile

import scala.util.Random

object Tile extends Enumeration {
  type Tile = Value
  val WALL = Value("  ")
  val PATH = Value(Console.WHITE_B + "  " + Console.RESET)
}

case class Coordinate(x: Int, y: Int) {

  def getNeighbours = {
    List(Coordinate(x - 1, y), Coordinate(x + 1, y), Coordinate(x, y - 1), Coordinate(x, y + 1))
  }

  def +(c: Coordinate) = {
    Coordinate(x + c.x, y + c.y)
  }

  def *(i: Int) = {
    Coordinate(x * i, y * i)
  }
}

case class Maze(tileMap: Map[Coordinate, Tile], width: Int, height: Int) {

  def printMaze = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        tileMap.get(Coordinate(x, y)) match {
          case None => print("  ")
          case Some(t) => print(t.toString)
        }
      }
      println
    }
  }
}

object Maze {

  def generateMaze1(width: Int, height: Int) = {

    var path = Set[Coordinate]()
    var border = Set[Coordinate](Coordinate(Random.nextInt(width / 2) * 2 + 1, Random.nextInt(height / 2) * 2 + 1))

    def getFreeDoubleNeighbours(coordinate: Coordinate) = {
      val neighbours = List(Coordinate(0, 1), Coordinate(0, -1), Coordinate(1, 0), Coordinate(-1, 0)).map(c => (coordinate + c, coordinate + (c * 2)))
      val res = neighbours.filter(t => isValidCoordinate(t._1) && isValidCoordinate(t._2) && isFreeCoordinate(t._1) && isFreeCoordinate(t._2))
      res
    }

    def isValidCoordinate(c: Coordinate) = {
      c.x >= 0 && c.x < width && c.y >= 0 && c.y < height
    }

    def isFreeCoordinate(c: Coordinate) = {
      !path.contains(c) && !border.contains(c)
    }

    def oneIteration = {
      val c = border.head
      val next = getFreeDoubleNeighbours(c)
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

    while (border.nonEmpty) oneIteration

    path = path union Set(Coordinate(1, 0), Coordinate(width - 2, height - 1))

    val allTiles = (for (x <- 0 until width; y <- 0 until height) yield Coordinate(x, y)).toList
    val mazeMap = allTiles.map(c => path.contains(c) match {
      case true => c -> Tile.PATH
      case false => c -> Tile.WALL
    }).toMap
    Maze(mazeMap, width, height)
  }

}

object MazeApp extends App {

  val m = Maze.generateMaze1(77, 33)
  m.printMaze


  /*
    for (_ <- 1 to 10) {
      val t0 = System.nanoTime()
      Maze.generateMaze1(1001, 1001)
      val t1 = System.nanoTime()

      println("Elapsed time: " + (t1 - t0).toDouble / 1000000000 + " Sec")
    }
  */

}
