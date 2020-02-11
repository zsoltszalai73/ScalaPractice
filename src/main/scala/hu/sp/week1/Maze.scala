package hu.sp.week1

import hu.sp.week1.Tile.Tile
import scala.util.Random

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

  def generateMaze(width: Int, height: Int) = {

    var path = Set[Coordinate]()
    var border = Set[Coordinate](Coordinate(Random.nextInt(width / 2) * 2 + 1, Random.nextInt(height / 2) * 2 + 1))

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

  println("Maze generator")
  println("Enter width and height")
  print("width=")
  val width = io.StdIn.readInt()
  print("height=")
  val height = io.StdIn.readInt()
  println(s"width=$width height=$height")

  assert(width > 2 && width % 2 == 1 && height > 2 && height % 2 == 1, "Width and height have to be uneven number, greater than 2!")

  val m = Maze.generateMaze(width, height)
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
