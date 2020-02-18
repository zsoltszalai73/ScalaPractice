package hu.sp.week1

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.util.Random

case class Fold(var nextKey: Int, var cMap: Map[Coordinate, Int], var pathList: ListBuffer[Coordinate], var wallList: ListBuffer[Coordinate])

// using fold
object MazeGeneratorFour extends MazeGenerator {

  override def generate(width: Int, height: Int): Maze = {
    val walls =
      for {x <- 0 until width
           y <- 0 until height
           if (x % 2 == 0 && y % 2 == 0) || x == 0 || x == width - 1 || y == 0 || y == height -1
           } yield Coordinate(x, y)
    val path =
      for {x <- 1 until width - 1
           y <- 1 until height - 1
           if x % 2 == 1 && y % 2 == 1
           } yield Coordinate(x, y)

    val toDecide = Random.shuffle(for {x <- 1 until width - 1
                        y <- 1 until height - 1
                        if x % 2 + y % 2 == 1
                        } yield Coordinate(x, y))

    val foldResult = toDecide.tail.foldLeft(Fold(0, Map.empty, ListBuffer.empty, ListBuffer.empty))((m, c) => {
      val neighbours = c.getPassagewayNeighbours
      val val0 = m.cMap.get(neighbours(0))
      val val1 = m.cMap.get(neighbours(1))

      (val0, val1) match {
        case (None, Some(i)) => {
          m.cMap += neighbours(0) -> i
          m.pathList += c
        }
        case (Some(i), None) => {
          m.cMap += neighbours(1) -> i
          m.pathList += c
        }
        case (Some(i1), Some(i2)) if i1 == i2 => {
          m.wallList += c
        }
        case (Some(i1), Some(i2)) if i1 != i2 => {
          m.pathList += c
          val updated = m.cMap.filter(t => t._2 == i1).map(t => t._1 -> i2)
          m.cMap ++= updated
        }
        case (None, None) => {
          m.cMap += (neighbours(0) -> m.nextKey, neighbours(1) -> m.nextKey)
          m.nextKey += 1
          m.pathList += c
        }
      }
      m
    })

    val mapWall = walls.map(_ -> Tile.WALL) toMap
    val mapWallFold = foldResult.wallList.map(_ -> Tile.WALL)
    val mapPath = path.map(_ -> Tile.PATH) toMap
    val mapPathFold = foldResult.pathList.map(_ -> Tile.PATH)

    //val mapToDecide = toDecide.map(_ -> Tile.UNDEFINED) toMap

    Maze(mapWall ++ mapPath ++ mapWallFold ++ mapPathFold, width, height)

  }


}
