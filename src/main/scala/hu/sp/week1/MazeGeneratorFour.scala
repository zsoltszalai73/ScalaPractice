package hu.sp.week1

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.util.Random

case class FoldHelper(var nextKey: Int, var cMap: Map[Coordinate, Int], var pathList: ListBuffer[Coordinate])

// using fold
object MazeGeneratorFour extends MazeGenerator {

  def generate(width: Int, height: Int): Maze = {
    val tilesToDecide = Random.shuffle(for (x <- 1 until width - 1; y <- 1 until height - 1 if x % 2 + y % 2 == 1) yield Coordinate(x, y))
    val foldResult = tilesToDecide.foldLeft(FoldHelper(0, Map.empty, ListBuffer.empty))(processTiles)
    val allCoords = for (x <- 0 until width; y <- 0 until height) yield Coordinate(x, y)
    val mazeMap = allCoords.map(c => sortTiles(c, foldResult.pathList toList)).toMap +
      (Coordinate(1,0) -> Tile.PATH, Coordinate(width - 2, height - 1) -> Tile.PATH) // add enter and exit
    Maze(mazeMap, width, height)
  }

  private def processTiles(m: FoldHelper, c: Coordinate) = {
    val neighbours = getPassagewayNeighbours(c)
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
      case _ => None
    }
    m
  }

  private def getPassagewayNeighbours(c: Coordinate) = {
    val leftRight = c.x % 2 == 0 && c.y % 2 == 1
    val upDown = c.x % 2 == 1 && c.y % 2 == 0
    assert(!upDown || !leftRight, s"Not a passageway coordinate! c.x=$c.x c.y=$c.y")
    upDown match {
      case true => List(Coordinate(c.x, c.y - 1), Coordinate(c.x, c.y + 1))
      case false => List(Coordinate(c.x - 1, c.y), Coordinate(c.x + 1, c.y))
    }
  }

  def sortTiles(c: Coordinate, pathList: List[Coordinate]) = {
    c match {
      case c if pathList.contains(c) => c -> Tile.PATH            // decided as path
      case c if (c.x % 2 == 1 && c.y % 2 == 1) => c -> Tile.PATH  // path for sure
      case _ => c -> Tile.WALL                                    // the rest is wall
    }
  }

}
