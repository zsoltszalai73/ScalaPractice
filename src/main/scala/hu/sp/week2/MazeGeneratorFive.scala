package hu.sp.week2

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.util.Random

case class FoldHelper(var nextKey: Int, var cMap: Map[Coordinate, Int], var pathList: ListBuffer[Coordinate])

// using fold
object MazeGeneratorFive extends MazeGenerator {

  def generate(width: Int, height: Int, numOfPaths: Int): Maze = {
    val tilesToDecide = Random.shuffle(for (x <- 1 until width - 1; y <- 1 until height - 1 if x % 2 + y % 2 == 1) yield Coordinate(x, y))
    val (extraPath, remaining) = tilesToDecide.splitAt(numOfPaths - 1)
    val foldResult = remaining.foldLeft(FoldHelper(0, Map.empty, ListBuffer.empty))(processTiles)
    val pathList = foldResult.pathList.toList
    val allCoords = for (x <- 0 until width; y <- 0 until height) yield Coordinate(x, y)
    val entry = Coordinate(1, 0)
    val exit = Coordinate(width - 2, height - 1)
    val mazeMap = allCoords.map(c => categorize(c, pathList ++ extraPath)).toMap

    val tilesWithDiamonds = Random.shuffle(mazeMap.filter(_._2.tileType == TileType.PATH)).take(numOfPaths).map(t => t._1 -> Tile(TileType.PATH_WITH_DIAMOND, t._2.distanceMap))
    println(tilesWithDiamonds)

    val mapWithEntryExit = mazeMap ++ tilesWithDiamonds + (entry -> Tile(TileType.PATH), exit -> Tile(TileType.PATH)) // add enter and exit

    Maze(mapWithEntryExit, width, height, entry, exit, tilesWithDiamonds.keySet.toList)
  }

  private def processTiles(m: FoldHelper, c: Coordinate) = {
    val (n1, n2) = getPassagewayNeighbours(c)
    val (val1, val2) = (m.cMap.get(n1), m.cMap.get(n2))

    (val1, val2) match {
      case (None, Some(i)) => {
        m.cMap += n1 -> i
        m.pathList += c
      }
      case (Some(i), None) => {
        m.cMap += n2 -> i
        m.pathList += c
      }
      case (Some(i1), Some(i2)) if i1 != i2 => {
        m.pathList += c
        val updated = m.cMap.filter(t => t._2 == i1).map(t => t._1 -> i2)
        m.cMap ++= updated
      }
      case (None, None) => {
        m.cMap += (n1 -> m.nextKey, n2 -> m.nextKey)
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
      case true => (Coordinate(c.x, c.y - 1), Coordinate(c.x, c.y + 1))
      case false => (Coordinate(c.x - 1, c.y), Coordinate(c.x + 1, c.y))
    }
  }

  def categorize(c: Coordinate, pathList: List[Coordinate]) = {
    c match {
      case c if pathList.contains(c) => c -> Tile(TileType.PATH)            // decided as path
      case c if (c.x % 2 == 1 && c.y % 2 == 1) => c -> Tile(TileType.PATH)  // path for sure
      case _ => c -> Tile(TileType.WALL)                                    // the rest is wall
    }
  }

}
