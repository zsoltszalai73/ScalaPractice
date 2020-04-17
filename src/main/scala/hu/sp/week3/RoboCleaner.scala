package hu.sp.week3

import java.util

import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color

import scala.collection.mutable.ListBuffer
import scala.util.Random

object RoboCleaner extends AnsiScreenPrinter {

  /**
   * Should return a path as a list
   */
  def cleanUpKnownSpace(room: Set[Coordinate], base: Coordinate, stepsWithOneCharge: Int) : List[Coordinate] = {
    List(base)
  }

  def visitRandomPoints(room: Set[Coordinate], base: Coordinate, stepsWithOneCharge: Int) = {
    val roomWithoutBase = (room - base).toList
    val points = List(base) ++ Random.shuffle(roomWithoutBase).take(5) ++ List(base)
    val path = points.sliding(2).flatMap(l => findPath(room, l(0), l(1))).toList
    (path, points)
  }

  def findPath(room: Set[Coordinate], from: Coordinate, to: Coordinate) = {
    val distanceMap = distanceMapBuilder(room, to, from)
    val path = ListBuffer.empty[Coordinate]
    var nc = from
    do {
      path += nc
      nc = nc.getFilteredNeighbours(room).minBy(distanceMap(_))
    } while (nc != to)
    path.toList
  }

  def distanceMapBuilder(room: Set[Coordinate], from: Coordinate, to: Coordinate) = {
    var distanceMap = collection.mutable.HashMap[Coordinate, Int]()
    var distance = 0
    var nextLevel = Set(from)
    do {
      distanceMap ++= nextLevel.map(_ -> distance).toMap
      distance += 1
      nextLevel = nextLevel.flatMap(_.getFilteredNeighbours(room)).filter(!distanceMap.contains(_))
    } while (nextLevel.nonEmpty)
    distanceMap
  }


}
