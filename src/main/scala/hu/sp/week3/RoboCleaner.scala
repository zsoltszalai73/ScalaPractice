package hu.sp.week3

import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color

import scala.util.Random

object RoboCleaner extends AnsiScreenPrinter {

  /**
   * Should return a path as a list
   */
  def cleanUpKnownSpace(room: Set[Coordinate], base: Coordinate, stepsWithOneCharge: Int) : List[Coordinate] = {
    val to = Random.shuffle(room).head
    findPath(room, base, to)
    List(base)
  }

  def findPath(room: Set[Coordinate], from: Coordinate, to: Coordinate) = {

    var distanceMap = collection.mutable.HashMap[Coordinate, Int]()

    def dPrinter(c: Coordinate) = {

      def form(i: Int): String = {
        i match {
          case i: Int if i < 10 => "0" + i
          case i: Int if i > 99 => form(i % 100)
          case _ => i.toString
        }
      }

      distanceMap.get(c) match {
        case Some(value) => Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a(form(value))
        case None => Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("<>")
      }

    }

    var distance = 0
    var nextLevel = Set(from)
    do {
      ansiPrintFlat(room, from, dPrinter)
      Thread.sleep(200)
      distanceMap ++= nextLevel.map(_ -> distance).toMap
      distance += 1
      nextLevel = nextLevel.flatMap(_.getFilteredNeighbours(room)).filter(!distanceMap.contains(_))
    } while (nextLevel.nonEmpty)

    ansiPrintFlat(room, from, dPrinter)

  }


}
