package hu.sp.week3

import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color

object AnsiPlayer extends App with AnsiTreminalScreen {

  ansiInitScreen
  ansiClearScreen

  val (testFlat, base) = FlatBuilder.getSetFromStringList(FlatBuilder.testFlat)
  printFlat(testFlat, base)

  ansiDestroyScreen

  def printFlat(cSet: Set[Coordinate], base: Coordinate) = {
    import scala.math.{max, min}

    val it = cSet.iterator
    val starter = it.next()
    val (minX, maxX, minY, maxY) = it.foldLeft(starter.x, starter.x, starter.y, starter.y)(
      (t, c) => (min(t._1, c.x), max(t._2, c.x), min(t._3, c.y), max(t._4, c.y))
    )

    val (xShift, yShift) = (1 - minX, 1 - minY)
    cSet.foreach(c => ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a(" "), c.x + xShift, c.y + yShift))
    ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("*"), base.x + xShift, base.y + yShift)
    ansiPrintAt(Ansi.ansi().a(s"minx=$minX maxx=$maxX"), 1, maxY+2)
  }

}
