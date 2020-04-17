package hu.sp.week3

import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color

object AnsiPlayer extends App with AnsiScreenPrinter {

  val SLEEP = 250

  ansiInitScreen
  ansiClearScreen

  val (testFlat, base) = FlatBuilder.getSetFromStringList(FlatBuilder.testFlat)
  val (path, points) = RoboCleaner.visitRandomPoints(testFlat, base, 100)
  val (xShift, yShift) = ansiPrintFlat(testFlat, base, c => {
    if (points.contains(c)) Ansi.ansi().bg(Color.BLUE).fg(Color.RED).a("<>")
    else defaultPrinter(c)
  })
  ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("##"), 2*base.x + xShift, base.y + yShift)
  Thread.sleep(SLEEP)
  path.sliding(2).foreach(l => {
    ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("  "), 2*l(0).x + xShift, l(0).y + yShift)
    ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("##"), 2*l(1).x + xShift, l(1).y + yShift)
    Thread.sleep(SLEEP)
  })
  ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("##"), 2*base.x + xShift, base.y + yShift)
  Thread.sleep(SLEEP)

  ansiDestroyScreen

}
