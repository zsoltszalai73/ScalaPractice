package hu.sp.week3

import org.fusesource.jansi.Ansi.Color
import org.fusesource.jansi.{Ansi, AnsiConsole}

trait AnsiScreenPrinter {

  def ansiInitScreen = {
    synchronized {
      AnsiConsole.systemInstall
    }
  }

  def ansiDestroyScreen = {
    synchronized {
      AnsiConsole.systemUninstall
    }
  }

  def ansiClearScreen = {
    synchronized {
      AnsiConsole.out.println(Ansi.ansi().eraseScreen())
    }
  }

  def ansiPrintAt(ansiSeq: Ansi, x: Int, y: Int) = {
    synchronized {
      AnsiConsole.out.print(Ansi.ansi().cursor(y, x).a(ansiSeq).reset)
    }
  }

  def defaultPrinter(c: Coordinate) = {
    Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("  ")
  }

  def ansiPrintFlat(cSet: Set[Coordinate], base: Coordinate, cPrinter: (Coordinate) => Ansi = defaultPrinter) = {
    import scala.math.{max, min}

    val it = cSet.iterator
    val starter = it.next()
    val (minX, maxX, minY, maxY) = it.foldLeft(starter.x, starter.x, starter.y, starter.y)(
      (t, c) => (min(t._1, c.x), max(t._2, c.x), min(t._3, c.y), max(t._4, c.y))
    )

    val (xShift, yShift) = (1 - minX, 1 - minY)
    cSet.foreach(c => ansiPrintAt(cPrinter(c), 2*c.x + xShift, c.y + yShift))
    ansiPrintAt(Ansi.ansi().bg(Color.BLUE).fgBrightYellow().a("**"), 2*base.x + xShift, base.y + yShift)
    //ansiPrintAt(Ansi.ansi().a(s"minx=$minX maxx=$maxX"), 1, maxY+2)
  }

}
