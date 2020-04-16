package hu.sp.week3

import org.fusesource.jansi.{Ansi, AnsiConsole}

trait AnsiTreminalScreen {

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

}
