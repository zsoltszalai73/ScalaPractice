package hu.sp.week3

object AnsiPlayer extends App with AnsiScreenPrinter {

  ansiInitScreen
  ansiClearScreen

  val (testFlat, base) = FlatBuilder.getSetFromStringList(FlatBuilder.testFlat)
  RoboCleaner.cleanUpKnownSpace(testFlat, base, 100)
  //ansiPrintFlat(testFlat, base)

  ansiDestroyScreen


}
