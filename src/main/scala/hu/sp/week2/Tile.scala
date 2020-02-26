package hu.sp.week2

object Tile extends Enumeration {
  type Tile = Value
  val UNDEFINED = Value("??")
  val WALL = Value("[]")
  val PATH = Value(Console.WHITE_B + "  " + Console.RESET)
}