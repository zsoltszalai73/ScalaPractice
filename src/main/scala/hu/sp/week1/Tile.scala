package hu.sp.week1

object Tile extends Enumeration {
  type Tile = Value
  val WALL = Value("  ")
  val PATH = Value(Console.WHITE_B + "  " + Console.RESET)
}