package hu.sp.week2

object TileType extends Enumeration {
  type TileType = Value
  val UNDEFINED = Value("??")
  val WALL = Value("[]")
  val PATH = Value(Console.WHITE_B + "  " + Console.RESET)
  val PATH_WITH_DIAMOND = Value(Console.WHITE_B + Console.BLACK + "<>" + Console.RESET)
}