package hu.sp.week3

case class Direction(x: Int, y: Int)

case object Direction {
  val UP = Direction(0, 1)
  val DOWN = Direction(0, -1)
  val RIGHT = Direction(1, 0)
  val LEFT = Direction(-1, 0)
}


