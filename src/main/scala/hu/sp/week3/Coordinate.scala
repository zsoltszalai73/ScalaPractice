package hu.sp.week3

case class Coordinate(x: Int, y: Int) {
  def +(d: Direction) = Coordinate(x + d.x, y + d.y)
  def getNeighbours = List(this + Direction.UP, this + Direction.DOWN, this + Direction.LEFT, this + Direction.RIGHT)
}
