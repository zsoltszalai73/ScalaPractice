package hu.sp.week3

case class Coordinate(x: Int, y: Int) {
  def +(d: Direction) = Coordinate(x + d.x, y + d.y)
  def getNeighbours = List(this + Direction.UP, this + Direction.DOWN, this + Direction.LEFT, this + Direction.RIGHT,
    this + Direction.UP + Direction.RIGHT, this + Direction.UP + Direction.LEFT,
    this + Direction.DOWN + Direction.RIGHT, this + Direction.DOWN + Direction.LEFT)
  def getFilteredNeighbours(s: Set[Coordinate]) = getNeighbours.filter(s.contains(_))
}
