package hu.sp.week2

case class Coordinate(x: Int, y: Int) {

  def getNeighbours = {
    List(Coordinate(x - 1, y), Coordinate(x + 1, y), Coordinate(x, y - 1), Coordinate(x, y + 1))
  }

  def +(c: Coordinate) = {
    Coordinate(x + c.x, y + c.y)
  }

  def *(i: Int) = {
    Coordinate(x * i, y * i)
  }
}

