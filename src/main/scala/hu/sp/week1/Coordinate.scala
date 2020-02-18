package hu.sp.week1

case class Coordinate(x: Int, y: Int) {

  def getNeighbours = {
    List(Coordinate(x - 1, y), Coordinate(x + 1, y), Coordinate(x, y - 1), Coordinate(x, y + 1))
  }

  def getPassagewayNeighbours = {
    val leftRight = x % 2 == 0 && y % 2 == 1
    val upDown = x % 2 == 1 && y % 2 == 0
    assert(!upDown || !leftRight, s"Not a passageway coordinate! x=$x y=$y")
    upDown match {
      case true => List(Coordinate(x, y - 1), Coordinate(x, y + 1))
      case false => List(Coordinate(x - 1, y), Coordinate(x + 1, y))
    }
  }

  def +(c: Coordinate) = {
    Coordinate(x + c.x, y + c.y)
  }

  def *(i: Int) = {
    Coordinate(x * i, y * i)
  }
}

