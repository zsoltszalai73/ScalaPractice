package hu.sp.week2

case class Maze(tileMap: Map[Coordinate, Tile], width: Int, height: Int) {

  def asString = {
    val sb = new StringBuilder
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        tileMap.get(Coordinate(x, y)) match {
          case None => sb ++= "  "
          case Some(t) => sb ++= t.toString
        }
      }
      sb ++= "\n"
    }
    sb.toString()
  }
}

