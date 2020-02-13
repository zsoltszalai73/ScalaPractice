package hu.sp.week1

import hu.sp.week1.Tile.Tile

case class Maze(tileMap: Map[Coordinate, Tile], width: Int, height: Int) {

  def printMaze = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        tileMap.get(Coordinate(x, y)) match {
          case None => print("  ")
          case Some(t) => print(t.toString)
        }
      }
      println
    }
  }
}

object MazeApp extends App {

  println("Maze generator")
  println("Enter width and height")
  print("width=")
  val width = io.StdIn.readInt()
  print("height=")
  val height = io.StdIn.readInt()
  println(s"width=$width height=$height")

  assert(width > 2 && width % 2 == 1 && height > 2 && height % 2 == 1, "Width and height have to be uneven number, greater than 2!")

  //  val m = MazeGeneratorTwo(width, height).generate
  //  m.printMaze

  println("MazeGeneratorOne")
  run(new MazeGeneratorOne, width, height, 10)
  println("MazeGeneratorTwo")
  run(new MazeGeneratorTwo, width, height, 10)


  def run(mazeGen: MazeGenerator, width: Int, height: Int, count: Int) = {
    val times = for (_ <- 1 to count) yield {
      val t0 = System.nanoTime()
      mazeGen.generate(width, height)
      val t1 = System.nanoTime()
      val diff = (t1 - t0).toDouble / 1000000000
      println(s"Elapsed time: $diff Sec")
      diff
    }
    println(s"Average time = ${times.sum / times.length}")
  }

}
