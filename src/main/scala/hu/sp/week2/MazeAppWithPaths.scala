package hu.sp.week2

object MazeAppWithPaths extends App {

  //  val (width, height, numOfPaths) = readParams
  //  run(MazeGeneratorFive, width, height, numOfPaths)

  val m = MazeGeneratorFive

  private val maze: Maze = m.generate(77, 37, 5)

  (maze.exit :: maze.diamonds).foreach(maze.addDistanceMap(_))

  val entryToExitPath = maze.findPathByDMapKey(maze.entry, maze.exit)
  println(s"${maze.asString(entryToExitPath, maze.exit)}")

//  val entryToDiamondPath = maze.findPathByDMapKey(maze.entry, maze.diamonds.head)
//  println(s"${maze.asString(entryToDiamondPath, maze.diamonds.head)}")


  val entryToExitWithDiamondsPath = maze.findShortestPathWithAllDiamonds
  println(s"${maze.asString(entryToExitWithDiamondsPath)}")


  private def readParams = {
    println("Maze generator")
    println("Enter width and height")
    print("width=")
    val width = io.StdIn.readInt()
    print("height=")
    val height = io.StdIn.readInt()
    print("count=")
    val count = io.StdIn.readInt()
    assert(width > 2 && width % 2 == 1 && height > 2 && height % 2 == 1 && count > 0, "Width and height have to be uneven number, greater than 2!")
    (width, height, count)
  }

  private def run(mazeGen: MazeGenerator, width: Int, height: Int, count: Int) = {

    println(s"${mazeGen.getClass.getSimpleName}\n" + "=" * mazeGen.getClass.getSimpleName.length)
    val times = for (_ <- 1 to count) yield {
      val t0 = System.nanoTime()
      mazeGen.generate(width, height, 5)
      val t1 = System.nanoTime()
      val diff = (t1 - t0).toDouble / 1000000000
      println(s"Elapsed time: $diff Sec")
      diff
    }
    println(s"Average time = ${times.sum / times.length}\n")
  }

}

