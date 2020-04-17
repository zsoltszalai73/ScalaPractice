package hu.sp.week3

object Utils {

  def formatInt(i: Int): String = {
    i match {
      case i: Int if i < 10 => "0" + i
      case i: Int if i > 99 => formatInt(i % 100)
      case _ => i.toString
    }
  }

}
