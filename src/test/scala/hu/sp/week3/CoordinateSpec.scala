package hu.sp.week3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoordinateSpec extends AnyFlatSpec with Matchers {

  val origo = Coordinate(0, 0)

  "Add" should "work in all directions" in {
    origo + Direction.UP should be (Coordinate(0, 1))
    origo + Direction.DOWN should be (Coordinate(0, -1))
    origo + Direction.LEFT should be (Coordinate(-1, 0))
    origo + Direction.RIGHT should be (Coordinate(1, 0))
  }
}
