package advent

import math._

object Day25 {
  case class Coord(x: Int, y: Int, z: Int, t: Int) {
    def distance(other: Coord): Int =
      abs(x - other.x) + abs(y - other.y) + abs(z - other.z) + abs(t - other.t)
  }

  def mergeAll(cs: List[Constellation]): List[Constellation] = {
    cs.foldLeft(List.empty[Constellation]) { case (a, c) =>
      val (m, s) = a.partition(_.canMerge(c))
      c.merge(m) :: s
    }
  }

  case class Constellation(coords: Set[Coord]) {
    def canMerge(other: Constellation): Boolean = {
      def helper(cs: List[Coord]): Boolean = {
        cs match {
          case Nil    => false
          case h :: t =>
            if(other.coords.exists(_.distance(h) <= 3)) true
            else helper(t)
        }
      }

      helper(coords.toList)
    }

    def merge(other: Constellation): Constellation = Constellation(coords | other.coords)

    def merge(others: Seq[Constellation]): Constellation =
      others.foldLeft(this) { case (accum, c) =>  accum.merge(c) }
  }

  val coordRegex = """(-?\d+),(-?\d+),(-?\d+),(-?\d+)""".r
  def parseCoord(s: String): Coord =
    s match {
      case coordRegex(x, y, z, t) => Coord(x.toInt, y.toInt, z.toInt, t.toInt)
    }

  def loadData(file: String): List[Constellation] =
    io.Source.fromFile(file)
      .getLines()
      .filterNot(_.isEmpty)
      .map(l => Constellation(Set(parseCoord(l))))
      .toList
}
