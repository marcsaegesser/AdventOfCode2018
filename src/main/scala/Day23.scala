package advent

import Math._

object Day23 {
  case class Coord(x: Int, y: Int, z: Int) {
    def up: Coord    = Coord(x, y+1, z)
    def down: Coord  = Coord(x, y-1, z)
    def left: Coord  = Coord(x-1, y, z)
    def right: Coord = Coord(x+1, y, z)
    def in: Coord    = Coord(x, y, z+1)
    def out: Coord   = Coord(x, y, z-1)

    def adjacencies: Set[Coord] = Set(up, down, left, right, in, out)

    def inRadius(r: Int): Set[Coord] = {
      def coordsFrom(x: Int, y: Int, z: Int): Set[Coord] =
        Set(Coord(x, y, z), Coord(x, y, -z), Coord(x, y, z), Coord(x, -y, z),
          Coord(x, -y, -z), Coord(-x, y, z), Coord(-x, y, -z), Coord(-x, -y, z),
          Coord(-x, -y, -z)
        )

      (for {
        z <- 0 to r
        y <- 0 to r-z
        x <- 0 to r-y-z
      } yield coordsFrom(x, y, z)).flatten.toSet
    }
  }


  def distance(c1: Coord, c2: Coord): Int =
    abs(c1.x - c2.x) + abs(c1.y - c2.y) + abs(c1.z - c2.z)

  def bounds(bots: List[Nanobot]): (Coord, Coord) =
    bots.foldLeft((Coord(Int.MaxValue, Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue, Int.MinValue))) { case ((mn, mx), Nanobot(Coord(x, y, z), r)) =>
      (
        Coord(min(mn.x, x-r), min(mn.y, y-r), min(mn.z, z-r)),
        Coord(max(mx.x, x+r), max(mx.y, y+r), max(mx.z, z+r)),
      )
    }

  case class Nanobot(pos: Coord, radius: Int)

  def part1(bots: List[Nanobot]): Int = {
    val m = bots.maxBy(_.radius)
    val r = m.radius
    val inRange = bots.filter(b => distance(m.pos, b.pos) <= r)
    inRange.size
  }


  val botRegex = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  def parseBot(s: String): Nanobot =
    s match {
      case botRegex(x, y, z, r) => Nanobot(Coord(x.toInt, y.toInt, z.toInt), r.toInt)
    }

  def loadData(file: String): List[Nanobot] =
    io.Source.fromFile(file)
      .getLines().filterNot(_.isEmpty)
      .map(parseBot)
      .toList
}
