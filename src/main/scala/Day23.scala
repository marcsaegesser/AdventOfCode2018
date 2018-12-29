package advent

import Math._

object Day23 {
  case class Coord(x: Int, y: Int, z: Int)
  def distance(c1: Coord, c2: Coord): Int =
    abs(c1.x - c2.x) + abs(c1.y - c2.y) + abs(c1.z - c2.z)

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
