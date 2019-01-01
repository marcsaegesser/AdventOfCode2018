package advent

import Math._

object Day23 {

  def part1(bots: List[Nanobot]): Int = {
    val m = bots.maxBy(_.radius)
    val r = m.radius
    val inRange = bots.filter(b => distance(m.pos, b.pos) <= r)
    inRange.size
  }

  def part2(bots: List[Nanobot]): Long = {
    val c = findMostConnected(bots)
    distance(origin, c.p)
  }

  case class Coord(x: Long, y: Long, z: Long)
  val origin = Coord(0, 0, 0)

  case class Cube(p: Coord, size: Long) {

    def contains(c: Coord): Boolean =
      (p, c) match { case (Coord(x, y, z), Coord(cx, cy, cz)) =>
        x <= cx && cx < x+size &&
        y <= cy && cy < y+size &&
        z <= cz && cz < z+size
      }

    def corners: Set[Coord] =
      p match { case Coord(x, y, z) =>
        Set(
          Coord(x, y, z),
          Coord(x+size-1, y, z),
          Coord(x, y+size-1, z),
          Coord(x+size-1, y+size-1, z),
          Coord(x, y, z+size-1),
          Coord(x+size-1, y, z+size-1),
          Coord(x, y+size-1, z+size-1),
          Coord(x+size-1, y+size-1, z+size-1)
        )
      }

    def split: Set[Cube] = {
      val newSize = size / 2
      p match { case Coord(x, y, z) =>
        Set(
          Cube(Coord(x, y, z), newSize),
          Cube(Coord(x+newSize, y, z), newSize),
          Cube(Coord(x, y+newSize, z), newSize),
          Cube(Coord(x+newSize, y+newSize, z), newSize),
          Cube(Coord(x, y, z+newSize), newSize),
          Cube(Coord(x+newSize, y, z+newSize), newSize),
          Cube(Coord(x, y+newSize, z+newSize), newSize),
          Cube(Coord(x+newSize, y+newSize, z+newSize), newSize)
        )
      }
    }
  }

  case class Nanobot(pos: Coord, radius: Long)

  // Distance to closest corner from p or 0 if the cube contains p
  def cubeDist(cube: Cube, p: Coord): Long = {
    def helper(p: Long, c1: Long, c2: Long): Long = {
      if(p < c1)      c1 - p
      else if(p > c2) p - c2
      else            0
    }

    helper(p.x, cube.p.x, cube.p.x+cube.size-1) +
    helper(p.y, cube.p.y, cube.p.y+cube.size-1) +
    helper(p.z, cube.p.z, cube.p.z+cube.size-1)
  }

  def findMostConnected(bots: List[Nanobot]): Cube = {
    def helper(cubes: List[(Cube, Int, Long)]): Cube = {
      val sorted = cubes.sortBy(c => (-c._2, c._3, c._1.size))
      sorted match {
        case h :: t =>
          if(h._1.size == 1) h._1
          else {
            val newCubes = h._1.split.par.map(c => (c, botsTouchingCube(c, bots), cubeDist(c, origin)))
            helper(t ++ newCubes)
          }
        case Nil    => throw new Exception("No more cubes!")
      }
    }

    val initialCube = boundingCube(bots)
    val start = List((initialCube, botsTouchingCube(initialCube, bots), cubeDist(initialCube, origin)))
    helper(start)
  }

  def botsTouchingCube(cube: Cube, bots: List[Nanobot]): Int =
    bots.par.foldLeft(0) { case (count, b) => if(botTouchesCube(cube, b)) count+1 else count }

  def botTouchesCube(cube: Cube, bot: Nanobot): Boolean = {
    cube.corners.foldLeft(false) { case (a, p) =>  a || botInRange(bot, p) } || cube.contains(bot.pos)
  }

  def botInRange(bot: Nanobot, p: Coord): Boolean =
    bot match { case Nanobot(c, r) => distance(p, c) <= r }

  def botsInRange(bots: List[Nanobot], p: Coord): Int = {
    bots.foldLeft(0) { case (n, Nanobot(c, r)) =>
      if(distance(p, c) <= r) n + 1
      else                   n
    }
  }

  def distance(c1: Coord, c2: Coord): Long =
    abs(c1.x - c2.x) + abs(c1.y - c2.y) + abs(c1.z - c2.z)

  def bounds(bots: List[Nanobot]): (Coord, Coord) =
    bots.foldLeft((Coord(Int.MaxValue, Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue, Int.MinValue))) { case ((mn, mx), Nanobot(Coord(x, y, z), r)) =>
      (
        Coord(min(mn.x, x-r), min(mn.y, y-r), min(mn.z, z-r)),
        Coord(max(mx.x, x+r), max(mx.y, y+r), max(mx.z, z+r)),
      )
    }

  def boundingCube(bots: List[Nanobot]): Cube = {
    val (a, b) = bounds(bots)
    val maxSide = max(max((b.x-a.x), b.y-a.y), b.z-a.z)
    val cubeSize = math.pow(2, ((31 - Integer.numberOfLeadingZeros(maxSide.toInt))+1).toDouble).toInt
    Cube(a, cubeSize.toLong)
  }

  val botRegex = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  def parseBot(s: String): Nanobot =
    s match {
      case botRegex(x, y, z, r) => Nanobot(Coord(x.toLong, y.toLong, z.toLong), r.toLong)
    }

  def loadData(file: String): List[Nanobot] =
    io.Source.fromFile(file)
      .getLines().filterNot(_.isEmpty)
      .map(parseBot)
      .toList
}
