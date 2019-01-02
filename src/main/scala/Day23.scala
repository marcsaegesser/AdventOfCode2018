package advent

import Math._

object Day23 {

  def day23(): Unit = {
    val bots = loadData("data/Day23.txt")
    println(s"Day23.part1 = ${part1(bots)}")
    println(s"Day23.part2 = ${part2(bots)}")
  }

  def part1(bots: List[Nanobot]): Int = {
    val m = bots.maxBy(_.radius)
    val r = m.radius
    val inRange = bots.filter(b => distance(m.pos, b.pos) <= r)
    inRange.size
  }

  def part2(bots: List[Nanobot]): Long = {
    val c = findMostConnectedPoint(bots)
    distance(origin, c)
  }

  case class Coord(x: Long, y: Long, z: Long)
  val origin = Coord(0, 0, 0)

  /** A Cube is defined by a point, p, and a size. The size
    * extends from p in the positive x, y, and z directions.
    *
    * For the following algorithms to work, size MUST BE A POWER OF 2.
    */
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

    /** Split the current cube into 8 equal sized sub-cubes.
      *
      * NOTE: The size is assumed to be a power of 2 so that
      * there is never a remainder in the divisision computing
      * newSize.
      */
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

    // Distance to closest corner from c, or 0 if the cube contains c
    def cubeDist(c: Coord): Long = {
      def helper(c: Long, c1: Long, c2: Long): Long = {
        if(c < c1)      c1 - c
        else if(c > c2) c - c2
        else            0
      }

      helper(p.x, p.x, p.x+size-1) +
      helper(p.y, p.y, p.y+size-1) +
      helper(p.z, p.z, p.z+size-1)
    }

  }

  case class Nanobot(pos: Coord, radius: Long)

  /** Find the point closest to the most bots.
    *
    * The core of this algorithm was taken from 2-D description at
    * https://raw.githack.com/ypsu/experiments/master/aoc2018day23/vis.html
    */
  def findMostConnectedPoint(bots: List[Nanobot]): Coord = {
    def helper(cubes: List[(Cube, Int, Long)]): Cube = {
      val sorted = cubes.sortBy(c => (-c._2, c._3, c._1.size))
      sorted match {
        case h :: t =>
          if(h._1.size == 1) h._1
          else {
            val newCubes = h._1.split.par.map(c => (c, botsTouchingCube(c, bots), c.cubeDist(origin)))
            helper(t ++ newCubes)
          }
        case Nil    => throw new Exception("No more cubes!")
      }
    }

    val initialCube = boundingCube(bots)
    val start = List((initialCube, botsTouchingCube(initialCube, bots), initialCube.cubeDist(origin)))
    helper(start).p
  }

  /** Count the bots whose range touches a cube.
    */
  def botsTouchingCube(cube: Cube, bots: List[Nanobot]): Int =
    bots.par.foldLeft(0) { case (count, b) =>
      if(botTouchesCube(cube, b)) count+1
      else                        count
    }

  /** Is the given cube in the range of the given bot.
    *
    * A cube is in range if the bot's range includes any of the cube's
    * corners or if the bot's position is within the cube.
    */
  def botTouchesCube(cube: Cube, bot: Nanobot): Boolean = {
    cube.corners.foldLeft(false) { case (a, p) =>  a || botInRange(bot, p) } ||
    cube.contains(bot.pos)
  }

  /** Is the given point inside the the bot's range.
    */
  def botInRange(bot: Nanobot, p: Coord): Boolean =
    bot match { case Nanobot(c, r) => distance(p, c) <= r }

  /** 3-D manhattan distance between two points.
    */
  def distance(c1: Coord, c2: Coord): Long =
    abs(c1.x - c2.x) + abs(c1.y - c2.y) + abs(c1.z - c2.z)

  /** Compute a bounds of the bot-space including their radii.
    */
  def bounds(bots: List[Nanobot]): (Coord, Coord) =
    bots.foldLeft((Coord(Int.MaxValue, Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue, Int.MinValue))) { case ((mn, mx), Nanobot(Coord(x, y, z), r)) =>
      (
        Coord(min(mn.x, x-r), min(mn.y, y-r), min(mn.z, z-r)),
        Coord(max(mx.x, x+r), max(mx.y, y+r), max(mx.z, z+r)),
      )
    }

  /** Compute a bounding cube around the bot-space.
    *
    * The size of this cube is the smallest power of two greater than
    * the bounds of the bot-space.
    */
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
