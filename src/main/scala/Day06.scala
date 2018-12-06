package advent

/** I'd like to re-visit this because I'm sure there are more
  * efficient solutions.
  *
  */
object Day06 {
  type Coord = (Int, Int)

  def part1(locations: List[Coord]): (Coord, Int) = {
    val region = bounds(locations)
    val closestLocations =
      (for {
        x <- region._1._1 to region._2._1
        y <- region._1._2 to region._2._2
        l = closestLocationsTo((x, y), locations) if l.size == 1
      } yield (l.head, (x, y))).groupBy(_._1).mapValues(_.map(_._2))

    closestLocations
      .collect { case (k, v) if !v.exists(c => outsideRegion(c, region)) => (k, v.size) }
      .toList
      .sortBy(_._2)
      .reverse.head
  }

  def part2(limit: Int, locations: List[Coord]): Int = {
    val region = bounds(locations)
    val closeEnough =
      for {
        x <- region._1._1 to region._2._1
        y <- region._1._2 to region._2._2
        d =  distanceToAllLocations((x, y), locations).sum if d < limit
      } yield (x ,y)

    closeEnough.size
  }

  def outsideRegion(c: Coord, bounds: (Coord, Coord)): Boolean =
    c._1 <= bounds._1._1 || c._1 >= bounds._2._1 || c._2 <= bounds._1._2 || c._2 >= bounds._2._2

  def closestLocationsTo(c: Coord, locations: List[Coord]): List[Coord] = {
    val byDistance = locations.map { l => (l, distance(c, l)) }.sortBy(_._2)
    byDistance.takeWhile(_._2 == byDistance.head._2).map(_._1)
  }

  def distanceToAllLocations(c: Coord, locations: List[Coord]): List[Int] =
    locations.map(l => distance(l, c))

  def bounds(locations: List[Coord]): (Coord, Coord) =
    locations.foldLeft(((Int.MaxValue, Int.MaxValue), (Int.MinValue, Int.MinValue))) { case ((min, max), (x, y)) =>
      (
        (Math.min(min._1, x), Math.min(min._2, y)),
        (Math.max(max._1, x), Math.max(max._2, y))
      )
    }

  def distance(a: Coord, b: Coord): Int =
    Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  val coordRegex = """(\d+),\s+(\d+)""".r

  def loadData(file: String): List[Coord] =
    io.Source.fromFile(file)
      .getLines()
      .filterNot(_.isEmpty)
      .map { l => val coordRegex(x, y) = l; (x.toInt, y.toInt) }
      .toList
}
