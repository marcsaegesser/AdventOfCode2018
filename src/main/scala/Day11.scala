package advent

object Day11 {
  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  }

  type Grid = Map[Coord, Int]

  def part1(serialNumber: Int): (Coord, Int, Int) =
    maxForSize(createGrid(serialNumber), 3)

  def part2(serialNumber: Int): (Coord, Int, Int) = {
    val grid = createGrid(serialNumber)
    val sizes = (1 to 300)

    sizes.map { s => maxForSize(grid, s) }.sortBy(_._2).last
  }

  def maxForSize(grid: Grid, size: Int): (Coord, Int, Int) = {
    val positions = positionsForSize(size)
    positions.foldLeft((Coord(0, 0), Int.MinValue, size)) { case ((m, p, _), c) =>
      val pwr = powerForRegion(grid, c, size)
      if(pwr > p) (c, pwr, size)
      else        (m, p, size)
    }
  }

  def positionsForSize(size: Int) =
    for {
      y <- 1 to 300-size
      x <- 1 to 300-size
    } yield Coord(x, y)

  // val cacheOffsetsForSize = collection.mutable.Map.empty[Int, IndexedSeq[Coord]]
  // def offsetsForSize(size: Int): IndexedSeq[Coord] = {
  //   cacheOffsetsForSize.get(size).getOrElse {
  //     val coords =
  //       for {
  //         x <- 0 until size
  //         y <- 0 until size
  //       } yield Coord(x, y)

  //     cacheOffsetsForSize(size) = coords
  //     coords
  //   }
  // }

  val offsetsForSize = {
    (1 to 300).map { s =>
      (s, for { x <- 0 until s; y <- 0 until s } yield Coord(x, y))
    }.toMap
  }

  def powerForRegion(grid: Grid, c: Coord, size: Int): Int = {
    offsetsForSize(size).foldLeft(0) { case (a, o) =>  a + grid(c + o)}
  }

  def createGrid(serialNum: Int): Grid = {
    (for {
      y <- 1 to 300
      x <- 1 to 300
      p =  powerForCell(x, y, serialNum)
    } yield (Coord(x, y), p)).toMap
  }

  def powerForCell(x: Int, y: Int, serialNum: Int): Int = {
    val rackId = x + 10
    val power = ((((rackId * y) + serialNum) * rackId) / 100) % 10 - 5
    power
  }
}
