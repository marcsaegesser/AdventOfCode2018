package advent

object Day11 {
  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  }

  type Grid = Map[Coord, Int]

  def part1(serialNumber: Int): (Coord, Int) = {
    val grid = createGrid(serialNumber)
    val positions = for { y <- 1 to 297; x <- 1 to 297 } yield Coord(x, y)
    positions.foldLeft((Coord(0, 0), Int.MinValue)) { case ((m, p), c) =>
      val pwr = powerForRegion(grid, c)
      if(pwr > p) (c, pwr)
      else        (m, p)
    }
  }

  def powerForRegion(grid: Grid, c: Coord): Int = {
    grid(c) +
    grid(c + Coord(1, 0)) +
    grid(c + Coord(2, 0)) +
    grid(c + Coord(0, 1)) +
    grid(c + Coord(1, 1)) +
    grid(c + Coord(2, 1)) +
    grid(c + Coord(0, 2)) +
    grid(c + Coord(1, 2)) +
    grid(c + Coord(2, 2))
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
