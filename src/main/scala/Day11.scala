package advent

object Day11 {
  type Power = Int
  type Size = Int
  val GridSize = 300

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  }

  type Grid = Map[Coord, Int]

  def part1(serialNumber: Int): (Coord, Power, Size) =
    maxForSize(createGrid(serialNumber), 3)

  def part2(serialNumber: Int): (Coord, Power, Size) = {
    val grid = createGrid(serialNumber)
    (for {y <- 1 to GridSize; x <- 1 to GridSize } yield Coord(x, y))
      .par
      .map(c => maxSizeAtPoint(grid, c)).maxBy(_._2)
  }

  def maxForSize(grid: Grid, size: Int): (Coord, Power, Size) = {
    val positions = positionsForSize(size)
    positions.foldLeft((Coord(0, 0), Int.MinValue, size)) { case ((m, p, _), c) =>
      val pwr = powerForRegion(grid, c, size)
      if(pwr > p) (c, pwr, size)
      else        (m, p, size)
    }
  }

  def maxSizeAtPoint(grid: Grid, p: Coord): (Coord, Int, Int) = {
    println(s"maxSizeAtPoint: $p")
    val sizeLimit = math.min(GridSize - p.x, GridSize - p.y) + 1

    def helper(maxPower: Power, maxSize: Size, currentPwr: Power, currentSize: Size): (Power, Size) = {
      if(currentSize == sizeLimit) (maxPower, maxSize)
      else {
        val s = currentSize+1
        val newRow = (p.x until (p.x + s)).map(x => Coord(x, p.y + currentSize))
        val newCol = (p.y until (p.y + s - 1)).map(y => Coord(p.x + currentSize, y))
        val newPwr = (newRow ++ newCol).foldLeft(currentPwr){ case (a, c) => a + grid(c) }
        if(newPwr > maxPower) helper(newPwr, s, newPwr, s)
        else                  helper(maxPower, maxSize, newPwr, s)
      }
    }

    val (pwr, size) = helper(grid(p), 1, grid(p), 1)
    (p, pwr, size)
  }

  def positionsForSize(size: Int) =
    for {
      y <- 1 to GridSize-size
      x <- 1 to GridSize-size
    } yield Coord(x, y)

  val offsetsForSize = {
    (1 to GridSize).map { s =>
      (s, for { x <- 0 until s; y <- 0 until s } yield Coord(x, y))
    }.toMap
  }

  def powerForRegion(grid: Grid, c: Coord, size: Int): Int = {
    offsetsForSize(size).foldLeft(0) { case (a, o) =>  a + grid(c + o)}
  }

  def createGrid(serialNum: Int): Grid = {
    (for {
      y <- 1 to GridSize
      x <- 1 to GridSize
      p =  powerForCell(x, y, serialNum)
    } yield (Coord(x, y), p)).toMap
  }

  def powerForCell(x: Int, y: Int, serialNum: Int): Int = {
    val rackId = x + 10
    val power = ((((rackId * y) + serialNum) * rackId) / 100) % 10 - 5
    power
  }
}
