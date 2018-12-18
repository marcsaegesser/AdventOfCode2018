package advent

object Day18 {

  def day18(): Unit = {
    val area = loadData("data/Day18.txt")
    println(s"Day18.part1 = ${part1(area)}")
    println(s"Day18.part2 = ${part2(area, 1000000000L)}")
  }

  def part1(area: Area): Int = {
    resourceValue(runN(10, area))
  }

  def part2(area: Area, n: Long): Int = {
    val (cycleStart, cycle, _) = findCycle(1, area)
    val cycleLength = cycle.size

    cycle((n - cycleStart).toInt % cycleLength)
  }

  def resourceValue(area: Area): Int ={
    val numTrees = area.values.filter(_ == Trees).size
    val numLumberYards = area.values.filter(_ == LumberYard).size

    numTrees * numLumberYards
  }

  case class Coord(x: Int, y: Int) {
    def adjacencies: List[Coord] = List(u, ur, r, dr, d, dl, l, ul)
    def u: Coord  = Coord(x,   y-1)
    def ur: Coord = Coord(x+1, y-1)
    def r: Coord  = Coord(x+1, y)
    def dr: Coord = Coord(x+1, y+1)
    def d: Coord  = Coord(x,   y+1)
    def dl: Coord = Coord(x-1, y+1)
    def l: Coord  = Coord(x-1, y)
    def ul: Coord = Coord(x-1, y-1)
  }

  sealed trait Usage
  case object Open       extends Usage
  case object Trees      extends Usage
  case object LumberYard extends Usage

  type Area = Map[Coord, Usage]

  def runN(n: Long, area: Area): Area ={
    if(n == 0) area
    else      runN(n-1, updateState(area))
  }

  def findCycle(n: Int, area: Area, history: List[(Int, Int)] = List()): (Int, List[Int], Area) = {
    if(n >= 1000000) throw new Exception("No cycle found")
    else{
      val next = updateState(area)
      val hash = next.hashCode
      if(history.map(_._1).contains(hash)) (n, (resourceValue(next) :: history.takeWhile(_._1 != hash).map(_._2).reverse), next)
      else                                 findCycle(n+1, next, (hash, resourceValue(next)) :: history.take(99))
    }

  }

  def updateState(area: Area): Area =
    area.toList.map { case (c, u) =>
      (c, updateUsage(area, c, u))
    }.toMap

  def updateUsage(area: Area, p: Coord, u: Usage): Usage = {
    val adjUsages = p.adjacencies.map(area.get).flatten
    u match {
      case Open       => if(adjUsages.filter(_ == Trees).size >= 3) Trees else Open
      case Trees      => if(adjUsages.filter(_ == LumberYard).size >= 3) LumberYard else Trees
      case LumberYard =>
        val numLY = adjUsages.filter(_ == LumberYard).size
        val numTrees = adjUsages.filter(_ == Trees).size
        if(numLY >= 1 && numTrees >= 1) LumberYard
        else                         Open
    }
  }

  def showUsage(u: Usage): String =
    u match {
      case Open       => "."
      case Trees      => "|"
      case LumberYard => "#"
    }

  def showArea(area: Area): String = {
    def showRow(y: Int, x1: Int, x2: Int): String =
      ((x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) => s ++= showUsage(area(Coord(x, y))) }).mkString

    val (tl, br) = bounds(area)
    ((for {
      y <- tl.y to br.y
      r =  showRow(y, tl.x, br.x)
    } yield r).mkString("\n"))
  }

  def bounds(area: Area): (Coord, Coord) =
    area.keys.foldLeft((Coord(Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue))) { case ((min, max), Coord(x, y)) =>
      (
        Coord(Math.min(min.x, x), Math.min(min.y, y)),
        Coord(Math.max(max.x, x), Math.max(max.y, y))
      )
    }

  def parseUsage(c: Char): Usage =
    c match {
      case '.' => Open
      case '|' => Trees
      case '#' => LumberYard
      case _   => throw new Exception(s"Invalid input $c")
    }

  def parseLine(y: Int, l: String): Stream[(Coord, Usage)] = {
    l.toStream.map(parseUsage).zip(Stream.from(0)).map { case (u, x) => (Coord(x, y), u) }
  }

  def loadData(file: String): Area =
    io.Source.fromFile(file).getLines().filterNot(_.isEmpty).toStream
      .zip(Stream.from(0)).map{ case (l, y) => parseLine(y, l) }
      .flatten.toMap
}
