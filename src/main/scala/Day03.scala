package advent

object Day03 {
  type IdNum = Int
  type Coord = (Int, Int)

  case class Patch(id: IdNum, x: Int, y: Int, w: Int, h: Int)

  def patchToCoords(p: Patch): List[(Coord, IdNum)] =
    for {
      xs <- (p.x until p.x+p.w).toList
      ys <- (p.y until p.y+p.h).toList
    } yield ((xs, ys), p.id)

  type BoardState = Map[Coord, List[IdNum]]  // The board is Map from (x,y) coords to a list of ids claiming that coordinate

  def emptyBoard = Map.empty[Coord, Int]

  def mkBoard(ps: List[Patch]): BoardState =
    ps.flatMap(patchToCoords)
      .groupBy(_._1)
      .mapValues(_.map(_._2))

  def findOverlaps(b: BoardState): BoardState =
    b.filter { case (k, v) => v.size > 1 }


  def part1(ps: List[Patch]): Int = {
    findOverlaps(mkBoard(ps)).size
  }

  def part2(ps: List[Patch]): Set[IdNum] = {
    val allIds = ps.map(_.id).toSet
    val overlapIds = findOverlaps(mkBoard(ps)).values.flatten.toSet

    allIds -- overlapIds
  }

  def loadData(file: String): List[Patch] =
    io.Source.fromFile(file)
      .getLines()
      .map(parsePatch)
      .toList

  val patchRegex = """#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)""".r
  def parsePatch(s :String): Patch =
    s match {
      case patchRegex(i, x, y, h, w) => Patch(i.toInt, x.toInt, y.toInt, h.toInt, w.toInt)
      case _                         => throw new Exception(s"Bad input: $s")
    }

}
