package advent

object Day15 {
  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x+other.x, y+other.y)
  }
  val Up    = Coord(0, -1)
  val Right = Coord(1, 0)
  val Down  = Coord(0, 1)
  val Left  = Coord(-1, 0)

  sealed trait Unit
  sealed trait LiveUnit { def hp: Int; def power: Int }
  case object Wall                   extends Unit
  case object Open                   extends Unit
  case class Elf(hp: Int, power: Int)    extends Unit with LiveUnit
  case class Goblin(hp: Int, power: Int) extends Unit with LiveUnit

  type Board = Map[Coord, Unit]

  def distance(a: Coord, b: Coord): Int = Math.abs(b.x - a.x) + Math.abs(a.y - b.y)

  implicit object CoordOrdering extends Ordering[Coord] {
    def compare(a: Coord, b: Coord): Int = {
      if(a.y == b.y) a.x compare b.x
      else          a.y compare b.y
    }
  }

  def adjacencies(p: Coord): List[Coord] =
    List(Up, Left, Right, Down).map(p + _)

  def isAdjacent(p: Coord, q: Coord): Boolean =
    p.x == q.x && Math.abs(p.y-q.y) == 1 || p.y == q.y && Math.abs(p.x-q.x) == 1

  def showUnit(u: Unit): String =
    u match {
      case Wall        => "#"
      case Open        => "."
      case Elf(_,_)    => "E"
      case Goblin(_,_) => "G"
    }

  def showBoard(board: Board): String = {
    def showRow(y: Int, x1: Int, x2: Int): String =
      (x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) => s ++= showUnit(board.getOrElse(Coord(x,y), Open)) }.mkString

    val (tl, br) = bounds(board)

    (for {
      y <- tl.y to br.y
      r =  showRow(y, tl.x, br.x)
    } yield r).mkString("\n")
  }

  def bounds(board: Board): (Coord, Coord) =
    board.keys.foldLeft((Coord(Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue))) { case ((min, max), Coord(x, y)) =>
      (
        Coord(Math.min(min.x, x), Math.min(min.y, y)),
        Coord(Math.max(max.x, x), Math.max(max.y, y))
      )
    }

  def loadData(file: String): Board = {
    io.Source.fromFile(file)
      .getLines()
      .zip(Stream.from(0).toIterator)
      .flatMap{ case (l, y) =>
        l.zip(Stream.from(0)).collect { case (c, x) if c != '.' => (Coord(x, y), parseUnit(c)) } }
      .toMap
  }

  def parseUnit(c: Char): Unit =
    c match {
      case '#' => Wall
      case 'E' => Elf(200, 3)
      case 'G' => Goblin(200, 3)
    }

}
