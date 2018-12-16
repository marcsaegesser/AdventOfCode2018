package advent

object Day15 {
  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x+other.x, y+other.y)
  }
  def distance(a: Coord, b: Coord): Int = Math.abs(b.x - a.x) + Math.abs(a.y - b.y)

  def adjacencies(p: Coord): List[Coord] =
    List(Up, Left, Right, Down).map(p + _)

  def isAdjacent(p: Coord, q: Coord): Boolean =
    p.x == q.x && Math.abs(p.y-q.y) == 1 || p.y == q.y && Math.abs(p.x-q.x) == 1

  implicit object CoordOrdering extends Ordering[Coord] {
    def compare(a: Coord, b: Coord): Int = {
      if(a.y == b.y) a.x compare b.x
      else          a.y compare b.y
    }
  }

  val Up    = Coord(0, -1)
  val Right = Coord(1, 0)
  val Down  = Coord(0, 1)
  val Left  = Coord(-1, 0)

  sealed trait Unit
  sealed trait LiveUnit extends Unit { def hp: Int; def power: Int }
  case object Wall                   extends Unit
  case object Open                   extends Unit
  case class Elf(hp: Int, power: Int)    extends LiveUnit
  case class Goblin(hp: Int, power: Int) extends LiveUnit

  type Board = Map[Coord, Unit]

  def runRound(board: Board): (Boolean, Board) = {
    def helper(b: Board, units: List[(Coord, LiveUnit)]): (Boolean, Board) =
      units match {
        case Nil => (true, b)
        case u :: us =>
          runUnit(b, u._1, u._2) match {
            case (false, nb) => (false, nb)
            case (true, nb) => helper(nb, us)
          }
      }

    helper(board, liveUnits(board))
  }

  def runUnit(board: Board, p: Coord, u: LiveUnit): (Boolean, Board) = {
    val targets = targetUnits(board, p, u)
    if(targets.isEmpty)             (false, board)
    else if(board.get(p) != Some(u)) (true, board)  // This unit died before we could run it
    else {
      val next =
        findAttack(board, p, u, targets).map(at => board )  // TODO: Implement attack
          .orElse {
            findMove(board, p, u, targets)
              .map{ to =>
                val b = moveUnit(board, p, to, u)
                val b2 = findAttack(b, to, u, targets).map(at => board) // TODO: Implement attack
                b2.getOrElse(b)
              }
          }
          .getOrElse(board)

      (true, next)
    }

  }

  def moveUnit(board: Board, from: Coord, to: Coord, u: Unit): Board =
    board - from + ((to, u))

  def findAttack(board: Board, p: Coord, u: LiveUnit, targets: List[(Coord, LiveUnit)]): Option[Coord] = {
    targets
      .filter(t => isAdjacent(t._1, p))
      .sortBy(_._1)
      .headOption
      .map { case (c, target) =>
        println(s"fight at $c")
        c
      }
  }

  object CoordDistOrdering extends Ordering[(Coord, Int)] {
    def compare(a: (Coord, Int), b: (Coord, Int)): Int =
      if(a._2 == b._2) CoordOrdering.compare(a._1, b._1)
      else            a._2 compare b._2
  }

  def findMove(board: Board, p: Coord, u: LiveUnit, targets: List[(Coord, LiveUnit)]): Option[Coord] = {
    reachableTargetPoints(board, p, targets)
      .map(tp => (tp, distance(p, tp)))
      .toList
      .sorted(CoordDistOrdering)
      .headOption
      .map{ case (t, _) =>
        println(s"move to $t")
        chooseStep(board, p, t) }
  }

  def chooseStep(board: Board, p: Coord, t: Coord): Coord = {
    adjacencies(p)
      .filter(c => isOpen(board, c))
      .map(c => (c, distance(c, t)))
      .sorted(CoordDistOrdering)
      .head._1
  }

  def isOpen(board: Board, p: Coord): Boolean = !board.isDefinedAt(p)

  def reachablePoints(board: Board, p: Coord): Set[Coord] = {
    def helper(accum: Set[Coord], c: Coord): Set[Coord] = {
      val newPoints = adjacencies(c).filterNot(accum.contains).filter(isOpen(board, _))
      if(newPoints.isEmpty) accum
      else                  newPoints.foldLeft(accum ++ newPoints) { case (a, c) => a ++ helper(a, c) }
    }

    helper(Set(), p)
  }

  def liveUnits(board: Board): List[(Coord, LiveUnit)] =
    board.toList.collect { case (c, u: LiveUnit) => (c, u) }.sortBy(_._1)

  def targetUnits(board: Board, p: Coord, u: LiveUnit): List[(Coord, LiveUnit)] = {
    u match {
      case e: Elf    => liveUnits(board).collect { case (c, g: Goblin) => (c, g) }
      case g: Goblin => liveUnits(board).collect { case (c, g: Elf) => (c, g) }
    }
  }

  def targetPoints(board: Board, ts: List[(Coord, LiveUnit)]): Set[Coord] =
    ts.flatMap ( t => adjacencies(t._1) ).filter(p => isOpen(board, p)).toSet

  def reachableTargetPoints(board: Board, p: Coord, ts: List[(Coord, LiveUnit)]): Set[Coord] =
    reachablePoints(board, p) & targetPoints(board, ts)

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
