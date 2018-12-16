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
  sealed trait LiveUnit extends Unit { def hp: Int; def power: Int; def newHP(x: Int): LiveUnit }
  case object Wall                   extends Unit
  case object Open                   extends Unit
  case class Elf(hp: Int, power: Int)    extends LiveUnit { def newHP(x: Int) = Elf(x, power) }
  case class Goblin(hp: Int, power: Int) extends LiveUnit { def newHP(x: Int) = Goblin(x, power) }

  type Board = Map[Coord, Unit]

  def runCombat(board: Board): (Int, Board) = {
    def helper(round: Int, b: Board): (Int, Board) = {
      Thread.sleep(1000)
      println(s"$round")
      println(s"${showBoard(b)}")
      println("")
      runRound(b) match {
        case (false, nb) => (round-1, nb)
        case (true, nb) => helper(round+1, nb)
      }
    }
    helper(0, board)
  }

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
        runAttack(board, p, u, targets).map { case (at, b) => b }
          .orElse {
            runMove(board, p, u, targets)
              .map{ case (to, b) =>
                val b2 = runAttack(b, to, u, targets)
                b2.map(_._2).getOrElse(b)
              }
          }
          .getOrElse(board)

      (true, next)
    }

  }

  def moveUnit(board: Board, from: Coord, to: Coord, u: Unit): Board =
    board - from + ((to, u))

  def updateBoard(board: Board, at: Coord, u: Unit): Board =
    u match {
      case Open => board - at
      case Wall => ???
      case u => board + ((at, u))

    }

  def runAttack(board: Board, p: Coord, u: LiveUnit, targets: List[(Coord, LiveUnit)]): Option[(Coord, Board)] = {
    targets
      .filter(t => isAdjacent(t._1, p))
      .sortBy(_._1)
      .headOption
      .map { case (at, target) =>
        val newHP = target.hp - u.power
        val newBoard =
          if(newHP <= 0) updateBoard(board, at, Open)
          else          updateBoard(board, at, target.newHP(newHP))
        (at, newBoard)
      }
  }

  object CoordDistOrdering extends Ordering[(Coord, Int)] {
    def compare(a: (Coord, Int), b: (Coord, Int)): Int =
      if(a._2 == b._2) CoordOrdering.compare(a._1, b._1)  // If distances are the same choose coord in Reading Order
      else            a._2 compare b._2                  // If distances are different take shortest distance
  }

  def runMove(board: Board, p: Coord, u: LiveUnit, targets: List[(Coord, LiveUnit)]): Option[(Coord, Board)] = {
      targets.flatMap(t => targetPoints(board, t._1))
        .map(pathToTarget(board, p, _))
        .flatten
        .sortBy(_.size)
        .map(l => (l.head, l.size))
        .sorted(CoordDistOrdering)
        .headOption
        .map { case (coord, _) => (coord, moveUnit(board, p, coord, u)) }
  }

  def chooseStep(board: Board, p: Coord, t: Coord): Coord = {
    adjacencies(p)
      .filter(c => isOpen(board, c))
      .map(c => (c, distance(c, t)))
      .sorted(CoordDistOrdering)
      .head._1
  }

  def isOpen(board: Board, p: Coord): Boolean = !board.isDefinedAt(p)

  def targetPoints(board: Board, p: Coord): List[Coord] =
    adjacencies(p).filter(isOpen(board, _))

  def pathsToTarget(board: Board, p: Coord, t: Coord): Set[List[Coord]] = {
    def helper(accum: Set[List[Coord]], path: List[Coord], c: Coord): Set[List[Coord]] = {
      if(c == t) {
        val newPath = (c :: path).reverse
        val sz = newPath.size
        if(accum.isEmpty) accum + newPath
        else {
          val minSize = accum.map(_.size).min
          if(sz < minSize) Set(newPath)
          else if(sz == minSize) accum + newPath
          else accum
        }
      }
      else if(!accum.isEmpty && path.size > accum.map(_.size).min) accum
      else if(!isOpen(board, c)) accum
      else {
        val adjs = adjacencies(c).filterNot(path.contains).toSet
        adjs.foldLeft(accum) { case (a, n) => a ++ helper(a, c :: path, n) }
      }
    }

    val ps = adjacencies(p).flatMap(c => helper(Set(), List(), c)).toSet
    if(ps.isEmpty) ps
    else {
      val minSz = ps.map(_.size).min
      ps.filter(_.size == minSz)
    }
  }

  def pathToTarget(board: Board, p: Coord, t: Coord): Option[List[Coord]] = {
    pathsToTarget(board, p, t).toList.sortBy(_.head).headOption
  }

  def liveUnits(board: Board): List[(Coord, LiveUnit)] =
    board.toList.collect { case (c, u: LiveUnit) => (c, u) }.sortBy(_._1)

  def targetUnits(board: Board, p: Coord, u: LiveUnit): List[(Coord, LiveUnit)] = {
    u match {
      case e: Elf    => liveUnits(board).collect { case (c, g: Goblin) => (c, g) }
      case g: Goblin => liveUnits(board).collect { case (c, g: Elf) => (c, g) }
    }
  }

  // def targetPoints(board: Board, ts: List[(Coord, LiveUnit)]): Set[Coord] =
  //   ts.flatMap ( t => adjacencies(t._1) ).filter(p => isOpen(board, p)).toSet

  // def reachableTargetPoints(board: Board, p: Coord, ts: List[(Coord, LiveUnit)]): Set[Coord] =
  //   reachablePoints(board, p) & targetPoints(board, ts)

  def showUnit(u: Unit): String =
    u match {
      case Wall        => "#"
      case Open        => "."
      case Elf(_,_)    => "E"
      case Goblin(_,_) => "G"
    }

  def showHP(u: Unit): String =
    u match {
      case Wall        => ""
      case Open        => ""
      case Elf(hp,_)    => s"E($hp)"
      case Goblin(hp,_) => s"G($hp)"
    }

  def showBoard(board: Board): String = {
    def showRow(y: Int, x1: Int, x2: Int): String = {
      val b = (x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) => s ++= showUnit(board.getOrElse(Coord(x,y), Open)) }
      val u = (x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) => s ++= showHP(board.getOrElse(Coord(x,y), Open)) }
      (b ++= " " ++= u).mkString
    }

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
