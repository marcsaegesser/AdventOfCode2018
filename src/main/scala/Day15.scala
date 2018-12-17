package advent

import java.util.UUID

object Day15 {

  def part1(board: Board): Int = {
    val (r, b) = runCombat(board)
    r * liveUnits(b).map(_._2.hp).sum
  }

  def part2(board: Board) = {
    val elfCount = countElves(board)

    val (rounds, brd, e, p) =
      Stream.from(4)
        .map { p =>
          val (r, b) = runCombat(updateElfPower(board, p))
          (r, b, countElves(b), p)
        }
        .dropWhile(_._3 < elfCount).head

    val result = rounds * liveUnits(brd).map(_._2.hp).sum
    println(s"result=$result")
    (rounds, brd, e, p)
  }

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x+other.x, y+other.y)
  }
  def distance(a: Coord, b: Coord): Int = Math.abs(b.x - a.x) + Math.abs(a.y - b.y)

  def adjacencies(p: Coord): List[Coord] =
    List(Up, Left, Right, Down).map(p + _)

  def isAdjacent(p: Coord, q: Coord): Boolean =
    p.x == q.x && Math.abs(p.y-q.y) == 1 || p.y == q.y && Math.abs(p.x-q.x) == 1

  implicit val coordOrdering = Ordering.by((c: Coord) => (c.y, c.x))

  val Up    = Coord(0, -1)
  val Right = Coord(1, 0)
  val Down  = Coord(0, 1)
  val Left  = Coord(-1, 0)

  sealed trait Unit
  sealed trait LiveUnit extends Unit { def id: UUID; def hp: Int; def power: Int; def newHP(x: Int): LiveUnit }
  case object Wall                   extends Unit
  case object Open                   extends Unit
  case class Elf(id: UUID, hp: Int, power: Int)    extends LiveUnit { def newHP(x: Int) = Elf(id, x, power) }
  case class Goblin(id: UUID, hp: Int, power: Int) extends LiveUnit { def newHP(x: Int) = Goblin(id, x, power) }

  type Board = Map[Coord, Unit]

  def updateElfPower(board: Board, power: Int): Board =
    board.mapValues { u =>
      u match {
        case e: Elf => e.copy(power=power)
        case _      => u
      }
    }

  def countElves(board: Board): Int =
    board.values.collect { case u: Elf => u }.size

  def moveUnit(board: Board, from: Coord, to: Coord, u: Unit): Board =
    board + ((to, board(from))) - from

  def updateBoard(board: Board, at: Coord, u: Unit): Board =
    u match {
      case Open => board - at
      case Wall => ???
      case u => board + ((at, u))

    }

  def getUnits(board: Board, cs: List[Coord]): List[LiveUnit] =
    cs.map(board.get(_)).flatten.collect { case l: LiveUnit => l }


  def runCombat(board: Board): (Int, Board) = {
    def helper(round: Int, b: Board): (Int, Board) = {
      // println(s"$round")
      // println(s"${showBoard(b)}")
      // println("")
      runRound(b) match {
        case (false, nb)    => (round, nb)
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

  def unitMatches(board: Board, p: Coord, u: LiveUnit): Boolean =
    board.get(p).map {
      _ match {
        case l: LiveUnit => u.id == l.id
        case _           => false
      }
    }.getOrElse(false)

  def runUnit(board: Board, p: Coord, u: LiveUnit): (Boolean, Board) = {
    val targets = targetUnits(board, p, u)
    if(!unitMatches(board, p, u)) (true, board)  // This unit died before we could run it
    else if(targets.isEmpty )     (false, board)  // No targets, we're done
    else {
      val next =
        runAttack(board, p, u, targets)
          .map { case (at, b) => b }         // An attack from p to at, pass along the new board, don't move
          .orElse {
            runMove(board, p, u, targets)    // No attack so move the unit
              .map{ case (to, b) =>          // The unit moved. See if it can attack now
                runAttack(b, to, u, targets)
                  .map(_._2)                 // There was an attack, pass along the new board
                  .getOrElse(b)              // No attack, use the board from the move
              }
          }
          .getOrElse(board)                  // No action possible, board is unchanged

      (true, next)
    }
  }

  def runAttack(board: Board, p: Coord, u: LiveUnit, targets: List[(Coord, LiveUnit)]): Option[(Coord, Board)] = {
    targets
      .filter(t => isAdjacent(t._1, p))       // Find adjacent targets
      .sortBy { case (c, t) => (t.hp, c) }    // Sort by lowest HP, then reading-order
      .headOption                             // Selected target, if any
      .map { case (at, target) =>             // This unit attacks 'target' at location 'at'
        val newHP = target.hp - u.power
        val newBoard =
          if(newHP <= 0) updateBoard(board, at, Open)
          else          updateBoard(board, at, target.newHP(newHP))
        // println(s"""attack:  ($u, $p) -> ($target, $at) ==> ${newBoard.getOrElse(at, " ")}""")
        (at, newBoard)
      }
  }

  def runMove(board: Board, p: Coord, u: LiveUnit, targets: List[(Coord, LiveUnit)]): Option[(Coord, Board)] = {
    chooseStep(board, p, u, targets.flatMap(t => targetPoints(board, t._1)).toSet)   // Compute open target points for targets
      .map { coord =>
        val newBoard = moveUnit(board, p, coord, u)
        // println(s"move:  ($u, $p) -> $coord => ${newBoard(coord)}")
        (coord, newBoard)
      }
  }

  // An expanding shell of reachable coords
  def findClosest(board: Board, p: Coord, targetPoints: Set[Coord]): Option[(Int, Coord)] = {
    def helper(accum: Map[Coord, Int], dist: Int, newCoords: Set[Coord]): Option[(Int, Coord)] =
      if(newCoords.isEmpty) None // No target paths found
      else {
        val hits = newCoords & targetPoints                           // Intersection of next coords with target points
        if(hits.isEmpty) {
          val nextAccum = accum ++ newCoords.map((_, dist))        // Add new coords to shell
          val next = newCoords.map(c => openAdjacencies(board, c)).flatten.filterNot(c => accum.keySet.contains(c))
          helper(nextAccum, dist+1, next)
        } else {
          Some((dist, hits.toList.sorted.head))
        }
      }

    helper(Map.empty[Coord, Int], 0, Set(p))
  }

  def chooseStep(board: Board, p: Coord, u: LiveUnit, tps: Set[Coord]): Option[Coord] = {
    findClosest(board, p, tps).flatMap { case (d, t) =>
      findClosest(board, t, targetPoints(board, p).toSet)
    }.map(_._2)
  }

  def isOpen(board: Board, p: Coord): Boolean = !board.isDefinedAt(p)

  def targetPoints(board: Board, p: Coord): List[Coord] =
    adjacencies(p).filter(isOpen(board, _))

  def openAdjacencies(board: Board, p: Coord): Set[Coord] =
    adjacencies(p).filter(c => isOpen(board, c)).toSet

  def liveUnits(board: Board): List[(Coord, LiveUnit)] =
    board.toList.collect { case (c, u: LiveUnit) => (c, u) }.sortBy(_._1)

  def targetUnits(board: Board, p: Coord, u: LiveUnit): List[(Coord, LiveUnit)] = {
    u match {
      case e: Elf    => liveUnits(board).collect { case (c, g: Goblin) => (c, g) }
      case g: Goblin => liveUnits(board).collect { case (c, e: Elf) => (c, e) }
    }
  }

  def showUnit(u: Unit): String =
    u match {
      case Wall           => "#"
      case Open           => "."
      case Elf(_, _,_)    => "E"
      case Goblin(_, _,_) => "G"
    }

  def showHP(u: Unit): String =
    u match {
      case Wall            => ""
      case Open            => ""
      case Elf(_, hp,_)    => s"E($hp)"
      case Goblin(_, hp,_) => s"G($hp)"
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

  def parseBoard(s: String): Board = {
    s.lines
      .zip(Stream.from(0).toIterator)
      .flatMap{ case (l, y) =>
        l.zip(Stream.from(0)).collect { case (c, x) if c != '.' => (Coord(x, y), parseUnit(c)) } }
      .toMap
  }

  def parseUnit(c: Char): Unit =
    c match {
      case '#' => Wall
      case 'E' => Elf(UUID.randomUUID, 200, 3)
      case 'G' => Goblin(UUID.randomUUID, 200, 3)
    }

}
