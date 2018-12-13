package advent

object Day13 {

  def day13(): Unit = {
    val input = loadData("data/Day13.txt")
    println(s"Day13.part1 = ${part1(input)}")
    println(s"Day13.part2 = ${part2(input)}")
  }

  def part1(state: State): Coord =
    runUntilCrash(state) match {
      case Crashed(at, _) => at
      case Running(_, _)  => throw new Exception("Invalid state")
    }

  def part2(state: State): Coord =
    runUntilSingle(state) match {
      case Crashed(_, _) => throw new Exception("Invalid state")
      case Running(cs, _) => cs.head._1
    }

  case class Coord(x: Int, y: Int) {
    def move(dir: Direction): Coord =
      dir match {
        case Up    => Coord(x, y-1)
        case Down  => Coord(x, y+1)
        case Left  => Coord(x-1, y)
        case Right => Coord(x+1, y)
      }
  }

  sealed trait Path
  case object Vertical     extends Path
  case object Horizontal   extends Path
  case object Intersection extends Path
  case object NESW         extends Path
  case object NWSE         extends Path

  type Tracks = Map[Coord, Path]
  type Carts  = Map[Coord, Cart]

  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Right extends Direction
  case object Left  extends Direction

  sealed trait Turn
  case object LeftTurn  extends Turn
  case object Straight  extends Turn
  case object RightTurn extends Turn
  def nextTurn(turn: Turn): Turn =
    turn match {
      case LeftTurn => Straight
      case Straight => RightTurn
      case RightTurn => LeftTurn
    }
  def takeTurn(dir: Direction, turn: Turn): Direction =
    (dir, turn) match {
      case (Up,    LeftTurn)  => Left
      case (Up,    RightTurn) => Right
      case (Down,  LeftTurn)  => Right
      case (Down,  RightTurn) => Left
      case (Left,  LeftTurn)  => Down
      case (Left,  RightTurn) => Up
      case (Right, LeftTurn)  => Up
      case (Right, RightTurn) => Down
      case (_,     Straight)  => dir
    }

  case class Cart(dir: Direction, nextTurn: Turn = LeftTurn)

  sealed trait State
  case class Running(carts: Carts, tracks: Tracks)   extends State
  case class Crashed(crashAt: Coord, tracks: Tracks) extends State

  def runUntilCrash(state: State): State =
    state match {
      case c: Crashed => state
      case r: Running => runUntilCrash(advanceCarts(r))
    }

  def runUntilSingle(state: State): State = {
    state match {
      case c: Crashed                     => throw new Exception("Invalid state")
      case r: Running if r.carts.size == 1 => state
      case r: Running                     => runUntilSingle(advanceCartsRemoveCrash(r))
    }
  }

  def runUntilN(n: Int, state: State): State = {
    state match {
      case c: Crashed                     => throw new Exception("Invalid state")
      case r: Running if r.carts.size <= n => state
      case r: Running                     => runUntilN(n, advanceCartsRemoveCrash(r))
    }
  }

  def advanceCart(cart: Cart, pos: Coord, tracks: Tracks): (Coord, Cart) =
    (cart, tracks(pos)) match {
      case (Cart(d, _), Vertical)     => (pos.move(d),     cart)
      case (Cart(d, _), Horizontal)   => (pos.move(d),     cart)
      case (Cart(Up, _),    NESW)     => (pos.move(Right), cart.copy(dir=Right))
      case (Cart(Down, _),  NESW)     => (pos.move(Left),  cart.copy(dir=Left))
      case (Cart(Left, _),  NESW)     => (pos.move(Down),  cart.copy(dir=Down))
      case (Cart(Right, _), NESW)     => (pos.move(Up),    cart.copy(dir=Up))
      case (Cart(Up, _),    NWSE)     => (pos.move(Left),  cart.copy(dir=Left))
      case (Cart(Down, _),  NWSE)     => (pos.move(Right), cart.copy(dir=Right))
      case (Cart(Left, _),  NWSE)     => (pos.move(Up),    cart.copy(dir=Up))
      case (Cart(Right, _), NWSE)     => (pos.move(Down),  cart.copy(dir=Down))
      case (Cart(d, n), Intersection) => (pos.move(takeTurn(d, n)), Cart(takeTurn(d, n), nextTurn(n)))
      case _                          => throw new Exception("Invalid state")
    }

  def advanceCarts(state: Running): State = {
    def helper(accum: Carts, remaining: List[Coord]): (Carts, List[Coord]) = {
      remaining match {
        case Nil    => (accum, remaining)
        case h :: t =>
          val (nextCoord, nextCart) = advanceCart(state.carts(h), h, state.tracks)
          if(accum.isDefinedAt(nextCoord)) (accum, nextCoord +: remaining)
          else if(t.contains(nextCoord))   (accum, nextCoord +: remaining)
          else                              helper(accum + ((nextCoord, nextCart)), t)
      }
    }

    val coords = cartCoorsInOrder(state.carts)
    val (newCarts, unMoved) = helper(Map.empty[Coord, Cart], coords)

    if(unMoved.isEmpty) state.copy(carts = newCarts)
    else Crashed(unMoved.head, state.tracks)
  }

  def cartCoorsInOrder(carts: Carts): List[Coord] = {
    val byRowAndCol = carts.keys.groupBy(_.y).mapValues(_.toList.sortBy(_.x))
    byRowAndCol.keys.toList.sorted.flatMap(k => byRowAndCol(k))
  }

  def advanceCartsRemoveCrash(state: Running): State = {
    def helper(accum: Carts, remaining: List[Coord]): Carts = {
      remaining match {
        case Nil    => accum
        case h :: t =>
          val (nextCoord, nextCart) = advanceCart(state.carts(h), h, state.tracks)
          if(accum.isDefinedAt(nextCoord)) helper(accum - nextCoord, t)
          else if(t.contains(nextCoord))   helper(accum, t.filterNot(_ == nextCoord))
          else                             helper(accum + ((nextCoord, nextCart)), t)
      }
    }

    val coords = cartCoorsInOrder(state.carts)

    val newCarts = helper(Map.empty[Coord, Cart], coords)
    state.copy(carts = newCarts)
  }

  def parseLine(y: Int, line: String): (List[(Coord, Path)], List[(Coord, Cart)]) = {
    line.zip(Stream.from(0)).foldLeft((List.empty[(Coord, Path)], List.empty[(Coord, Cart)])) { case ((ts, cs), (c, x)) =>
      c match {
        case '-'  => ((Coord(x, y), Horizontal)   +: ts, cs)
        case '|'  => ((Coord(x, y), Vertical)     +: ts, cs)
        case '/'  => ((Coord(x, y), NESW)         +: ts, cs)
        case '\\' => ((Coord(x, y), NWSE)         +: ts, cs)
        case '+'  => ((Coord(x, y), Intersection) +: ts, cs)
        case '>'  => ((Coord(x, y), Horizontal)   +: ts, (Coord(x, y), Cart(Right)) +: cs)
        case '<'  => ((Coord(x, y), Horizontal)   +: ts, (Coord(x, y), Cart(Left))  +: cs)
        case '^'  => ((Coord(x, y), Vertical)     +: ts, (Coord(x, y), Cart(Up))    +: cs)
        case 'v'  => ((Coord(x, y), Vertical)     +: ts, (Coord(x, y), Cart(Down))  +: cs)
        case ' '  => (ts, cs)
      }
    }
  }


  def loadData(file: String): State = {
    val (ts, cs) =
      io.Source.fromFile(file)
        .getLines()
        .zip(Stream.from(0).toIterator)
        .map { case (l, r) => parseLine(r, l) }
        .foldLeft((List.empty[(Coord, Path)], List.empty[(Coord, Cart)])) { case ((ts, cs), (p, c)) => (ts ++ p, cs ++ c) }

    Running(cs.toMap, ts.toMap)
  }

  // def showDirection(dir: Direction): String =
  //   dir match {
  //     case Up    => "^"
  //     case Down  => "v"
  //     case Left  => "<"
  //     case Right => ">"
  //   }

  // def showCart(cart: Cart): String = showDirection(cart.dir)

  // def showTrack(track: Path): String =
  //   track match {
  //     case Vertical => "|"
  //     case Horizontal => "-"
  //     case Intersection => "+"
  //     case NESW => "/"
  //     case NWSE => "\\"

  //   }

  // def showRunningState(state: Running): String = {
  //   def showRow(y: Int): String = {
  //     // val maxCartX = state.carts.keys.filter(_.y == y).map(_.x).max
  //     // val maxTrackX = state.tracks.keys.filter(_.y == y).map(_.x).max
  //     // val maxX = Math.max(maxCartX, maxTrackX)
  //     val maxX = state.tracks.keys.filter(_.y == y).map(_.x).max
  //     val b = StringBuilder.newBuilder
  //       (for {
  //         x <- 0 to maxX
  //         coord = Coord(x, y)
  //         c =  state.carts.get(coord)
  //         t =  state.tracks.get(coord)
  //       } yield (c, t)).map { _ match {
  //         case (Some(c), Some(t)) => b ++= showCart(c)
  //         case (Some(c), None)    => b ++= showCart(c)
  //         case (None, Some(t))    => b ++= showTrack(t)
  //         case (None, None)       => b ++= " "
  //       }
  //       }

  //     b.mkString
  //   }

  //   val maxY = state.tracks.keys.map(_.y).max
  //   (for {
  //     y <- 0 to maxY
  //   } yield showRow(y)).mkString("\n")
  // }

  // def showState(state: State): String =
  //   state match {
  //     case c: Crashed => "Crashed"
  //     case r: Running => showRunningState(r)
  //   }

  // def showCartState(state: State): Unit = {
  //   state match {
  //     case Crashed(at, ts) => println(s"Crashed at $at.  ${ts(at)}")
  //     case Running(cs, ts) =>
  //       val coords = cartCoorsInOrder(cs)
  //       coords.foreach { c => println(s"$c:  ${cs(c)}, ${ts(c)}") }
  //   }
  //   println("")
  // }
}
