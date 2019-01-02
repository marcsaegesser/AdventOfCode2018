package advent

object Day20 {

  def day20(): Unit = {
    val maze = scanMaze(loadData("data/Day20.txt"))
    val distances = computeDistances(maze)

    println(s"Day20.part1 = ${distances.values.max}")
    println(s"Day20.part2 = ${distances.values.filter(_ >= 1000).size}")
  }

  def part1(maze: Maze): Int = {
    computeDistances(maze).values.max
  }

  def part2(maze: Maze): Int = {
    computeDistances(maze).values.filter(_ >= 1000).size
  }

  case class Coord(x: Int, y: Int) {
    def adjacencies: List[Coord] = List(north, east, south, west)
    def north: Coord = Coord(x,   y-1)
    def east: Coord  = Coord(x+1, y)
    def south: Coord = Coord(x,   y+1)
    def west: Coord  = Coord(x-1, y)
  }

  sealed trait Direction
  case object N extends Direction
  case object E extends Direction
  case object S extends Direction
  case object W extends Direction

  sealed trait LocationType
  case object Room extends LocationType
  case object Wall extends LocationType
  case object DoorH extends LocationType
  case object DoorV extends LocationType

  val doors: Set[LocationType] = Set(DoorH, DoorV)

  def showLocationType(t: LocationType): String =
    t match {
      case Room => "."
      case DoorH => "-"
      case DoorV => "|"
      case Wall => "#"
    }

  type Maze = Map[Coord, LocationType]
  val initialMaze: Maze = Map((Coord(0, 0) -> Room))

  def furthestRoom(maze: Maze): Int = {
    computeDistances(maze).values.max
  }

  /** Compute the shortest distance to all reachable rooms starting
    * from the initial location (0, 0).
    *
    * Does a breadth first search of reachable rooms labeling each coordinate
    * with it's distance.
    */
  def computeDistances(maze: Maze): Map[Coord, Int] = {
    def helper(accum: Map[Coord, Int], next: Set[Coord], dist: Int): Map[Coord, Int] = {
      if(next.isEmpty) accum
      else {
        val nextAccum = next.foldLeft(accum) { case (m, c) => m + ((c, dist)) }
        val nextRound = next.foldLeft(Set.empty[Coord]) { case (nn, c) => nn ++ nextRooms(maze, c, accum) }
        helper(nextAccum, nextRound, dist+1)
      }
    }

    helper(Map(), Set(Coord(0, 0)), 0)
  }


  /** Find reachable rooms that have not already been labeled with a distance.
    */
  def nextRooms(maze: Maze, p: Coord, mapped: Map[Coord, Int]): Set[Coord] = {
    reachableRooms(maze, p) &~ mapped.keySet   // Set difference
  }

  /** Determine all the rooms reachable from the given coordinate.
    */
  def reachableRooms(maze: Maze, p: Coord): Set[Coord] = {
    val n = maze.get(p.north).map(l => doors.contains(l))
    val e = maze.get(p.east).map(l => doors.contains(l))
    val s = maze.get(p.south).map(l => doors.contains(l))
    val w = maze.get(p.west).map(l => doors.contains(l))

    Set(
      n.flatMap(isDoor => if(isDoor) Some(p.north.north) else None ),
      e.flatMap(isDoor => if(isDoor) Some(p.east.east) else None),
      s.flatMap(isDoor => if(isDoor) Some(p.south.south) else None),
      w.flatMap(isDoor => if(isDoor) Some(p.west.west) else None)
    ).flatten
  }

  // Given a regex compute the maze that it generates.
  def scanMaze(regex: Regex): Maze = {
    def helper(maze: Maze, p: Coord, segments: List[Segment]): Maze = {
      segments match {
        case Nil => maze
        case s :: rest => val (m, pp) = scanSegment(maze, p, s); helper(m, pp, rest)
      }
    }

    helper(initialMaze, Coord(0, 0), regex)
  }

  def scanSegments(maze: Maze, p: Coord, regex: Regex): (Maze, Coord) = {
    def helper(maze: Maze, p: Coord, segments: List[Segment]): (Maze, Coord) = {
      segments match {
        case Nil => (maze, p)
        case s :: rest => val (m, pp) = scanSegment(maze, p, s); helper(m, pp, rest)
      }
    }

    helper(maze, p, regex)
  }

  def scanSegment(m: Maze, p: Coord, segment: Segment) =
    segment match {
      case r: Route => scanRoute(m, p, r)
      case c: Choice => scanChoice(m, p, c)
    }

  def scanRoute(maze: Maze, p: Coord, route: Route): (Maze, Coord) =
    route.route.foldLeft((maze, p)) { case ((m, p), d) => moveDirection(m, p, d) }

  def scanChoice(maze: Maze, p: Coord, choice: Choice): (Maze, Coord) = {
    choice.choices.foldLeft((maze,p)) { case ((m, _), s) => scanSegments(m, p, s) }

  }

  def moveDirection(maze: Maze, p: Coord, d: Direction): (Maze, Coord) =
    d match {
      case N => (maze + ((p.north, DoorH)) + ((p.north.west, Wall)) + ((p.north.east, Wall)) + ((p.north.north, Room)), p.north.north)
      case E => (maze + ((p.east, DoorV)) + ((p.east.north, Wall)) + ((p.east.south, Wall))  + ((p.east.east, Room)),   p.east.east)
      case S => (maze + ((p.south, DoorH)) + ((p.south.west, Wall)) + ((p.south.east, Wall)) + ((p.south.south, Room)), p.south.south)
      case W => (maze + ((p.west, DoorV))  + ((p.west.north, Wall)) + ((p.west.south, Wall)) + ((p.west.west, Room)),   p.west.west)
    }

  sealed trait Segment
  case class Route(route: List[Direction]) extends Segment
  case class Choice(choices: List[Regex]) extends Segment
  case class Sequence(segments: List[Regex])
  type Regex = List[Segment]

  def showRoute(r: Route): String = r.route.mkString

  def showChoice(c: Choice): String =
    c.choices.map(_.map(showSegment).mkString).mkString("(","|", ")")

  def showSegment(s: Segment): String =
    s match {
      case r: Route  => showRoute(r)
      case c: Choice => showChoice(c)
    }

  def showRegex(regex: Regex): String =
    regex.map(showSegment).mkString("^", "", "$")

  def parseRoute(s: List[Char]): (Route, List[Char]) = {
    def helper(r: List[Direction], remaining: List[Char]): (Route, List[Char]) = {
      remaining match {
        case Nil => throw new Exception("EOS while parsing route.")
        case 'N' :: ss => helper(N :: r, ss)
        case 'E' :: ss => helper(E :: r, ss)
        case 'S' :: ss => helper(S :: r, ss)
        case 'W' :: ss => helper(W :: r, ss)
        case _ :: ss   => (Route(r.reverse), remaining)
      }
    }

    helper(List(), s)
  }

  def parseChoice(s: List[Char]): (Choice, List[Char]) = {
    def helper(accum: List[Regex], current: List[Segment], remaining: List[Char]): (Choice, List[Char]) = {
      remaining match {
        case ')' :: ss => (Choice((current.reverse +: accum).reverse), ss)
        case '|' :: ss => helper(current.reverse +: accum, List(), ss)
        case _   :: ss => val (s, rest) = parseSegment(remaining); helper(accum, s +: current, rest)
        case Nil       => throw new Exception("EOS while parsing choice")
      }
    }

    helper(List(), List(), s)
  }

  def parseSegment(s: List[Char]): (Segment, List[Char]) = {
    s match {
      case '(' :: ss => parseChoice(ss)
      case _   :: ss => parseRoute(s)
      case Nil       => throw new Exception("EOS while parsing segment")
    }
  }


  def parseRegex(s: String): Regex = {
    def helper(accum: List[Segment], remaining: List[Char]): Regex =
      remaining match {
        case Nil => throw new Exception("Malformed input. No terminator.")
        case '^' :: ss => helper(accum, ss)
        case '$' :: ss => accum.reverse
        case _   :: ss => val (s, rest) = parseSegment(remaining); helper(s :: accum, rest)
      }

    helper(List(), s.toList)
  }

  def loadData(file: String): Regex =
    parseRegex(io.Source.fromFile(file).mkString)

  // The following code is used to display a maze. It is only useful for debugging small mazes.
  //

  def bounds(maze: Maze): (Coord, Coord) =
    maze.keys.foldLeft((Coord(Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue))) { case ((min, max), Coord(x, y)) =>
      (
        Coord(Math.min(min.x, x), Math.min(min.y, y)),
        Coord(Math.max(max.x, x), Math.max(max.y, y))
      )
    }

  def addWalls(maze: Maze): Maze = {
    val (tl, br) = bounds(maze)
    val wallsAt =
      for {
        y <- tl.y to br.y
        x <- tl.x to br.x
        if !maze.isDefinedAt(Coord(x, y))
      } yield Coord(x, y)

    wallsAt.foldLeft(maze) { case (m, c) => m + ((c, Wall)) }
  }

  def showMaze(maze: Maze, p: Coord): String = {
    def showRow(y: Int, x1: Int, x2: Int): String =
      ((x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) =>
        val c = Coord(x, y)
        if(c == p) s ++= "X"
        else      s ++= maze.get(c).map(showLocationType).getOrElse("?")
      }).mkString

    val (tl, br) = bounds(maze)
    ((for {
      y <- tl.y to br.y
      r =  showRow(y, tl.x, br.x)
    } yield r).mkString("\n"))
  }

}
