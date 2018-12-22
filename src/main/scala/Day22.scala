package advent

import scala.annotation.tailrec

object Day22 {
  case class Coord(x: Int, y: Int) {
    def adjacencies: List[Coord] = List(up, down, left, right)
    def up:    Coord = Coord(x, y-1)
    def down:  Coord = Coord(x, y+1)
    def left:  Coord = Coord(x-1, y)
    def right: Coord = Coord(x+1, y)
  }

  sealed trait Tool
  case object Climbing extends Tool
  case object Torch    extends Tool
  case object Neither  extends Tool

  type Time = Int

  type RouteState = Map[Coord, Map[Tool, Time]]
  val initialRouteState: RouteState = Map(Coord(0,0) -> Map(Torch -> 0, Climbing -> 7))

  case class Rescue(map: CaveMap, target: Coord, state: RouteState)

  def runN(until: Int, map: CaveMap): RouteState = {
    @tailrec
    def helper(n: Int, state: RouteState): RouteState = {
      println(s"$n")
      if(n == until) state
      else          helper(n+1, nextState(state, map, n))
    }

    helper(0, initialRouteState)
  }

  def nextState(state: RouteState, map: CaveMap, r: Int): RouteState = {
    val newCoords = diagnalFrom(r).map(c => (c, emptyAdjacents(state, c)))
    val (changed, newState) = updateLocations(state, map, newCoords)
    backtrack(newState, map, changed)
  }

  def backtrack(state: RouteState, map: CaveMap, newCoords: Set[Coord]): RouteState = {
    if(newCoords.isEmpty) state
    else {
      val nonEmptyCoords = newCoords.map(p => (p, nonEmptyAdjacents(state, p)))
      val (changedCoords, newState) = updateLocations(state, map, nonEmptyCoords)
      backtrack(newState, map, changedCoords)
    }
  }

  def updateLocations(state: RouteState, map: CaveMap, coords: Set[(Coord, Set[Coord])]): (Set[Coord], RouteState) =
    coords.foldLeft((Set.empty[Coord], state)) { case ((cs, s), (from, to)) =>
      to.foldLeft((cs, s)) { case ((cs, s), t) =>
        val (isChange, ns) = updateLocation(s, map, from, t)
        if(isChange) (cs + t, ns)
        else         (cs,     ns)
      }
    }

  def updateLocation(state: RouteState, map: CaveMap, from: Coord, to: Coord): (Boolean, RouteState) = {
    val newState: RouteState =
      (map(to), state(from).get(Climbing), state(from).get(Torch), state(from).get(Neither)) match {
        case (Rocky, pc, pt, _) =>
          val stateTo = state.getOrElse(to, Map(Climbing -> Int.MaxValue, Torch -> Int.MaxValue))
          val nC = pc.map(_+1)
          val nT = pt.map(_+1)
          val newClimbing = Math.min(nC.orElse(nT.map(_+7)).get, stateTo(Climbing))
          val newTorch    = Math.min(nT.orElse(nC.map(_+7)).get, stateTo(Torch))
          state.updated(to, Map((Climbing -> newClimbing), (Torch -> newTorch)))
        case (Wet, pc, _, pn) =>
          val stateTo = state.getOrElse(to, Map(Climbing -> Int.MaxValue, Neither -> Int.MaxValue))
          val nC = pc.map(_+1)
          val nN = pn.map(_+1)
          val newClimbing = Math.min(nC.orElse(nN.map(_+7)).get, stateTo(Climbing))
          val newNeither  = Math.min(nN.orElse(nC.map(_+7)).get, stateTo(Neither))
          state.updated(to, Map((Climbing -> newClimbing), (Neither -> newNeither)))
        case (Narrow, _, pt, pn) =>
          val stateTo = state.getOrElse(to, Map(Torch -> Int.MaxValue, Neither -> Int.MaxValue))
          val nT = pt.map(_+1)
          val nN = pn.map(_+1)
          val newTorch    = Math.min(nT.orElse(nN.map(_+7)).get, stateTo(Torch))
          val newNeither  = Math.min(nN.orElse(nT.map(_+7)).get, stateTo(Neither))
          state.updated(to, Map((Torch -> newTorch), (Neither -> newNeither)))
      }
    (state != newState, newState)
  }

  def diagnalFrom(i: Int): Set[Coord] =
    (0 to i).zip(i to 0 by -1).map { case (x, y) => Coord(x, y) }.toSet

  def emptyAdjacents(state: RouteState, p: Coord): Set[Coord] =
    p.adjacencies.filter(c => c.x >= 0 && c.y >= 0 && !state.get(c).isDefined).toSet

  def nonEmptyAdjacents(state: RouteState, p: Coord): Set[Coord] =
    p.adjacencies.filter(c => c.x >= 0 && c.y >= 0 && state.get(c).isDefined).toSet

  def showTool(t: Tool): String =
    t match {
      case Climbing => "C"
      case Torch    => "T"
      case Neither  => "N"
    }

  def showNode(node: Option[Map[Tool, Time]]): String = {
    node.map { n =>
      val choice = n.toList.sortBy(_._2).head
      f"${showTool(choice._1)}${choice._2}%02d"
    }.getOrElse("   ")
  }

  def showRoute(state: RouteState): String = {
    def showRow(y: Int): String = {
      state.filterKeys(_.y == y).toList.sortBy(_._1.x).map(x => showNode(Some(x._2))).mkString(" ")
    }

    val maxY = state.keys.toList.sortBy(_.y).reverse.head.y

    (for {
      y <- (0 to maxY).toList
      r =  showRow(y)
    } yield r).mkString("\n")
  }

  def showRoute(state: RouteState, tl: Coord, br: Coord): String = {
    def showRow(y: Int, x1: Int, x2: Int): String = {
      (x1 to x2).map(x => showNode(state.get(Coord(x, y)))).mkString(" ")
    }

    (for {
      y <- tl.y to br.y
      r =  showRow(y, tl.x, br.x)
    } yield r).mkString("\n")
  }

  sealed trait LocationType { def riskLevel: Int }
  case object Rocky  extends LocationType  { val riskLevel = 0 }
  case object Wet    extends LocationType  { val riskLevel = 1 }
  case object Narrow extends LocationType  { val riskLevel = 2 }

  type GeologicIndex = Int
  type ErosionLevel = Int

  case class Cave(map: Map[Coord, ErosionLevel], depth: Int, target: Coord)
  def mkCave(depth: Int, target: Coord): Cave =
    Cave(Map.empty[Coord, ErosionLevel], depth, target)

  type CaveMap = Map[Coord, LocationType]

  val erosionMod = 20183

  def erosionLevel(cave: Cave, p: Coord): (ErosionLevel, Cave) = {
    if(cave.map.isDefinedAt(p)) (cave.map(p), cave)  // Used memoized value
    else { //  Compute erosionlevel at p
      val (e, c) =
        if(p == Coord(0, 0))      (cave.depth % erosionMod, cave)
        else if(p == cave.target) (cave.depth % erosionMod, cave)
        else if(p.x == 0)         (((p.y * 48271) + cave.depth) % erosionMod, cave)
        else if(p.y == 0)         (((p.x * 16807) + cave.depth) % erosionMod, cave)
        else {
          val (e1, c1) = erosionLevel(cave, p.up)
          val (e2, c2) = erosionLevel(c1, p.left)
          (((e1 * e2) + cave.depth) % erosionMod, c2)
        }

      (e, c.copy(map=c.map + ((p, e))))
    }
  }

  def erosionLevelToType(level: ErosionLevel): LocationType =
    (level%3)  match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
    }

  def riskLevel(cave: Cave): (Int, Cave) = {
    val regionCoords =
      for {
        x <- 0 to cave.target.x
        y <- 0 to cave.target.y
      } yield Coord(x, y)

    regionCoords.foldLeft((0, cave)) { case ((r, c), p) =>
      val (e, cc) = erosionLevel(c, p)
      (r + e%3, cc)
    }
  }

  def mkCaveMap(cave: Cave): CaveMap = {
    val regionCoords =
      for {
        x <- 0 to cave.target.x * 5
        y <- 0 to cave.target.y * 5
      } yield Coord(x, y)

    regionCoords.foldLeft((Map.empty[Coord, LocationType], cave)) { case ((m, c), p) =>
      val (e, cc) = erosionLevel(c, p)
      (m + ((p, erosionLevelToType(e%3))), cc)
    }._1
  }

  def mkCaveMap(cave: Cave, n: Int): CaveMap = {
    val regionCoords =
      for {
        x <- 0 to n
        y <- 0 to n
      } yield Coord(x, y)

    regionCoords.foldLeft((Map.empty[Coord, LocationType], cave)) { case ((m, c), p) =>
      val (e, cc) = erosionLevel(c, p)
      (m + ((p, erosionLevelToType(e%3))), cc)
    }._1
  }

  def getRegion(cave: Cave, p: Coord): (LocationType, Cave) = {
    val (e, c) = erosionLevel(cave, p)
    (erosionLevelToType(e), c)
  }

  def availableRegions(cave: Cave, p: Coord): (List[(Coord, LocationType)], Cave) = {
    val adj = p.adjacencies.filter { case Coord(x, y) => x >= 0 && y >= 0 }
    adj.foldLeft((List.empty[(Coord, LocationType)], cave)) { case ((accum, c), p) =>
      val (l, cc) = getRegion(cave, p)
      ((p, l) :: accum, cc)
    }
  }


  val caveRegex = """depth: (\d+) target: (\d+),(\d+)""".r

  def parseCave(s: String): Cave =
    s match {
      case caveRegex(d, tx, ty) => mkCave(d.toInt, Coord(tx.toInt, ty.toInt))
    }

  def loadData(file: String): Cave =
    parseCave(io.Source.fromFile(file).getLines().mkString(" "))
}
