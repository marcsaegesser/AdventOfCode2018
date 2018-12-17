package advent

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Day17 {

  def day17(): Unit = {
    val result = run(loadData("data/Day17.txt"))
    println(s"Day17.part1 = ${part1(result)}")
    println(s"Day17.part2 = ${part2(result)}")
  }

  def part1(tiles: Tiles): Int = {
    val (tl, _) = bounds(tiles)
    tiles
      .collect { case (c, t) if t == Water || t == Wet => (c, t) }
      .filterKeys(_.y >= tl.y)
      .size
  }

  def part2(tiles: Tiles): Int = {
    tiles
      .collect { case (c, t) if t == Water => (c, t) }
      .size
  }

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x+other.x, y+other.y)
    def up: Coord = Coord(x, y-1)
    def down: Coord = Coord(x, y+1)
    def left: Coord = Coord(x-1, y)
    def right: Coord = Coord(x+1, y)
  }

  implicit val coordOrdering = Ordering.by((c: Coord) => (c.y, c.x))

  sealed trait Tile
  case object Clay   extends Tile
  case object Water  extends Tile
  case object Wet    extends Tile
  case object Spring extends Tile
  case object Sand   extends Tile

  type Tiles = Map[Coord, Tile]

  sealed trait Operation
  case class Fall(p: Coord) extends Operation
  case class Flow(p: Coord) extends Operation
  val initialOperations: Set[Operation] = Set(Fall(Coord(500, 0)))

  @tailrec
  def run(tiles: Tiles, ops: Set[Operation]=initialOperations, n: Int = 0): Tiles = {
    if(n%1000 == 0)
      println(s"$n:  ${ops.size}")

    if(ops.isEmpty) tiles
    else {
      val (h, t) = (ops.head, ops.tail)
      val (ts, os) = runOperation(tiles, h)
      run(ts, t ++ os, n+1)
    }
  }

  def runOperation(tiles: Tiles, op: Operation): (Tiles, Set[Operation]) = {
    op match {
      case Fall(p) => runFall(tiles, p)
      case Flow(p) => runFlow(tiles, p)
    }
  }

  def runFall(tiles: Tiles, p: Coord): (Tiles, Set[Operation]) = {
    val colTiles = tiles.filter { case (Coord(x, y), t) => x == p.x && y > p.y && (t == Clay || t == Water) }.toList.sortBy(_._1)

    if(colTiles.isEmpty) {
      (fillVert(tiles, Wet, p), Set())
    } else {
      val bottom = Coord(p.x, colTiles.minBy(_._1)._1.y-1)
      (fillVert(tiles, Wet, p.down, bottom), Set(Flow(bottom)))
    }
  }

  def runFlow(tiles: Tiles, p: Coord): (Tiles, Set[Operation]) = {
    findConstraints(tiles, p) match {
      case (Closed(l), Closed(r)) => (fillHoriz(tiles, Water, l.right, r.left), Set(Flow(p.up)))
      case (Closed(l), Open(r))   => (fillHoriz(tiles, Wet,   l.right, r.right), Set(Fall(r.right)))
      case (Open(l),   Closed(r)) => (fillHoriz(tiles, Wet,   l.left,  r.left), Set(Fall(l.left)))
      case (Open(l),   Open(r))   => (fillHoriz(tiles, Wet,   l.left,  r.right), Set(Fall(r.right), Fall(l.left)))
    }
  }

  def fall(tiles: Tiles, p: Coord): Tiles = {
    val colTiles = tiles.filter { case (Coord(x, y), t) => x == p.x && y > p.y && (t == Clay || t == Water) }.toList.sortBy(_._1)

    if(colTiles.isEmpty) {
      fillVert(tiles, Wet, p)
    } else {
      val bottom = Coord(p.x, colTiles.minBy(_._1)._1.y-1)
      flow(fillVert(tiles, Wet, p.down, bottom), bottom)
    }
  }

  sealed trait EndConstraint
  case class Closed(at: Coord) extends EndConstraint
  case class Open(at: Coord) extends EndConstraint

  def flow(tiles: Tiles, p: Coord): Tiles = {
    findConstraints(tiles, p) match {
      case (Closed(l), Closed(r)) => flow(     fillHoriz(tiles, Water, l.right, r.left),  p.up)
      case (Closed(l), Open(r))   => fall(     fillHoriz(tiles, Wet,   l.right, r.right), r.right)
      case (Open(l),   Closed(r)) => fall(     fillHoriz(tiles, Wet,   l.left,  r.left),  l.left)
      case (Open(l),   Open(r))   => fall(fall(fillHoriz(tiles, Wet,   l.left,  r.right), r.right), l.left)
    }
  }

  def findConstraints(tiles: Tiles, p: Coord): (EndConstraint, EndConstraint) = {
    val rowTiles = tiles.filter { case (Coord(x, y), t) => y == p.y && (t == Clay || t == Water) }.toList.sortBy(_._1)
    val toLeft = rowTiles.filter(_._1.x < p.x).map(_._1)
    val toRight = rowTiles.filter(_._1.x > p.x).map(_._1)
    val leftEndBase = findLeftEnd(tiles, p.down)
    val rightEndBase = findRightEnd(tiles, p.down)
    val leftEnd =
      if(toLeft.isEmpty)                    Open(leftEndBase.up)
      else if(leftEndBase.x <= toLeft.max.x) Closed(toLeft.max)
      else                                  Open(leftEndBase.up)

    val rightEnd =
      if(toRight.isEmpty)                     Open(rightEndBase.up)
      else if(rightEndBase.x >= toRight.min.x) Closed(toRight.min)
      else                                    Open(rightEndBase.up)
    (leftEnd, rightEnd)
  }

  def findLeftEnd(tiles: Tiles, p: Coord) ={
    def helper(prev: Coord, remaining: SortedSet[Coord]): Coord = {
      if(remaining.isEmpty) prev
      else if(Math.abs(prev.x-remaining.last.x) == 1) helper(remaining.last, remaining.init)
      else prev
    }

    val toLeft = tiles.filter { case (Coord(x, y), t) => y == p.y && x <= p.x && (t == Clay || t == Water) }.map(_._1).to[SortedSet]
    if(toLeft.size == 1) toLeft.last
    else helper(toLeft.last, toLeft.init)
  }

  def findRightEnd(tiles: Tiles, p: Coord) ={
    def helper(prev: Coord, remaining: SortedSet[Coord]): Coord = {
      if(remaining.isEmpty) prev
      else if(Math.abs(prev.x-remaining.head.x) == 1) helper(remaining.head, remaining.tail)
      else prev
    }

    val toRight = tiles.filter { case (Coord(x, y), t) => y == p.y && x >= p.x && (t == Clay || t == Water) }.map(_._1).to[SortedSet]
    if(toRight.size == 1) toRight.head
    else helper(toRight.head, toRight.tail)
  }


  def fillHoriz(tiles: Tiles, tile: Tile, from: Coord, to: Coord): Tiles =
    tiles ++ (from.x to to.x).map(x => (Coord(x, to.y), tile))

  def fillVert(tiles: Tiles, tile: Tile, from: Coord, to: Coord): Tiles =
    tiles ++ (from.y to to.y).map{ y => (Coord(from.x, y), tile) }

  def fillVert(tiles: Tiles, tile: Tile, from: Coord): Tiles = {
    val (_, Coord(_, by)) = bounds(tiles)
    tiles ++ (from.y+1 to by).map{ y => (Coord(from.x, y), tile) }
  }

  def bounds(tiles: Tiles): (Coord, Coord) =
    tiles.keys.foldLeft((Coord(Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue))) { case ((min, max), Coord(x, y)) =>
      (
        Coord(Math.min(min.x, x), Math.min(min.y, y)),
        Coord(Math.max(max.x, x), Math.max(max.y, y))
      )
    }

  def showTile(t: Tile): String =
    t match {
      case Clay   => "#"
      case Water  => "~"
      case Wet    => "|"
      case Spring => "+"
      case Sand   => "."
    }

  def showTiles(tiles: Tiles): String = {
    def showRow(y: Int, x1: Int, x2: Int): String =
      ((x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) => s ++= showTile(tiles.getOrElse(Coord(x, y), Sand)) }).mkString

    val (tl, br) = bounds(tiles)
    ((for {
      y <- tl.y to br.y
      r =  showRow(y, tl.x, br.x)
    } yield r).mkString("\n"))
  }

  val columnRegex = """x=(\d+),\s+y=(\d+)..(\d+)""".r
  def parseColumn(line: String): List[(Coord, Tile)] =
    line match {
      case columnRegex(x, y0, y1) => (y0.toInt to y1.toInt).map(y => (Coord(x.toInt, y), Clay)).toList
    }

  val rowRegex = """y=(\d+),\s+x=(\d+)..(\d+)""".r
  def parseRow(line: String): List[(Coord, Tile)] =
    line match {
      case rowRegex(y, x0, x1) => (x0.toInt to x1.toInt).map(x => (Coord(x, y.toInt), Clay)).toList
    }

  def parseLine(line: String):  List[(Coord, Tile)] =
    if(line.startsWith("x")) parseColumn(line)
    else                     parseRow(line)

  def loadData(file: String): Tiles =
    (io.Source.fromFile(file)
      .getLines().filterNot(_.isEmpty)
      .map(parseLine)
      .flatten
      .toMap)

}
