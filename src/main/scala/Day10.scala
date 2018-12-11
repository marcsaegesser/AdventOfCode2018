package advent

// import scala.util.Sorting

object Day10 {
  case class Coord(x: Int, y: Int)
  case class Point(x: Int, y: Int, vx: Int, vy: Int)

  def updateUntilSize(count: Int, untilX: Int, untilY: Int, points: List[Point]): (Int, List[Point]) = {
    val (sx, sy) = size(points)
    if(sx <= untilX && sy <= untilY) (count, points)
    else updateUntilSize(count+1, untilX, untilY, updatePoints(points))
  }

  def showAndStep(points: List[Point]): (String, List[Point]) =
    (showPoints(points), updatePoints(points))

  def size(points: List[Point]): (Int, Int) = {
    val (tl, br) = bounds(points)
    (br.x - tl.x, br.y - tl.y)
  }


  def bounds(points: List[Point]): (Coord, Coord) =
    points.foldLeft((Coord(Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue))) { case (a, b) =>
      val minx = if(b.x < a._1.x) b.x else a._1.x
      val miny = if(b.y < a._1.y) b.y else a._1.y
      val maxx = if(b.x > a._2.x) b.x else a._2.x
      val maxy = if(b.y > a._2.y) b.y else a._2.y
      (Coord(minx, miny), Coord(maxx, maxy))
    }

  def showPoints(points: List[Point]): String = {
    def showRow(space: Set[Coord], y: Int, x1: Int, x2: Int): String =
      (x1 to x2).foldLeft(StringBuilder.newBuilder) { case (s, x) => s ++= (if(space.contains(Coord(x, y))) "#" else ".") }.mkString

    val (topLeft, bottomRight) = bounds(points)
    val space = points.map(p => Coord(p.x, p.y)).toSet
    val rows =
      for {
        y <- (topLeft.y to bottomRight.y)
        r =  showRow(space, y, topLeft.x, bottomRight.x)
      } yield r
    rows.mkString("\n")
  }

  def updatePoints(points: List[Point]): List[Point] =
    points.map { p => p.copy(x=p.x+p.vx, y=p.y+p.vy) }

  val pointRegex = """position=<\s*([-0-9]+),\s*([-0-9]+)>\s+velocity=<\s*([-0-9]+),\s*([-0-9]+)>""".r

  def parsePoint(line: String): Point =
    line match {
      case pointRegex(x, y, vx, vy) => Point(x.toInt, y.toInt, vx.toInt, vy.toInt)
    }

  def loadData(file: String): List[Point] =
    io.Source.fromFile(file)
      .getLines().filterNot(_.isEmpty)
      .map(parsePoint)
      .toList

}
