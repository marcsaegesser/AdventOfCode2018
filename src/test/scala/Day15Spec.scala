package advent

import org.scalatest._

class Day15Spec extends WordSpec with Matchers {
  import Day15._

  "A puzzle solver" should {
    "choose the correct step for a unit" in {
      for {
        (b, i, e) <- testBoards
      } yield runMoveTest(b, i, e)
    }

    "count the correct number of rounds" in {
      val b = """####
                |##E#
                |#GG#
                |####""".stripMargin
      val board = parseBoard(b)
      val (r, _) = runCombat(board)
      r shouldEqual 67
    }
  }

  def runMoveTest(b: String, index: Int, expected: Option[Coord]): Assertion = {
    val board = parseBoard(b)
    val live = liveUnits(board)
    val p = live(index)._1
    val u = live(index)._2
    val targets = targetUnits(board, p, u)
    val tPts = targets.flatMap(t => targetPoints(board, t._1)).toSet
    val c = chooseStep(board, p, u, tPts)

    c shouldEqual expected
  }

  val testBoards = List[(String, Int, Option[Coord])](
    ("""#######
       |#.E..G#
       |#.#####
       |#G#####
       |#######""".stripMargin, 0, Some(Coord(3, 1))),
    ("""#######
       |#.E...#
       |#.#..G#
       |#.###.#
       |#E#G#G#
       |#...#G#
       |#######""".stripMargin, 0, Some(Coord(3, 1))),
    (
      """#########
        |#G......#
        |#.E.#...#
        |#..##..G#
        |#...##..#
        |#...#...#
        |#.G...G.#
        |#.....G.#
        |#########""".stripMargin, 4, Some(Coord(5, 6))
    ),
    (
      """#########
        |#G......#
        |#.E.#...#
        |#..##..G#
        |#...##..#
        |#...#...#
        |#.G...G.#
        |#.....G.#
        |#########""".stripMargin, 1, Some(Coord(2, 1))
    ),
    (
      """#######
        |#G..#E#
        |#E#E.E#
        |#G.##.#
        |#...#E#
        |#...E.#
        |#######""".stripMargin, 6, None
    ),
    (
      """##############
        |#............#
        |#....#####...#
        |#....#####.G.#
        |#..E.#####...#
        |#....#####.G.#
        |#....#####...#
        |#............#
        |##############""".stripMargin, 1, Some(Coord(3, 3))
    ),
    (
      """##############
        |#.......E....#
        |#....#####...#
        |#....#####.G.#
        |#..E.#####...#
        |#....#####.G.#
        |#....#####...#
        |#............#
        |##############""".stripMargin, 2, Some(Coord(4, 4))
    ),
    (
      """#########
        |#.......#
        |#.E.G.E.#
        |#.......#
        |#########""".stripMargin, 1, Some(Coord(3, 2))
    ),
    (
      """#########
        |#....E..#
        |#...G...#
        |#.......#
        |#########""".stripMargin, 1, Some(Coord(4, 1))
    ),
    (
      """#########
        |#....E..#
        |#.E.G.E.#
        |#.......#
        |#...E...#
        |#########""".stripMargin, 2, Some(Coord(4, 1))
    ),
    (
      """#########
        |#...GE..#
        |#.E.G...#
        |#.......#
        |#...E...#
        |#########""".stripMargin, 3, Some(Coord(3, 2))
    ),
    (
      """#########
        |#...GE..#
        |#.EGG...#
        |#.......#
        |#...E...#
        |#########""".stripMargin, 4, Some(Coord(5, 2))
    ),
    (
      """#########
        |#...G...#
        |#.EGG...#
        |#.......#
        |#...E...#
        |#########""".stripMargin, 3, Some(Coord(4, 3))
    ),
    (
      """#########
        |#...G...#
        |#.EGG...#
        |#.......#
        |#...E...#
        |#########""".stripMargin, 3, Some(Coord(4, 3))
    ),
    (
      """#########
        |#.......#
        |#...#E..#
        |#..G#...#
        |#.......#
        |#########""".stripMargin, 0, Some(Coord(5, 1))
    ),
    (
      """#######
        |#E..G.#
        |#...#.#
        |#.G.#G#
        |#######""".stripMargin, 0, Some(Coord(2, 1))
    ),
    (
      """################
        |#.......G......#
        |#G.............#
        |#..............#
        |#....###########
        |#....###########
        |#.......EG.....#
        |################""".stripMargin, 0, Some(Coord(7, 1))
    ),
    (
      """################
        |#.......G......#
        |#G.............#
        |#..............#
        |#....###########
        |#....###########
        |#.......EG.....#
        |################""".stripMargin, 1, Some(Coord(2, 2))
    ),
    (
      """#######
        |#..E#G#
        |#.....#
        |#G#...#
        |#######""".stripMargin, 0, Some(Coord(2, 1))
    ),
    (
      """###########
        |#.........#
        |#..E..G..G#
        |#.........#
        |###########""".stripMargin, 2, Some(Coord(9, 1))
    ),
    (
      """###########
        |#.........#
        |#....E....#
        |#....#....#
        |#....G....#
        |#.........#
        |###########""".stripMargin, 0, Some(Coord(4, 2))
    ),
    (
      """#######
        |#.E...#
        |#.....#
        |#...G.#
        |#######""".stripMargin, 0, Some(Coord(3, 1))
    )

  )
}


