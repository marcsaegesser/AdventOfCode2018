package advent

object Day09 {

  def day09(): Unit = {
    println(s"""Day09.part1 = ${part1("data/Day09.txt")}""")
    println(s"""Day09.part2 = ${part2("data/Day09.txt")}""")

  }

  def part1(file: String): List[Long] =
    loadData(file).map(playGame).map(_.players.toList.max)

  def part2(file: String): List[Long] =
    loadData(file, 100).map(playGame).map(_.players.toList.max)


  case class Game(players: Zipper[Long], marble: Long, maxMarble: Long, board: Zipper[Long]) {
    def hasNext: Boolean = marble <= maxMarble

    def nextState: Game =
      this match {
        case Game(p, m, x, b) if m > x => this
        case Game(p, m, _, b) =>
          if(m % 23 == 0) {
            val (v, bb) = b.moveLeft(7).take
            val pp = p.update(players.focus + m + v).moveRight
            Game(pp, m+1, maxMarble, bb)
          } else {
            Game(p.moveRight, m+1, maxMarble, b.moveRight.addRight(m))
          }
      }
  }

  def playGame(g: Game): Game =
    if(g.hasNext) playGame(g.nextState)
    else          g


  /** A very simple implementation of a circular Zipper with the
    * assumption that everything is NOT EMPTY.
    *
    * This is sufficient for this puzzle but NOT the REAL WORLD.
    */
  case class Zipper[A](l: List[A], focus: A, r: List[A]) {
    def moveRight: Zipper[A] =
      this match {
        case Zipper(l, f, Nil) =>
          val ll = (f :: l).reverse
          Zipper(Nil, ll.head, ll.tail)
        case Zipper(l, f, h :: t) => Zipper(f :: l, h, t)
      }

    def moveRight(n: Int): Zipper[A] =
      if(n == 0) this
      else      moveRight.moveRight(n-1)

    def moveLeft: Zipper[A] =
      this match {
        case Zipper(Nil, f, r) =>
          val rr = (f :: r).reverse
          Zipper(rr.tail, rr.head, Nil)
        case Zipper(h :: t, f, r) => Zipper(t, h, f :: r)
      }

    def moveLeft(n: Int): Zipper[A] =
      if(n == 0) this
      else      moveLeft.moveLeft(n-1)

    def addRight(a: A): Zipper[A] =
      Zipper(focus :: l, a, r)

    def addLeft(a: A): Zipper[A] =
      Zipper(l, a, focus :: r)

    def take: (A, Zipper[A]) =
      this match {
        case Zipper(l, f, Nil) =>
          val ll = l.reverse
          (f, Zipper(Nil, ll.head, ll))
        case Zipper(l, f, h :: t) => (f, Zipper(l, h, t))
      }

    def update(a: A): Zipper[A] = Zipper(l, a, r)

    def toList: List[A] =
      focus :: r ++ l.reverse
  }

  object Zipper {
    def apply[A](a: A): Zipper[A] = Zipper(Nil, a, Nil)

    def fromList[A](l: List[A]): Zipper[A] = Zipper[A](Nil, l.head, l.tail)
  }

  val gameRegex = """(\d+) players; last marble is worth (\d+) points""".r

  def parseGame(input: String, scale: Int): Game =
    input match {
      case gameRegex(p, m) => Game(Zipper.fromList(List.fill(p.toInt)(0)), 1, m.toLong*scale, Zipper(0))
    }

  def loadData(file: String, scale: Int = 1): List[Game] =
    io.Source.fromFile(file).getLines().toList.map(l => parseGame(l, scale))
}
