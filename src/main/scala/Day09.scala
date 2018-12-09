package advent

object Day09 {
  case class Game(players: Zipper[Int], marbles: List[Int], board: Zipper[Int]) {
    def hasNext: Boolean = !this.marbles.isEmpty

    def nextState: Game =
      this match {
        case Game(p, Nil, b) => this
        case Game(p, h :: t, b) =>
          if(h % 23 == 0) {
            val (v, bb) = b.moveLeft(7).take
            val pp = p.update(players.focus + h + v).moveRight
            Game(pp, t, bb)
          } else {
            Game(p.moveRight, t, b.moveRight.addRight(h))
          }
      }
  }

  def playGame(g: Game): Game =
    if(g.hasNext) playGame(g.nextState)
    else          g


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

  def parseGame(input: String): Game =
    input match {
      case gameRegex(p, m) => Game(Zipper.fromList(List.fill(p.toInt)(0)), (1 to m.toInt).toList, Zipper(0))
    }

  def loadData(file: String): List[Game] =
    io.Source.fromFile(file).getLines().toList.map(parseGame)
}
