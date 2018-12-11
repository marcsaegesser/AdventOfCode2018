package advent

object Day05 {

  def day05(): Unit = {
    val input = loadData("data/Day05.txt")
    println(s"Day05.part1 = ${part1(input)}")
    println(s"Day05.part2 = ${part2(input)}")
  }

  def part1(input: List[Char]): Int =
    runReaction(input).size

  def part2(input: List[Char]): Int = {
    ('a' to 'z')
      .map(c => runReaction(filterUnits(c, input)).size)
      .min
  }

  def filterUnits(c: Char, input: List[Char]): List[Char] =
    input.filterNot(u => u.toLower == c.toLower)

  def willReact(a: Char, b: Char): Boolean =
    a.toLower == b.toLower && a != b

  def reactor(left: List[Char], right: List[Char]): String =
    (left, right) match {
      case (l, Nil)             => l.reverse.mkString
      case (Nil, h :: t)        => reactor(h :: Nil, t)
      case (lh :: lt, rh :: rt) =>
        if(willReact(lh, rh)) reactor(lt, rt)
        else                  reactor(rh :: left, rt)
    }

  def runReaction(input: List[Char]): String =
    reactor(List.empty[Char], input)

  def loadData(file: String): List[Char] =
    io.Source.fromFile(file)
      .getLines()
      .filterNot(_.isEmpty)
      .toList
      .flatten
}
