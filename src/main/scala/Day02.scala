package advent

object Day02 {

  def day02(): Unit = {
    val input = readFile("data/Day02.txt")
    println(s"Day02.part1 = ${part1(input)}")
    println(s"""Day02.part2 = ${part2(input).getOrElse("Error!")}""")
  }

  /** For each string collect all the occurences of each letter (i.e. Map[Char, List[Char]]).
    * The size of each list is the number of occurences of that character. We don't care about
    * specific characters just the counts so extract just the values from Map and convert the
    * result to Set to remove duplicates.
    */
  def part1(input: List[String]): Int = {
    def countOccurences(s: String): Set[Int] = s.groupBy(identity).mapValues(_.size).values.toSet
    def filterByCount(s: Set[Int], n: Int): Boolean = s.contains(n)

    val counted = input.map(countOccurences)
    val twoCount = counted.filter(s => filterByCount(s, 2)).size
    val threeCount = counted.filter(s => filterByCount(s, 3)).size

    twoCount * threeCount
  }

  /** Given a pair of Strings find their longest common prefix. Return that prefix
    * and the remaining tail of each string.
    */
  def commonPrefix(a: String, b: String): (String, (String, String)) = {
    def helper(p: List[Char], s1: List[Char], s2: List[Char]): (String, (String, String)) =
      (s1, s2) match {
        case (h1 :: t1, h2 :: t2) if h1 == h2 => helper(h1 :: p, t1, t2)
        case _                               => (p.reverse.mkString, (s1.mkString, s2.mkString))
      }

    helper(List.empty[Char], a.toList, b.toList)
  }

  sealed trait SearchState
  case class   Searching(l1: List[String], l2: List[String]) extends SearchState
  case class   Complete(ms: String)                          extends SearchState
  case object  Failed                                        extends SearchState

  def nextState(s: SearchState): SearchState =
    s match {
      case Searching(l1@(h1::t1), l2@(h2::t2)) =>
        commonPrefix(h1, h2) match {
          case ("", _)                                => Searching(t1, t1)        // No common prefix, advance l1, reset l2
          case (_, ("", _))                           => Searching(l1, t2)        // Prefix is the whole string, advance l2
          case (_, (_, ""))                           => Searching(l1, t2)
          case (p, (tt1, tt2)) if tt1.tail == tt2.tail => Complete(p ++ tt1.tail)  // Got one!
          case (p, _)                                 => Searching(l1, t2)        // Suffix didn't match, advance l2
        }
      case Searching(Nil, _) => Failed
      case Searching(_, Nil) => Failed
      case Complete(_)       => s
      case Failed            => s
    }

  def part2(input: List[String]): Option[String] = {
    def helper(s: SearchState): Option[String] = {
      nextState(s) match {
        case ns: Searching => helper(ns)
        case Complete(ms)  => Some(ms)
        case Failed        => None
      }
    }

    val sorted = input.sorted
    helper(Searching(sorted, sorted))
  }

  def readFile(file: String): List[String] = {
    io.Source.fromFile(file)
      .getLines()
      .toList
  }
}
