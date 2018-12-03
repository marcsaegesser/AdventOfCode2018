package advent

object Day02 {

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
  def commonPrefix(pair: (String, String)): (String, (String, String)) = {
    def helper(p: List[Char], s1: List[Char], s2: List[Char]): (String, (String, String)) =
      (s1, s2) match {
        case (h1 :: t1, h2 :: t2) if h1 == h2 => helper(h1 :: p, t1, t2)
        case _                               => (p.reverse.mkString, (s1.mkString, s2.mkString))
      }

    helper(List(), pair._1.toList, pair._2.toList)
  }

  /** If two strings in the input list differ by only a single character
    * then they will sort to be adjacent to each other. After sorting
    * the input list we only need to compare adjacent elements for a
    * match. This can be done easily using List.sliding(2, 1) to
    * create a list of pairs of adjacent elements. After that we just
    * traverse the list of pairs looking for any that match our
    * constraints.
    */
  def part2(input: List[String]): Iterator[String] =
    input.sorted.sliding(2, 1).collect { case a :: b :: Nil => (a, b) }                   // Sort the input and create pairs of adjacent items
      .map(commonPrefix)                                                                  // Extract the common prefix from each pair and both remaining tails
      .collect { case (p, (t1, t2)) if t1.size > 0 && t2.size > 0  && t1.tail == t2.tail =>  // Collect the pairs with identical, non-empty tails
        p + t1.tail
      }

  def readFile(file: String): List[String] = {
    io.Source.fromFile(file)
      .getLines()
      .toList
  }
}
