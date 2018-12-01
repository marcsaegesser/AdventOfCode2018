package advent

object Day01 {

  def part1(data: List[Int]): Int = data.sum

  def part2(data: List[Int]): Int = {
    def helper(f: Int, fs: Set[Int], input: Stream[Int]): Int = {
      val nextFreq = f + input.head
      if(fs.contains(nextFreq)) nextFreq
      else                      helper(nextFreq, fs + nextFreq , input.tail)
    }

    helper(0, Set(), Stream.continually(data.toStream).flatten)
  }

  def readFile(file: String): List[Int] = {
    io.Source.fromFile(file)
      .getLines()
      .map(_.toInt)
      .toList
  }
}
