package advent

object Day12 {
  type Pot = Char
  val Empty: Pot = '.'
  val Full: Pot = '#'
  val RuleLength = 5

  case class Plants(start: Int, pots: Vector[Pot], rules: Map[String, Pot]) {
    def show: String = pots.mkString
    def potsByNumber: List[(Int, Pot)] = Stream.from(start).zip(pots).toList
  }

  def day12(): Unit = {
    val plants = loadData("data/Day12.txt")
    println(s"Day12.part1 = ${part1(plants)}")
    println(s"Day12.part2 = ${part2(plants)}")
  }

  def part1(plants: Plants): Int = {
    val lastGen = step(20, plants)
    lastGen.potsByNumber.collect { case ((n, Full)) => n }.sum
  }

  def part2(plants: Plants): (Long, Long) = {
    val numGens = 50000000000L
    val gen10 = step(10000, plants)
    val gen20 = step(10000, gen10)
    val gen10Answer: Long = computeAnswer(gen10).toLong
    val gen20Answer: Long = computeAnswer(gen20).toLong
    val delta = gen20Answer - gen10Answer

    (delta, gen10Answer + (delta * (numGens - 10000) / 10000))
  }

  def computeAnswer(plants: Plants): Int =
    plants.potsByNumber.collect { case ((n, Full)) => n }.sum

  def step(n: Int, plants: Plants): Plants = {
    def helper(g: Int, ps: Plants): Plants = {
      if(g > n) ps
      else {
        val next = nextState(ps)
        helper(g+1, next)
      }
    }

    helper(1, plants)
  }

  def nextState(plants: Plants): Plants = {
    val nrml = normalizePots(plants)
    val nextPots =
      nrml.pots.sliding(RuleLength, 1).foldLeft(Vector.empty[Char]) { case (a, ps) =>
        a :+ plants.rules.getOrElse(ps.mkString, Empty)
      }

    nrml.copy(start=nrml.start+2, pots=nextPots)
  }

  def normalizePots(plants: Plants): Plants = {
    val initialEmpty = plants.pots.prefixLength(_ == Empty)
    val tailEmpty = (plants.pots.takeRight(RuleLength).reverse).prefixLength(_ == Empty)
    val toAddHead = RuleLength - initialEmpty
    val toAddTail = RuleLength - tailEmpty
    val newPots = Vector.fill(toAddHead)(Empty) ++ plants.pots ++ Vector.fill(toAddTail)(Empty)
    plants.copy(start=plants.start-toAddHead, pots=newPots)
  }


  val initialStateRegex = """initial state: (.*)""".r
  val ruleRegex = """([\.#]+) => ([\.#])""".r

  def parseInitialState(input: String): Vector[Pot] = {
    input match {
      case initialStateRegex(ps) => ps.toVector
    }
  }

  def parseRules(input: List[String]): Map[String, Pot] =
    input.map{ case ruleRegex(l, r) => (l, r.head)}.toMap

  def loadData(file: String): Plants = {
    val input = io.Source.fromFile(file).getLines().filterNot(_.isEmpty).toList
    Plants(0, parseInitialState(input.head), parseRules(input.tail))
  }
}
