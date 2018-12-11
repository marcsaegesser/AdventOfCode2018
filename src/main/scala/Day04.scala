package advent

object Day04 {
  type GuardId = Int
  type Day = String
  type Hour = Int
  type Minute = Int
  type Time = (Hour, Minute)  // (hour, minute)

  def day04(): Unit = {
    val input = mkEvents(loadData("data/Day04.txt"))
    println(s"Day04.part1 = ${part1(input)}")
    println(s"Day04.part2 = ${part2(input)}")
  }

  def part1(events: Map[GuardId, List[Event]]): Int = {
    val s = maxSleeper(events)
    val (m, _) = maxMinute(events(s))

    s * m
  }

  def part2(events: Map[GuardId, List[Event]]): Int = {
    val (g, (m, _)) =
      events
        .mapValues(maxMinute)
        .toList
        .sortBy(_._2._2).reverse.head

    g * m
  }

  sealed trait Event { val guard: GuardId; val day: Day }
  case class Asleep(guard: GuardId, day: Day, time: Minute) extends Event
  case class Awake(guard: GuardId, day: Day, time: Minute) extends Event

  /** For all guards, which was asleep the most.
    */
  def maxSleeper(data: Map[GuardId, List[Event]]): GuardId =
    data.mapValues (_.sliding(2, 2).collect { case Asleep(g1, d1, m1) :: Awake(g2, d2, m2) :: Nil => m2 - m1 }.sum)
      .toList.sortBy(_._2).reverse.head._1   // OK, this makes me feel dirty

  /** For a given guard, what minute were they asleep the most and how many days asleep at that minute.
    */
  def maxMinute(data: List[Event]): (Minute, Int) = {
    data.sliding(2, 2)
      .collect { case Asleep(_, _, m1) :: Awake(_, _, m2) :: Nil => (m1 until m2).toList }
      .flatten
      .toList
      .groupBy(identity)
      .mapValues(_.size)
      .toList.sortBy(_._2).reverse.head
  }

  val recordRegex = """\[([\d-]+) (\d+):(\d+)\] (.*)""".r

  val shiftRegex  = """Guard #(\d+) begins shift""".r
  val asleepRegex = """falls asleep""".r
  val awakeRegex  = """wakes up""".r

  def mkEvents(data: List[(Day, Time, String)]): Map[GuardId, List[Event]] =
    data.foldLeft((0, List.empty[Event])) { case ((g, a), (d, t, e)) =>
      e match {
        case shiftRegex(id)   => (id.toInt, a)
        case asleepRegex()    => (g, Asleep(g, d, t._2) :: a)
        case awakeRegex()     => (g, Awake(g, d, t._2) :: a)
        case _                => throw new Exception(s"Invalid input $e")
      }
    }._2.reverse.groupBy(_.guard)

  def parseLine(s: String): (Day, Time, String) =
    s match {
      case recordRegex(d, h, m, r) => (d, (h.toInt, m.toInt), r)
    }

  def loadData(file: String): List[(Day, Time, String)] =
    io.Source.fromFile(file).getLines()
      .map(parseLine)
      .toList
      .sortBy(x => (x._1, x._2))


}

