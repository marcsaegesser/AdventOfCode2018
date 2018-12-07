package advent

import scala.collection.immutable._

object Day07 {
  type Step = Char
  type Constraints = Map[Step, List[Step]]

  def part1(constraints: Constraints): String = {
    def helper(accum: List[Step], cs: Constraints): String = {
      nextStep(cs) match {
        case None    => accum.mkString.reverse
        case Some(s) => helper(s :: accum, runStep(s, cs))
      }
    }

    helper(List(), constraints)
  }

  def part2(numWorkers: Int, baseTime: Int, constraints: Constraints): Int = {
    def helper(elapsed: Int, queue: List[(Int, Step)], cs: Constraints): Int = {
      val nextQueue = fillQueue(readySteps(cs), numWorkers, baseTime, queue)
      println(s"$elapsed: $nextQueue")
      nextQueue match {
        case Nil    => elapsed
        case h :: t => helper(elapsed + h._1, t, runStep(h._2, cs))
      }
    }

    helper(0, List.empty[(Int, Step)], constraints)
  }

  def fillQueue(ready: SortedSet[Step], size: Int, baseTime: Int, queue: List[(Int, Step)]): List[(Int, Step)] = {
    val active = queue.map(_._2)
    val toAdd = ready
      .filterNot(s => active.contains(s))
      .take(size - queue.size).map(s => ((stepTime(s, baseTime), s)))
    toAdd.foldLeft(queue) { case ((q, (t, s))) => addDelta(t, s, q) }
  }

  def addDelta(time: Int, step: Step, queue: List[(Int, Step)]): List[(Int, Step)] = {
    def helper(remaining: Int, left: List[(Int, Step)], right: List[(Int, Step)]): List[(Int, Step)] = {
      (left, right) match {
        case (l, Nil)     => ((remaining, step) :: l).reverse
        case (l, h :: t)  =>
          if( remaining <= h._1 ) (l.reverse :+ ((remaining, step)) :+ ((h._1-remaining, h._2))) ++ t
          else                   helper(remaining - h._1, h :: l, t)
      }
    }

    helper(time, List(), queue)
  }

  def stepTime(step: Step, base: Int): Int = base + step.toUpper - 'A' + 1

  def initialSteps(cs: Constraints): SortedSet[Step] =
    cs.filter{ case (k, v) => v.map(s => cs.getOrElse(s, List())).flatten.size == 0 }
      .values
      .flatten
      .to[SortedSet]

  def nextStep(cs: Constraints): Option[Step] =
    cs.filter { case (_, v) => v.isEmpty }.keys.toList.sorted.headOption

  def readySteps(cs: Constraints): SortedSet[Step] =
    cs.filter { case (_, v) => v.isEmpty }.keys.to[SortedSet]

  def runStep(step: Step, cs: Constraints): Constraints =
    cs.filterKeys(_ != step)
      .mapValues(_.filterNot(_ == step))

  def addInitialSteps(cs: Constraints): Constraints = {
    initialSteps(cs)
      .foldLeft(cs) { case (a, b) => a + ((b, List.empty[Step])) }
  }

  val constraintRegeg = """Step (\w) must be finished before step (\w) can begin.""".r
  def loadData(file: String): Constraints =
    addInitialSteps(
      io.Source.fromFile(file)
        .getLines().filterNot(_.isEmpty)
        .map { case constraintRegeg(r, s) => (s.head, r.head) }
        .toList
        .groupBy(_._1)
        .mapValues(_.map(_._2))
    )
}
