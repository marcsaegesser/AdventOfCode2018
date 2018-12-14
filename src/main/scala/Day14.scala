package advent

import scala.annotation.tailrec

object Day14 {
  def day14(): Unit = {
    println(s"Day14.part1 = ${part1(puzzleInput)}")
    println(s"Day14.part2 = ${part2(puzzleInput.toString)}")
  }

  def part1(n: Int): String = {
    val s = runUntilSize(n + 10, initialState)
    if(s.scores.size == n+10) s.suffix.takeRight(10).mkString
    else              s.suffix.init.takeRight(10).mkString
  }

  def part2(pattern: String): Int = {
    val pat = pattern.grouped(1).toVector.map(_.toByte)
    runUntilPattern(pat, initialState)._1
  }


  case class State(elf1: Int, elf2: Int, scores: Map[Int, Byte], suffix: Vector[Byte])
  val initialState = State(0, 1, Map((0 -> 3), (1 -> 7)), Vector(3, 7))

  def intToVector(n: Int): Vector[Byte] = n.toString.grouped(1).toVector.map(_.toByte)

  def runN(n: Int, state: State): State = {
    def helper(curr: Int, state: State): State =
      if(curr == n) state
      else         helper(curr+1, nextState(state))

    helper(0, state)
  }

  def runUntilSize(size: Int, state: State): State = {
    if(state.scores.size >= size) state
    else                  runUntilSize(size, nextState(state))
  }

  def runUntilPattern(pattern: Vector[Byte], state: State): (Int, State) = {
    @tailrec
    def helper(s: State): (Int, State) = {
      s match { case State(_, _, scores, suffix) =>
        if(suffix.endsWith(pattern))           (scores.size - pattern.size, s)
        else if(suffix.init.endsWith(pattern)) (scores.size - pattern.size - 1, s)
        else                                   helper(nextState(s))
      }
    }

    helper(state)
  }

  def nextState(state: State): State = {
    state match { case State(e1, e2, ss, sx) =>
      val v1 = ss(e1)
      val v2 = ss(e2)
      val newScores = intToVector(v1+v2)
      val nextScores = newScores.foldLeft(ss) { case (scores, v) =>  scores + ((scores.size, v)) }
      val nextSuffix = (sx ++ newScores).takeRight(15)
      val nextE1 = (e1 + v1 + 1) % nextScores.size
      val nextE2 = (e2 + v2 + 1) % nextScores.size
      State(nextE1, nextE2, nextScores, nextSuffix)
    }
  }

  val puzzleInput = 293801
}
