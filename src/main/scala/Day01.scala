package advent

import scala.io.Source
import scalaz._
import scalaz.effect._

object Day01 {
  case class SearchState(freq: Int, prev: Set[Int], input: Stream[Int])
  def initialState(data: List[Int]): SearchState = SearchState(0, Set(), Stream.continually(data.toStream).flatten)

  def monadState[S,F[_] : Monad] = MonadState[StateT[S, F, ?], S]  // TODO: Figure out how to avoid needing this
  val searchState = monadState[SearchState, Free.Trampoline]
  import searchState._


  def part1(data: List[Int]): Int = data.sum

  def part2(data: List[Int]): Int = {
    val nextState = state { case   SearchState(f, fs, i) => (SearchState(f+i.head, fs + f, i.tail), ()) }
    val done      = state { case s@SearchState(f, fs, _) => (s, fs.contains(f)) }

    untilM_(nextState, done).run(initialState(data)).run._1.freq
  }

  def day01(): Unit = {
    (for {
      data <- loadData("data/Day01.txt")
      p1   =  part1(data)
      _    <- IO.putStrLn(s"Day01:p1 - $p1")
      p2   =  part2(data)
      _    <- IO.putStrLn(s"Day01:p2 - $p2")
    } yield ()).unsafePerformIO
  }

  def loadData(file: String): IO[List[Int]] =
    openSource(file).bracket(closeSource(_)) { s =>
      IO(
        s.getLines()
          .map(_.toInt)
          .toList)
    }

  def openSource(file: String): IO[Source] = IO(Source.fromFile(file))
  def closeSource(s: Source): IO[Unit]     = IO(s.close())
}
