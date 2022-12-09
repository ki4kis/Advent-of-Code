package common.v2

import cats.effect._
import cats.implicits._

import scala.io.{BufferedSource, Source}
import scala.concurrent.duration._

abstract class AdventApp[T](val year: Int, val day: Int) extends IOApp.Simple {
  type Input = T
  def reads(raw: String): T

  def part1(input: T): IO[Any]
  def part2(input: T): IO[Any]

  private def input(file: String): IO[BufferedSource] = IO(
    Source.fromResource(s"$file.input")
  )
  private def raw(file: String): Resource[IO, String] = for {
    input <- Resource.make(input(file))(source => IO(source.close()))
  } yield input.getLines.mkString("\n")

  private def printResult(part: Int)(res: Any) = IO.println(s"Part$part: $res")

  def parseAndRun(raw: String): IO[(Any, Any)] =
    for {
      input <- IO(reads(raw))
      res1  <- part1(input)
      res2  <- part2(input)
    } yield (res1, res2)

  val real: IO[(Any, Any)] = raw(s"day$day").use(parseAndRun)
  val test: IO[(Any, Any)] = raw(s"day$day.test").use(parseAndRun)

  private def timed[T](task: IO[T]): IO[T] = for {
    start  <- Clock[IO].monotonic
    result <- task
    finish <- Clock[IO].monotonic
    _      <- IO.println(s"Task took: ${(finish - start).toMillis}ms")
  } yield result

  val run: IO[Unit] = IO.println(s"Code of Advent ($year) Day $day") *>
    timed(real) flatMap { case (result1, result2) =>
      printResult(1)(result1) *> printResult(2)(result2)
    }
}
