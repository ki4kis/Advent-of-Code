package common.v2

import cats.effect._
import cats.implicits._

import scala.io.{BufferedSource, Source}

abstract class AdventApp[T](year: Int, day: Int) extends IOApp.Simple {
  type Input = T
  def reads(raw: String): T

  def part1(input: T): IO[Any]
  def part2(input: T): IO[Any]

  private val input: IO[BufferedSource] = IO(
    Source.fromResource(s"day$day.input")
  )
  private val raw: Resource[IO, String] = for {
    input <- Resource.make(input)(source => IO(source.close()))
  } yield input.getLines.mkString("\n")

  private def printResult(part: Int)(res: Any) = IO.println(s"Part$part: $res")

  def run: IO[Unit] = raw.use { raw =>
    val input = reads(raw)
    IO.println(s"Code of Advent ($year) Day $day result") *> (for {
      _ <- part1(input) >>= printResult(1)
      _ <- part2(input) >>= printResult(2)
    } yield ())
  }
}
