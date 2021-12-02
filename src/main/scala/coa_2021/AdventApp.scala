package coa_2021

import cats.effect._
import cats.implicits._

import scala.io.{BufferedSource, Source}

abstract class AdventApp[T: Reads](day: Int) extends IOApp.Simple {
  def part1(input: List[T]): IO[String]
  def part2(input: List[T]): IO[String]

  val input: IO[BufferedSource] = IO(Source.fromResource(s"day$day.input"))
  val lines: Resource[IO, List[String]] = for {
    input <- Resource.make(input)(source => IO(source.close()))
  } yield input.getLines().toList

  private def printResult(part: Int)(res: String) = IO.println(s"Part$part: $res")

  def run: IO[Unit] = lines.use { lines =>
    val input = lines map Reads.readLine[T]
    (part1(input) >>= printResult(1)) *>
      (part2(input) >>= printResult(2))
  }
}
