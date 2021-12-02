package coa_2021

import cats.effect._

import scala.io.{BufferedSource, Source}

abstract class AdventApp[T](day: Int)(implicit reads: Reads[T]) extends IOApp.Simple {
  def task(input: List[T]): IO[Unit]

  val input: IO[BufferedSource] = IO(Source.fromResource(s"day$day.input"))
  val lines: Resource[IO, List[String]] = for {
    input <- Resource.make(input)(source => IO(source.close()))
  } yield input.getLines().toList

  def run: IO[Unit] = lines.use(lines => task(lines map Reads.readLine[T]))
}
