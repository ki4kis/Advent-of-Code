package aoc.y2022.week1

import cats.effect.IO
import cats.implicits._
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/1

object Day1 extends AdventApp(year = 2022, day = 1) {
  type Input = Vector[Seq[Int]]
  def reads(raw: String): Input = for {
    chunk <- raw.split("\n\n").toVector
  } yield chunk.split("\n").map(_.toInt)

  def part1(input: Input): IO[Any] = IO(input.map(_.sum).max)
  def part2(input: Input): IO[Any] = IO(input.map(_.sum).sorted.takeRight(3).sum)
}
