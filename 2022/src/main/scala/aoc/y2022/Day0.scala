package aoc.y2022

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads

// To bootstrap new days

object Day0 extends AdventApp(year = 2022, day = 0) {
  type Input = String
  def reads(raw: String): Input    = raw
  def part1(input: Input): IO[Any] = IO("Not yet implemented!")
  def part2(input: Input): IO[Any] = IO("Not yet implemented!")
}
