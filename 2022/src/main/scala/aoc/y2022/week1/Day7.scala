package aoc.y2022

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/7

object Day7 extends AdventApp[String](year = 2022, day = 7) {
  def reads(raw: String): Input = raw
  def part1(input: Input): IO[Any] = IO("Not yet implemented!")
  def part2(input: Input): IO[Any] = IO("Not yet implemented!")
}
