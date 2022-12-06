package aoc.y2022.week1

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/6

object Day6 extends AdventApp[String](year = 2022, day = 6) {
  def reads(raw: String): Input = raw
  def findDistinctStreamEnd(data: String, length: Int): Int =
    data.sliding(length).indexWhere(_.toSet.size == length) + length

  def part1(input: Input): IO[Any] = IO(findDistinctStreamEnd(input, 4))
  def part2(input: Input): IO[Any] = IO(findDistinctStreamEnd(input, 14))
}
