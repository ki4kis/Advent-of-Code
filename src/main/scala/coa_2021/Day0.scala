package coa_2021

import cats.effect.IO

// To bootstrap new days

trait Input

object Day0 extends AdventApp[Input](day = 0) {
  def part1(input: List[Input]): IO[String] = IO("Not yet implemented!")
  def part2(input: List[Input]): IO[String] = IO("Not yet implemented1")
}