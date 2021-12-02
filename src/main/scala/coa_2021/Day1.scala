package coa_2021

import cats.effect.IO

// https://adventofcode.com/2021/day/1

object Day1 extends AdventApp[Int](day = 1) {
  def part1(input: List[Int]): IO[String] = IO {
    input
      .sliding(2).count(isIncreasing) // count how many times is increasing
      .toString
  }

  def part2(input: List[Int]): IO[String] = IO {
    input
      .sliding(3).map(_.sum).toList // sum of each 3 numbers
      .sliding(2).count(isIncreasing) // count how many times is increasing
      .toString
  }

  private def isIncreasing(list: List[Int]) = list match {
    case List(a, b) => b > a
  }
}