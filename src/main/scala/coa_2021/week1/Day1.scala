package coa_2021.week1

import cats.effect.IO
import common.AdventApp

object Day1 extends AdventApp[Int](year = 2021, day = 1) {
  def part1(input: List[Int]): IO[Int] = IO {
    input
      .sliding(2).count(isIncreasing) // count how many times is increasing
  }

  def part2(input: List[Int]): IO[Int] = IO {
    input
      .sliding(3).map(_.sum).toList // sum of each 3 numbers
      .sliding(2).count(isIncreasing) // count how many times is increasing
  }

  private def isIncreasing(list: List[Int]) = list match {
    case List(a, b) => b > a
  }
}
