package coa_2020.week1

import cats.effect.IO
import common.AdventApp

// To bootstrap new days

object Day1 extends AdventApp[Int](year = 2020, day = 1) {
  def part1(input: List[Input]): IO[String] = IO {
    val Some(product) = input
      .combinations(2)
      .find(_.sum == 2020)
      .map(_.product)

    product.toString
  }

  def part2(input: List[Input]): IO[String] = IO {
    val Some(product) = input
      .combinations(3)
      .find(_.sum == 2020)
      .map(_.product)

    product.toString
  }
}