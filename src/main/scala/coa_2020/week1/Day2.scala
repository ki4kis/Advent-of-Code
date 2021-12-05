package coa_2020.week1

import cats.effect.IO
import common.AdventApp
import common.Reads._

// To bootstrap new days

object Day2 extends AdventApp[Row](year = 2020, day = 2) {
  def part1(input: List[Input]): IO[String] = IO {
    input.count(_.isValidOld).toString
  }
  def part2(input: List[Input]): IO[String] = IO {
    input.count(_.isValidNew).toString
  }
}

case class Limits(min: Int, max: Int) {
  def isBetween(nr: Int): Boolean = min <= nr && nr <= max
  def toList: List[Int] = List(min, max).map(_ - 1)
}

case class Row(limits: Limits, char: Char, password: String) {
  def isValidOld: Boolean = limits isBetween password.count(_ == char)
  def isValidNew: Boolean = limits.toList.map(password).count(_ == char) == 1
}