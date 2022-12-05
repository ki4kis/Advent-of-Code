package aoc.y2020.week1

import cats.effect.IO
import common.v1.AdventApp

// To bootstrap new days

object Day6 extends AdventApp[String](year = 2020, day = 6) {
  def part1(input: List[Input]): IO[String] = IO {
    fold(input)(_ | _).map(_.size).sum.toString
  }

  def part2(input: List[Input]): IO[String] = IO {
    fold(input)(_ & _).map(_.size).sum.toString
  }

  def fold(
      input: List[Input]
  )(combine: (Set[Char], Set[Char]) => Set[Char]): List[Set[Char]] = {
    val Empty = Set('A')
    input.foldLeft[List[Set[Char]]](List(Empty)) {
      case (acc, "")            => acc :+ Empty
      case (acc :+ Empty, line) => acc :+ line.toSet
      case (acc :+ last, line)  => acc :+ combine(last, line.toSet)
    }
  }
}
