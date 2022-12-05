package aoc.y2021.week1

import cats.effect.IO
import common.v1.AdventApp

// https://adventofcode.com/2021/day/6

object Day6 extends AdventApp[List[Int]](year = 2021, day = 6) {
  def part1(input: List[Input]): IO[Long] = IO {
    fishDays(input)(80).values.sum
  }

  def part2(input: List[Input]): IO[Long] = IO {
    fishDays(input)(256).values.sum
  }

  def fishDays(firstDay: List[Input]): LazyList[Map[Int, Long]] = fishDays(
    firstDay.flatten.groupMapReduce(identity)(_ => 1L)(_ + _)
  )
  def fishDays(firstDay: Map[Int, Long]): LazyList[Map[Int, Long]] =
    firstDay #:: fishLoop(firstDay)
  def fishLoop(previousDay: Map[Int, Long]): LazyList[Map[Int, Long]] = {
    val birthAmount = previousDay.getOrElse(0, 0L)
    val nextDay = (previousDay - 0)
      .map { case (days, amount) => (days - 1, amount) }
      .foldLeft(Map(6 -> birthAmount, 8 -> birthAmount)) {
        case (acc, (day, amount)) =>
          acc.updated(day, acc.getOrElse(day, 0L) + amount)
      }

    nextDay #:: fishLoop(nextDay)
  }
}
