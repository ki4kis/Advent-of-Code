package aoc.y2022.week1

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/4

object Day4 extends AdventApp[Vector[Assigment]](year = 2022, day = 4) {
  def reads(raw: String): Input = raw
    .split("\n")
    .toVector
    .map(Assigment.fromLine)

  def part1(input: Input): IO[Any] = IO(input.count(_.hasCompleteOverlap))
  def part2(input: Input): IO[Any] = IO(input.count(_.intersection.nonEmpty))
}

case class Assigment(first: (Int, Int), second: (Int, Int)) {
  val (a1, a2) = first
  val (b1, b2) = second

  val firstSection = (a1 to a2).toSet
  val secondSection = (b1 to b2).toSet

  val intersection = firstSection & secondSection

  def hasCompleteOverlap =
    firstSection == intersection || secondSection == intersection
}

object Assigment {
  val Line = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r

  def fromLine(line: String): Assigment = line match {
    case Line(a1, a2, b1, b2) =>
      Assigment((a1.toInt, a2.toInt), (b1.toInt, b2.toInt))
  }
}
