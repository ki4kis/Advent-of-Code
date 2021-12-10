package coa_2021.week2

import cats.effect.IO
import common.AdventApp

// https://adventofcode.com/2021/day/8

object Day8 extends AdventApp[List[String]](year = 2021, day = 8) {
  def part1(input: List[Input]): IO[Any] = IO {
    val output = input.flatMap(_.takeRight(4))
    val searching = Set(Digit.One, Digit.Four, Digit.Seven, Digit.Eight).map(_.segments.size)
    output count { digit => searching(digit.length) }
  }
  def part2(input: List[Input]): IO[Any] = IO {
    val digitEntries = input.map(_.take(10))
    val outputEntires = input.map(_.takeRight(4))

    val patterns = digitEntries.map(Digit.findPattern)
    val outputs = patterns zip outputEntires map { case (pattern, output) =>
      val outputDigits = pattern.toDigits(output)
      if (outputDigits.length != 4) println(s"error parsing $output")
      outputDigits.map(_.int).mkString.toInt
    }

    outputs.sum
  }
}

case class Pattern(map: Map[Char, Char]) {
  def translate(segments: String): String = segments.map(map)
  def toDigit(segments: String): Option[Digit] = Digit(translate(segments))
  def toDigits(segments: List[String]): List[Digit] = segments.flatMap(toDigit)
}

sealed abstract class Digit(val int: Int, val segmentStr: String) {
  val segments: Set[Char] = segmentStr.toSet
}

object Digit {
  val allSegments = "abcdefg"
  private val allPatterns: List[Pattern] = allSegments.permutations.toList map { result =>
    Pattern((allSegments zip result).toMap)
  }

  def findPattern(segments: List[String]): Pattern = {
    allPatterns.find(pattern => pattern.toDigits(segments).length == 10).get
  }

  case object Zero  extends Digit(0, "abcefg")
  case object One   extends Digit(1, "cf")
  case object Two   extends Digit(2, "acdeg")
  case object Three extends Digit(3, "acdfg")
  case object Four  extends Digit(4, "bcdf")
  case object Five  extends Digit(5, "abdfg")
  case object Six   extends Digit(6, "abdefg")
  case object Seven extends Digit(7, "acf")
  case object Eight extends Digit(8, "abcdefg")
  case object Nine  extends Digit(9, "abcdfg")

  val Values: Set[Digit] = Set(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine)
  val Map: Map[Set[Char], Digit] = Values.map(digit => digit.segments -> digit).toMap

  def apply(segments: Set[Char]): Option[Digit] = Map.get(segments)
  def apply(segments: String): Option[Digit] = Digit(segments.toSet)
  def fromList(digits: List[String]): List[Digit] = digits.flatMap(Digit.apply)
}