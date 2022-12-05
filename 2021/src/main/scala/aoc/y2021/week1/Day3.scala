package aoc.y2021.week1

import cats.effect.IO
import common.v1.AdventApp

// https://adventofcode.com/2021/day/3

object Day3 extends AdventApp[String](year = 2021, day = 3) {
  def part1(input: List[Input]): IO[Int] = IO {
    val (gammaBits, epsilonBits) = rateSplitter(input)

    val gammaRate = parseBinary(gammaBits)
    val epsilonRate = parseBinary(epsilonBits)

    gammaRate * epsilonRate
  }

  def part2(input: List[Input]): IO[Int] = IO {
    val oxyRating = parseBinary(ratingSplitter(input)(_._1))
    val co2Rating = parseBinary(ratingSplitter(input)(_._2))

    oxyRating * co2Rating
  }

  def rateSplitter(input: List[Input]): (List[Char], List[Char]) = input
    .map(_.toList)
    .transpose
    .map { bits =>
      val bit1Count = bits.count(_ == '1')
      val bit0Count = input.length - bit1Count

      if (bit1Count >= bit0Count) ('1', '0')
      else ('0', '1')
    }
    .unzip

  def ratingSplitter(input: List[Input], idx: Int = 0)(
      reduce: ((List[Char], List[Char])) => List[Char]
  ): Input =
    if (input.length == 1) input.head
    else {
      val criteriaBit = reduce(rateSplitter(input))(idx)
      ratingSplitter(input.filter(_.apply(idx) == criteriaBit), idx + 1)(reduce)
    }

  def parseBinary(bin: List[Char]): Int = parseBinary(bin.mkString(""))
  def parseBinary(bin: String): Int = Integer.parseInt(bin, 2)
}
