package aoc.y2022.week1

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/2

object Day2 extends AdventApp[Vector[(RPS, String)]](year = 2022, day = 2) {
  import RPS._
  val Hands = "(\\w) (\\w)".r

  def reads(raw: String): Input = raw.split("\n").toVector.map {
    case Hands("A", self) => (Rock, self)
    case Hands("B", self) => (Paper, self)
    case Hands("C", self) => (Scissors, self)
  }

  val part1Map: Map[String, RPS] = RPS.parseMap("X", "Y", "Z")
  val part2Map: Map[RPS, Map[String, RPS]] = Map(
    Rock     -> RPS.parseMap("Y", "Z", "X"),
    Paper    -> RPS.parseMap("X", "Y", "Z"),
    Scissors -> RPS.parseMap("Z", "X", "Y"),
  )

  def part1(input: Input): IO[Any] = IO(input.map { case (that, self) =>
    that vs part1Map(self)
  }.sum)

  def part2(input: Input): IO[Any] = IO(input.map { case (that, self) =>
    that vs part2Map(that)(self)
  }.sum)
}

abstract class RPS(val score: Int) extends Ordered[RPS] { self =>
  import RPS._

  def compare(that: RPS): Int = (self, that) match {
    case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 0
    case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => -1
    case (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors) => 1
  }

  def vs(that: RPS): Int = {
    val outcome =
      if (self < that) 6
      else if (self == that) 3
      else 0

    outcome + that.score
  }

}

object RPS {
  case object Rock     extends RPS(1)
  case object Paper    extends RPS(2)
  case object Scissors extends RPS(3)

  def parseMap(by: String*): Map[String, RPS] = by match {
    case Seq(rock, paper, scissors) =>
      Map(
        rock     -> Rock,
        paper    -> Paper,
        scissors -> Scissors,
      )
  }
}
