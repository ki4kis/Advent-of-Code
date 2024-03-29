package aoc.y2022.week2

import cats.implicits._
import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads
import common.Vec

// https://adventofcode.com/2022/day/9

object Day9 extends AdventApp(year = 2022, day = 9) {
  type Input = List[Vec]
  def initRope(lenght: Int) = List(Vec(0, 0)).replicateA(lenght).flatten

  def reads(raw: String): Input    = raw.split("\n").toList.map(Motion.readLine).flatMap(_.toMultipleSteps)
  def part1(input: Input): IO[Any] = IO(simulateRope(input, initRope(2)).map(_.last).distinct.length)
  def part2(input: Input): IO[Any] = IO(simulateRope(input, initRope(10)).map(_.last).distinct.length)

  def simulateRope(steps: List[Vec], rope: List[Vec]): List[List[Vec]] = steps.scanLeft(rope) { case (rope, step) =>
    rope.tail.scanLeft(rope.head + step) { case (headPos, tailPos) =>
      val direction = headPos - tailPos
      if (direction.vec.forall(_.abs <= 1)) tailPos
      else tailPos + direction.map(1 min _ max -1)
    }
  }
}

case class Motion(direction: Vec, distance: Int) {
  def asSingleStep: Vec          = direction * distance
  def toMultipleSteps: List[Vec] = List(direction).replicateA(distance).flatten
}

object Motion {
  val MotionR = "([LRUD]) (\\d+)".r

  def readLine(raw: String) = raw match {
    case MotionR("L", dist) => Motion(Vec(-1, 0), dist.toInt)
    case MotionR("R", dist) => Motion(Vec(1, 0), dist.toInt)
    case MotionR("U", dist) => Motion(Vec(0, 1), dist.toInt)
    case MotionR("D", dist) => Motion(Vec(0, -1), dist.toInt)
  }
}
