package aoc.y2022.week1

import cats.implicits._
import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads
import common.Vec

// https://adventofcode.com/2022/day/9

object Day9 extends AdventApp[List[Vec]](year = 2022, day = 9) {
  def initRope(lenght: Int) = List(Vec(0, 0)).replicateA(lenght).flatten

  def reads(raw: String): Input    = raw.split("\n").toList.map(Motion.readLine).flatMap(_.toMultipleSteps)
  def part1(input: Input): IO[Any] = IO(simulateRope(input, initRope(2)).map(_.last).distinct.length)
  def part2(input: Input): IO[Any] = IO(simulateRope(input, initRope(10)).map(_.last).distinct.length)

  def simulateRope(steps: List[Vec], rope: List[Vec]): List[List[Vec]] = steps.scanLeft(rope) { case (rope, step) =>
    val nextHeadPos = rope.head + step
    rope.tail.scanLeft(nextHeadPos) { case (headPos, tailPos) =>
      val direction = headPos - tailPos
      if (direction.vec.forall(_.abs <= 1)) tailPos
      else tailPos + Vec(direction.vec.map(_ min 1 max -1))
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
