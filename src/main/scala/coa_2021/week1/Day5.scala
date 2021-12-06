package coa_2021.week1

import cats.effect.IO
import common.AdventApp

// https://adventofcode.com/2021/day/5

object Day5 extends AdventApp[Vent](year = 2021, day = 5) {
  def part1(input: List[Input]): IO[Int] = IO {
    input.filter(_.isHorOrVert)
      .flatMap(_.ventPos)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 > 1)
  }

  def part2(input: List[Input]): IO[Int] = IO {
    input
      .flatMap(_.ventPos)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 > 1)
  }
}

case class Vent(from: Pos, to: Pos) {
  private val vector: Pos = Pos(to.x - from.x, to.y - from.y)
  val isHorOrVert: Boolean = vector.x == 0 || vector.y == 0

  private val xInc = if (vector.x < 0) -1 else 1
  private val yInc = if (vector.y < 0) -1 else 1
  private val xList = (0 to vector.x by xInc).toList
  private val yList = (0 to vector.y by yInc).toList
  private val length = xList.length max yList.length
  private val vectorPos = (xList.padTo(length, xList.head) zip yList.padTo(length, yList.head)) map {
    case (x, y) => Pos(x, y)
  }

  def ventPos: List[Pos] = vectorPos.map {
    case Pos(x, y) => Pos(from.x + x, from.y + y)
  }

}