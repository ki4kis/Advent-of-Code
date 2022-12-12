package aoc.y2022.week2

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/10

object Day10 extends AdventApp(year = 2022, day = 10) {
  import Day10Helper._
  type Input = Vector[Instruction]
  val cyclesPart1 = List(20, 60, 100, 140, 180, 220)

  def reads(raw: String): Input = raw.split("\n").toVector.map(Instruction.readLine)
  def part1(input: Input): IO[Any] = IO {
    val cycleValues = calculateCycles(input.flatMap(_.cycles))
    cyclesPart1.map(idx => cycleValues(idx - 1) * idx).sum
  }
  def part2(input: Input): IO[Any] = IO {
    val cycleValues = calculateCycles(input.flatMap(_.cycles))
    "\n" + draw(cycleValues)
  }

  def calculateCycles(cycles: Vector[Int]) = cycles.scanLeft(1)(_ + _)
  def draw(values: Vector[Int]): String = {
    values.zipWithIndex
      .map { case (value, idx) =>
        val x = idx % 40
        if (x - 1 <= value && value <= x + 1) "##"
        else ".."
      }
      .dropRight(1)
      .mkString
      .grouped(80)
      .mkString("\n")
  }
}

object Day10Helper {
  case class Register(value: Int)

  sealed abstract class Instruction(val cycles: List[Int])

  object Instruction {
    val AddxR = "addx (-?\\d+)".r
    val NoopR = "noop"

    def readLine(raw: String): Instruction = raw match {
      case NoopR        => Noop
      case AddxR(value) => AddX(value.toInt)
    }

    case object Noop            extends Instruction(List(0))
    case class AddX(value: Int) extends Instruction(List(0, value))
  }
}
