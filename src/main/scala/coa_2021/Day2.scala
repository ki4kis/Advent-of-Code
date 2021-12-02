package coa_2021

import cats.effect.IO

// https://adventofcode.com/2021/day/2

sealed trait Command { def units: Int }
object Command {
  case class Forward(units: Int) extends Command
  case class Down(units: Int) extends Command
  case class Up(units: Int) extends Command
}

object Day2 extends AdventApp[Command](day = 2) {
  private val initPos1 = Part1.Pos(0, 0)
  private val initPos2 = Part2.Pos(0, 0, 0)

  def part1(input: List[Command]): IO[String] = IO {
    val Part1.Pos(hor, depth) = input.foldLeft(initPos1)(_ move _)
    s"($hor, $depth) = ${hor * depth}"
  }

  def part2(input: List[Command]): IO[String] = IO {
    val Part2.Pos(hor, depth, _) = input.foldLeft(initPos2)(_ move _)
    s"($hor, $depth) = ${hor * depth}"
  }

  object Part1 {
    case class Pos(hor: Int, depth: Int) {
      def move(cmd: Command): Pos = cmd match {
        case Command.Forward(units) => copy(hor = hor + units)
        case Command.Down(units) => copy(depth = depth + units)
        case Command.Up(units) => copy(depth = depth - units)
      }
    }
  }

  object Part2 {
    case class Pos(hor: Int, depth: Int, aim: Int) {
      def move(cmd: Command): Pos = cmd match {
        case Command.Forward(units) => copy(hor + units, depth + (aim * units))
        case Command.Down(units) => copy(aim = aim + units)
        case Command.Up(units) => copy(aim = aim - units)
      }
    }
  }
}