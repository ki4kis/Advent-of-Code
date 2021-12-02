package coa_2021

import cats.effect.IO

import Day2._

object Day2Part1 extends AdventApp[Command](2) {
  private val initPos = Pos(0, 0)

  def task(input: List[Command]): IO[Unit] = IO {
    val Pos(hor, depth) = input.foldLeft(initPos)(_ move _)

    println(s"($hor, $depth) = ${hor * depth}")
  }

  case class Pos(hor: Int, depth: Int) {
    def move(cmd: Command): Pos = cmd match {
      case Command.Forward(units) => copy(hor = hor + units)
      case Command.Down(units) => copy(depth = depth + units)
      case Command.Up(units) => copy(depth = depth - units)
    }
  }
}

object Day2Part2 extends AdventApp[Command](2) {
  private val initPos = Pos(0, 0, 0)

  def task(input: List[Command]): IO[Unit] = IO {
    val Pos(hor, depth, _) = input.foldLeft(initPos)(_ move _)

    println(s"($hor, $depth) = ${hor * depth}")
  }

  case class Pos(hor: Int, depth: Int, aim: Int) {
    def move(cmd: Command): Pos = cmd match {
      case Command.Forward(units) => copy(hor + units, depth + (aim * units))
      case Command.Down(units) => copy(aim = aim + units)
      case Command.Up(units) => copy(aim = aim - units)
    }
  }
}

object Day2 {
  sealed trait Command { def units: Int }
  object Command {
    case class Forward(units: Int) extends Command
    case class Down(units: Int) extends Command
    case class Up(units: Int) extends Command
  }
}