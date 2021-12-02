package coa_2021

import cats.effect.IO

import Day2._

object Day2Part1 extends AdventApp[Command](2) {
  def task(input: List[Command]): IO[Unit] = IO {
    val (hor, depth) = input.foldLeft((0, 0)) {
      case ((hor, depth), Command(Direction.Forward, units)) => (hor + units, depth)
      case ((hor, depth), Command(Direction.Down, units)) => (hor, depth + units)
      case ((hor, depth), Command(Direction.Up, units)) => (hor, depth - units)
    }

    println(s"($hor, $depth) = ${hor * depth}")
  }
}

object Day2Part2 extends AdventApp[Command](2) {
  def task(input: List[Command]): IO[Unit] = IO {
    val (hor, depth, _) = input.foldLeft((0, 0, 0)) {
      case ((hor, depth, aim), Command(Direction.Forward, units)) => (hor + units, depth + (aim * units), aim)
      case ((hor, depth, aim), Command(Direction.Down, units)) => (hor, depth, aim + units)
      case ((hor, depth, aim), Command(Direction.Up, units)) => (hor, depth, aim - units)
    }

    println(s"($hor, $depth) = ${hor * depth}")
  }
}

object Day2 {
  sealed trait Direction
  object Direction {
    case object Forward extends Direction
    case object Down extends Direction
    case object Up extends Direction
  }

  case class Command(direction: Direction, units: Int)
}