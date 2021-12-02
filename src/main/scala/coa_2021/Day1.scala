package coa_2021

import cats.effect.IO

object Day1Part1 extends AdventApp[Int](1) {
  def task(input: List[Int]): IO[Unit] = IO {
    val increasing = input.sliding(2).count {
      case List(a, b) => b > a
    }

    println(s"Increasing: $increasing")
  }
}

object Day1Part2 extends AdventApp[Int](1) {
  def task(input: List[Int]): IO[Unit] = IO {
    val increasing = input
      .sliding(3).map(_.sum).toList
      .sliding(2).count {
        case List(a, b) => b > a
      }

    println(s"Increasing: $increasing")
  }
}