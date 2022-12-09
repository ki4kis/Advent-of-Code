package aoc.y2022

import common.v2.AdventApp
import aoc.y2022.week1._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.Assertions._

import cats.effect.unsafe.implicits.global

class AdventWeek2Spec extends AnyFreeSpec {
  val apps: List[AdventApp[_]] =
    List(Day8, Day9) // , Day10, Day11, Day12, Day13, Day14)
  val test: List[(Any, Any)] = List(
    (21, 8),
    (13, 1),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
  )
  val real: List[(Any, Any)] = List(
    (1829, 291840),
    (6563, 2653),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
    ("Not yet implemented!", "Not yet implemented!"),
  )

  "Advent of Code" - {
    apps.zipWithIndex foreach { case (app, idx) =>
      s"Year ${app.year} Day #${app.day}" - {
        test.lift(idx) foreach { case (exp1, exp2) =>
          "test" - {
            val (act1, act2) = app.test.unsafeRunSync()
            "part1" in assert(act1 == exp1)
            "part2" in assert(act2 == exp2)
          }
        }
        real.lift(idx) foreach { case (exp1, exp2) =>
          "real" - {
            val (act1, act2) = app.real.unsafeRunSync()
            "part1" in assert(act1 == exp1)
            "part2" in assert(act2 == exp2)
          }
        }
      }
    }
  }
}
