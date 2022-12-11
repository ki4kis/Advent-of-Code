package aoc.y2022

import common.v2.AdventApp
import aoc.y2022.week1._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.Assertions._

import cats.effect.unsafe.implicits.global

class AdventWeek1Spec extends AnyFreeSpec {
  val apps: List[AdventApp] = List(Day1, Day2, Day3, Day4, Day5, Day6, Day7)
  val test: List[(Any, Any)] = List(
    (24000, 45000),
    (15, 12),
    (157, 70),
    (2, 4),
    ("CMZ", "MCD"),
    (7, 19),
    (95437L, 24933642L),
  )
  val real: List[(Any, Any)] = List(
    (71780, 212489),
    (8890, 10238),
    (8039, 2510),
    (485, 857),
    ("FWNSHLDNZ", "RNRGDNFQG"),
    (1802, 3551),
    (1243729L, 4443914L),
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
