package aoc.y2021.week2

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import common.v1.AdventApp

// To bootstrap new days

object Day12 extends AdventApp[List[String]](year = 2021, day = 12) {
  def part1(input: List[Input]): IO[Any] = IO {
    CaveMap(input).allPaths1.length
  }
  def part2(input: List[Input]): IO[Any] = IO {
    CaveMap(input).allPaths2.length
  }
}

case class CaveMap(map: Map[String, List[String]]) {
  val allPaths1: List[List[String]] =
    loop(canTravelTwice = false)("start").sortBy(_.mkString)
  val allPaths2: List[List[String]] =
    loop(canTravelTwice = true)("start").sortBy(_.mkString)

  def loop(canTravelTwice: Boolean, pathTillNow: List[String] = Nil)(
      currentCave: String
  ): List[List[String]] = {
    val isSmallCave = currentCave.forall(_.isLower)

    if (currentCave == "end") List((currentCave :: pathTillNow).reverse)
    else if (currentCave == "start" && pathTillNow.nonEmpty) Nil
    else if (isSmallCave && pathTillNow.contains(currentCave))
      if (canTravelTwice)
        map(currentCave).flatMap(
          loop(canTravelTwice = false, currentCave :: pathTillNow)
        )
      else Nil
    else
      map(currentCave).flatMap(loop(canTravelTwice, currentCave :: pathTillNow))
  }
}

object CaveMap {
  def apply(input: List[List[String]]): CaveMap = CaveMap(
    input
      .flatMap { case left :: right :: Nil =>
        List(
          left -> right,
          right -> left
        )
      }
      .groupMapReduce(_._1)(_._2.pure[List])(_ ++ _)
  )
}
