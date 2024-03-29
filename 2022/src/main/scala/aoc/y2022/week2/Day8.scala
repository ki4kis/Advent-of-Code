package aoc.y2022.week2

import cats.effect.IO
import cats.implicits._
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/8

object Day8 extends AdventApp(year = 2022, day = 8) {
  type Input = TreeGrid
  def reads(raw: String): Input    = TreeGrid(raw.split("\n").toVector.map(_.toVector.map(_.toString().toInt)))
  def part1(input: Input): IO[Any] = input.part1
  def part2(input: Input): IO[Any] = input.part2
}

case class Pos(x: Int, y: Int)

case class TreeGrid(grid: Vector[Vector[Int]]) { self =>
  val rows = grid.length
  val cols = grid.head.length

  val allPos = for {
    y <- (0 until rows).toVector
    x <- (0 until cols)
  } yield Pos(x, y)

  val part1: IO[Int] = allPos.parTraverse(isVisibleFromOutside).map(_.count(identity))
  val part2: IO[Int] = allPos.parTraverse(visibilityFromInside).map(_.reduce(_ max _))

  def lookup(pos: Pos): Int = {
    for {
      row  <- grid.lift(pos.y)
      tree <- row.lift(pos.x)
    } yield tree
  } getOrElse -1

  def calculate(pos: Pos)(treeLineReduce: List[Int] => Int)(directionReduce: (Int, Int) => Int): Int = List(
    (pos.x to -1 by -1).toList.drop(1).map(x => lookup(Pos(x, pos.y))),
    (pos.y to -1 by -1).toList.drop(1).map(y => lookup(Pos(pos.x, y))),
    (pos.x to cols).toList.drop(1).map(x => lookup(Pos(x, pos.y))),
    (pos.y to rows).toList.drop(1).map(y => lookup(Pos(pos.x, y))),
  ).map(treeLineReduce).reduce(directionReduce)

  def isVisibleFromOutside(pos: Pos): IO[Boolean] = IO {
    val treeHeight = lookup(pos)
    val minHeight  = calculate(pos)(_.reduce(_ max _))(_ min _)
    treeHeight > minHeight
  }

  def visibilityFromInside(pos: Pos): IO[Int] = IO {
    val treeHeight = lookup(pos)
    def viewDistance(trees: List[Int], score: Int = 0): Int = trees match {
      case -1 :: Nil | Nil                     => score
      case height :: _ if height >= treeHeight => score + 1
      case _ :: tail                           => viewDistance(tail, score + 1)
    }

    calculate(pos)(viewDistance(_))(_ * _)
  }
}
