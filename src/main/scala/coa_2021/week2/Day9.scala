package coa_2021.week2

import cats.effect.IO
import common.AdventApp

// https://adventofcode.com/2021/day/9

object Day9 extends AdventApp[String](year = 2021, day = 9) {
  def part1(input: List[Input]): IO[Any] = IO {
    val heightMap = HeightMap(input)
    val lowPoints = heightMap.lowPoints
    lowPoints.map(heightMap.riskLevel).sum
  }
  def part2(input: List[Input]): IO[Any] = IO {
    val heightMap = HeightMap(input)
    val basins = heightMap.basins
    basins.takeRight(3).map(_.size).product
  }
}

case class HeightMap(map: Vector[Vector[Int]]) {
  private val width = map.length
  private val height = map.head.length
  val lowPoints: Seq[(Int, Int)] = for {
    x <- 0 until width
    y <- 0 until height
    value = getHeight(x, y)
    if around(x, y).forall(_ > value)
  } yield (x, y)

  def getHeight(pos: (Int, Int)): Int = getHeight(pos._1, pos._2)
  def getHeight(x: Int, y: Int): Int = map(x)(y)

  def riskLevel(pos: (Int, Int)): Int = getHeight(pos) + 1

  def aroundPos(pos: (Int, Int)): List[(Int, Int)] = aroundPos(pos._1, pos._2)
  def aroundPos(x: Int, y: Int): List[(Int, Int)] = List(
    (x - 1, y),
    (x + 1, y),
    (x, y - 1),
    (x, y + 1),
  ) filter { case (x, y) =>
    0 <= x && x < width && 0 <= y && y < height
  }

  def around(pos: (Int, Int)): List[Int] = around(pos._1, pos._2)
  def around(x: Int, y: Int): List[Int] = aroundPos(x, y).map(getHeight)

  def basins: List[Set[(Int, Int)]] = lowPoints.map(basinOf).toList.distinct.sortBy(_.size)

  private def basinOf(pos: (Int, Int)): Set[(Int, Int)] = basinOfLoop(LazyList(pos))
  private def basinOfLoop(pos: LazyList[(Int, Int)], acc: Set[(Int, Int)] = Set.empty): Set[(Int, Int)] = pos match {
    case _ if pos.isEmpty => acc
    case head #:: tail if acc(head) || getHeight(head) == 9 => basinOfLoop(tail, acc)
    case head #:: tail => basinOfLoop(tail #::: aroundPos(head).to(LazyList), acc + head)
  }
}

object HeightMap {
  def apply(input: List[String]): HeightMap = HeightMap(input.toVector.map(_.toVector.map(_.toString.toInt)))
}