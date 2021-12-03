package coa_2020.week1

import cats.effect.IO
import common.AdventApp

// To bootstrap new days

object Day3 extends AdventApp[String](year = 2020, day = 3) {
  def part1(input: List[Input]): IO[String] = IO {
    Forest(input).countTrees(right = 3, down = 1).toString
  }
  def part2(input: List[Input]): IO[String] = IO {
    val forest = Forest(input)
    List(
      forest.countTrees(right = 1, down = 1),
      forest.countTrees(right = 3, down = 1),
      forest.countTrees(right = 5, down = 1),
      forest.countTrees(right = 7, down = 1),
      forest.countTrees(right = 1, down = 2),
    ).product.toString
  }
}

case class Pos(x: Int, y: Int)

case class Forest(rows: List[String], width: Int) {
  private val treePosList = for {
    (row, y) <- rows.zipWithIndex
    (char, x) <- row.zipWithIndex
    if char == '#'
  } yield Pos(x, y)

  private val treePos = treePosList.toSet
  private val maxPos = Pos(width, treePos.map(_.y).max)

  def countTrees(right: Int, down: Int): Int = {
    val xPath = LazyList.from(0).map(_ * right % maxPos.x)
    val yPath = LazyList.from(0).map(_ * down).takeWhile(_ <= maxPos.y)

    val pathPosList = for {
      (x, y) <- xPath zip yPath
    } yield Pos(x, y)

    val pathPos = pathPosList.toSet
    val treeInPathPos = treePos intersect pathPos

    treeInPathPos.size
  }
}

object Forest {
  val Tree: Char = '#'
  def apply(rows: List[String]): Forest = Forest(rows, rows.head.length)
}