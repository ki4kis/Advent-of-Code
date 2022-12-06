package aoc.y2022

import cats.effect.IO
import cats.implicits._
import common.v2.AdventApp
import common.v2.Reads

// https://adventofcode.com/2022/day/3

object Day3 extends AdventApp[Vector[Rucksack]](year = 2022, day = 3) {
  def reads(raw: String): Input = raw
    .split("\n")
    .toVector
    .map(Rucksack.fromLine)

  def part1(input: Input): IO[Any] = IO(
    input.flatMap(_.overlap).map(_.priority).sum
  )
  def part2(input: Input): IO[Any] = IO {
    val groups = for {
      group <- input.grouped(3)
    } yield group.map(_.fullSet).reduce(_ & _).map(_.priority).sum

    groups.sum
  }
}

case class Item(item: Char) {
  def priority: Int = Item.priorityMap(item)
}

object Item {
  val priorityMap: Map[Char, Int] =
    ((('a' to 'z') ++ ('A' to 'Z')) zip (1 to 52)).toMap
}

case class Rucksack(first: Seq[Item], second: Seq[Item]) {
  val firstSet = first.toSet
  val secondSet = second.toSet
  val fullSet = firstSet ++ secondSet

  def overlap: Set[Item] = firstSet & secondSet
}

object Rucksack {
  def fromLine(line: String): Rucksack = {
    val (first, second) = line.splitAt(line.length / 2)
    Rucksack(first.map(Item.apply), second.map(Item.apply))
  }
}
