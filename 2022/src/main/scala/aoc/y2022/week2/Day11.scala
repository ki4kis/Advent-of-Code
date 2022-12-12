package aoc.y2022.week2

import cats.effect.IO
import common.v2.AdventApp
import common.v2.Reads
import scala.collection.immutable.SortedMap

// To bootstrap new days

object Day11 extends AdventApp(year = 2022, day = 11) {
  type Input = Monkeys
  def reads(raw: String): Input    = Monkeys(SortedMap.from(raw.split("\n\n").map(Monkey.parse)))
  def part1(input: Input): IO[Any] = IO(input.rounds(20)(_ / 3).monkeyBusiness)
  def part2(input: Input): IO[Any] = IO(input.rounds(10000)(identity).monkeyBusiness)
}

case class Item()

case class Monkeys(
    monkeys: SortedMap[Int, Monkey],
    lcm: Int,
    inspections: SortedMap[Int, Int] = SortedMap.empty,
) { self =>
  def update(id: Int, f: Monkey => Monkey): Monkeys = copy(monkeys = monkeys.updated(id, f(monkeys(id))))

  def inspected(id: Int, count: Int): Monkeys = {
    copy(inspections = inspections.updated(id, inspections.getOrElse(id, 0) + count))
  }

  def thrown(to: Int, item: Long) = update(to, _.receive(item % lcm))

  def turn(id: Int, relief: Long => Long): Monkeys = {
    val (monkey, throws) = monkeys(id).turn(relief)
    val state            = update(id, _ => monkey).inspected(id, throws.length)

    throws.foldLeft(state) { case (monkeys, (to, item)) =>
      monkeys.thrown(to, item)
    }
  }

  def round(relief: Long => Long): Monkeys = monkeys.keys.foldLeft(self)(_.turn(_, relief))
  def rounds(n: Int)(relief: Long => Long): Monkeys =
    (1 to n).foldLeft(self) { case (monkeys, _) => monkeys.round(relief) }

  def monkeyBusiness = inspections.values.toList.sorted.takeRight(2).map(_.toLong).product
}

object Monkeys {
  def apply(monkeys: SortedMap[Int, Monkey]): Monkeys = {
    val allMods = monkeys.values.map(_.mod).toList
    val lcm     = allMods.product

    Monkeys(monkeys, lcm)
  }
}

case class Monkey(
    items: List[Long],
    ope: Long => Long,
    mod: Int,
    ifTrue: Int,
    ifFalse: Int,
) {
  def throwTo(item: Long): Int    = if (item % mod == 0) ifTrue else ifFalse
  def receive(item: Long): Monkey = copy(items = items :+ item)
  def turn(relief: Long => Long): (Monkey, List[(Int, Long)]) = copy(items = Nil) -> (for {
    item <- items
    worry = relief(ope(item))
  } yield throwTo(worry) -> worry)
}

object Monkey {
  val Monkey1R = raw"""Monkey (\d+):""".r
  val Monkey2R = raw"""  Starting items: (.*)""".r
  val Monkey3R = raw"""  Operation: new = (.*)""".r
  val Monkey4R = raw"""  Test: divisible by (\d+)""".r
  val Monkey5R = raw"""    If true: throw to monkey (\d+)""".r
  val Monkey6R = raw"""    If false: throw to monkey (\d+)""".r

  val OpeR = raw"old ([\+\*]) (\w+)".r

  def parse(raw: String): (Int, Monkey) = raw.split("\n").toList match {
    case List(
          Monkey1R(id),
          Monkey2R(rawItems),
          Monkey3R(rawOpe),
          Monkey4R(modStr),
          Monkey5R(ifTrueStr),
          Monkey6R(ifFalseStr),
        ) =>
      // case MonkeyR(id, rawItems, rawOpe, modStr, ifTrueStr, ifFalseStr) =>
      val items   = rawItems.split(", ").map(_.toLong).toList
      val mod     = modStr.toInt
      val ifTrue  = ifTrueStr.toInt
      val ifFalse = ifFalseStr.toInt
      val ope: Long => Long = rawOpe match {
        case OpeR("+", "old") => (old: Long) => old + old
        case OpeR("*", "old") => (old: Long) => old * old
        case OpeR("+", nrStr) =>
          val nr = nrStr.toInt
          (old: Long) => old + nr
        case OpeR("*", nrStr) =>
          val nr = nrStr.toInt
          (old: Long) => old * nr
      }

      id.toInt -> Monkey(items, ope, mod, ifTrue, ifFalse)
  }
}
