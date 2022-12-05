package aoc.y2020.week1

import cats.implicits._
import cats.effect.IO
import common.v1.AdventApp

// To bootstrap new days

object Day7 extends AdventApp[List[String]](year = 2020, day = 7) {
  def part1(input: List[Input]): IO[String] = IO {
    val rules = input.map(Bag.parse)

    val bagContains = rules.foldLeft[Map[String, Set[String]]](Map.empty) {
      case (acc, bag) =>
        val keys = bag.contain.keySet
        keys.foldLeft(acc) { case (acc, key) =>
          acc.updated(key, acc.getOrElse(key, Set.empty) + bag.color)
        }
    }

    def loop(bags: List[String], acc: Set[String] = Set.empty): Set[String] = {
      val outerBags = bags.flatMap(bagContains.get).flatten
      val newAcc = acc ++ outerBags
      if (newAcc == acc) acc
      else loop(outerBags, newAcc)
    }

    loop(List("shiny gold")).size.toString
  }

  def part2(input: List[Input]): IO[String] = {
    val rules = input.map(Bag.parse)
    val rulesMap = rules.map(rule => rule.color -> rule.contain).toMap

    def countBags(bag: String): IO[Int] = rulesMap(bag) match {
      case contain =>
        contain.toList
          .traverse { case (bag, amount) =>
            countBags(bag).map(_ * amount)
          }
          .map(_.sum + 1)
    }

    countBags("shiny gold").map(_ - 1).map(_.toString)
  }
}

case class Bag(color: String, contain: Map[String, Int] = Map.empty)

object Bag {
  val BagR = "(\\w+ \\w+) bags?".r
  val BagAmountR = "(\\d+) (\\w+ \\w+) bags?".r

  def parse(line: List[String]): Bag = parse(line.mkString(" "))
  def parse(line: String): Bag = {
    val Array(BagR(innerBag), innerBags) = line.split(" contain ")
    val innerBagAmounts = for {
      innerBag <- innerBags.dropRight(1).split(", ").toList
      if innerBag != "no other bags"
      BagAmountR(amount, bag) = innerBag
    } yield bag -> amount.toInt

    Bag(innerBag, innerBagAmounts.toMap)
  }
}
