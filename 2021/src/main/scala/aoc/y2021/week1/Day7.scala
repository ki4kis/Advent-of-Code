package aoc.y2021.week1

import cats.effect.IO
import common.v1.AdventApp

import scala.annotation.tailrec

// https://adventofcode.com/2021/day/7

object Day7 extends AdventApp[List[Int]](year = 2021, day = 7) {
  type FuelCost = Double => Double

  def part1(input: List[Input]): IO[Any] = IO {
    val positions = input.head.sorted
    minFuel(positions)(fuelCost1)
  }

  def part2(input: List[Input]): IO[Any] = IO {
    val positions = input.head.sorted
    minFuel(positions)(fuelCost2)
  }

  def minFuel(input: Input)(implicit fuelCost: FuelCost): (Int, Int) = {
    val min = input.head.toDouble
    val max = input.last.toDouble

    @tailrec
    def loop(
        min: Double,
        max: Double,
        minFuel: Double,
        maxFuel: Double
    ): (Int, Int) = {
      val mid = (min + max) / 2
      if (max - min < 0.5) {
        val pos = math.round(mid).toInt
        val fuel = input.map(fuelTo(pos)).sum
        pos -> fuel
      } else {
        val midFuel = input.map(fuelTo(mid)).sum
        if (minFuel < maxFuel) loop(min, mid, minFuel, midFuel)
        else loop(mid, max, midFuel, maxFuel)
      }
    }

    loop(min, max, input.map(fuelTo(min)).sum, input.map(fuelTo(max)).sum)
  }

  def fuelTo(to: Int)(from: Int)(implicit fuelCost: FuelCost): Int =
    math.round(fuelCost(math.abs(to - from))).toInt
  def fuelTo(to: Double)(from: Int)(implicit fuelCost: FuelCost): Double =
    fuelCost(math.abs(to - from))

  def fuelCost1(dist: Double): Double = dist
  def fuelCost2(dist: Double): Double = (0 to math.round(dist).toInt).sum
}
