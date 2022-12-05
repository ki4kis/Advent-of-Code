package aoc.y2021.week2

import cats.effect.IO
import common.v1.AdventApp

// To bootstrap new days

object Day11 extends AdventApp[String](year = 2021, day = 11) {
  implicit val edges: Edges = Edges(Vec(0, 0), Vec(9, 9))
  def part1(input: List[Input]): IO[Any] = IO {
    OctoMap(input).steps(edges)(100).flashes
  }
  def part2(input: List[Input]): IO[Any] = IO {
    OctoMap(input).sync
  }
}

case class Vec(x: Int, y: Int) { self =>
  def unary_- : Vec = Vec(-self.x, -self.y)
  def +(that: Vec): Vec = Vec(self.x + that.x, self.y + that.y)
  def -(that: Vec): Vec = Vec(self.x - that.x, self.y - that.y)

  def sides(implicit edges: Edges): List[Vec] =
    Vec.sides.map(self + _) filter edges.isWithin
  def corners(implicit edges: Edges): List[Vec] =
    Vec.corners.map(self + _) filter edges.isWithin
  def allAround(implicit edges: Edges): List[Vec] =
    Vec.allAround.map(self + _) filter edges.isWithin
}

object Vec {
  val sides: List[Vec] = for {
    i <- List(-1, 1)
    pos <- List(Vec(i, 0), Vec(0, i))
  } yield pos

  val corners: List[Vec] = for {
    x <- List(-1, 1)
    y <- List(-1, 1)
  } yield Vec(x, y)

  val allAround: List[Vec] = sides ++ corners
}

case class Edges(topLeft: Vec, bottomRight: Vec) {
  def isWithin(vec: Vec): Boolean = {
    def isBetween(lower: Int, upper: Int)(value: Int) =
      lower <= value && value <= upper
    val betweenX = isBetween(topLeft.x, bottomRight.x)(vec.x)
    val betweenY = isBetween(topLeft.y, bottomRight.y)(vec.y)
    betweenX && betweenY
  }
}

case class OctoMap(map: Map[Vec, Int], flashes: Int = 0) { self =>
  def steps(implicit edges: Edges): LazyList[OctoMap] =
    self #:: self.nextStep.steps

  def sync(implicit edges: Edges): Int = steps.sliding(2).indexWhere {
    case LazyList(prev, next) => next.flashes - prev.flashes == 100
  } + 1

  def nextStep(implicit edges: Edges): OctoMap = {
    def loop(incPos: List[Vec], acc: Map[Vec, Int]): OctoMap = incPos match {
      case Nil => OctoMap(acc, flashes).reset
      case head :: tail =>
        val value = acc(head) + 1
        val newAcc = acc.updated(head, value)
        if (value == 10) loop(head.allAround ::: tail, newAcc)
        else loop(tail, newAcc)
    }

    loop(map.keys.toList, map)
  }

  private def countFlashes: Int = map.values.count(_ > 9)
  private def reset: OctoMap = OctoMap(
    map.view.mapValues {
      case flashed if flashed > 9 => 0
      case energy                 => energy
    }.toMap,
    flashes + countFlashes
  )

  def show(implicit edges: Edges): String = {
    val values = for {
      y <- edges.topLeft.y to edges.bottomRight.y
      x <- edges.topLeft.x to edges.bottomRight.x
    } yield map(Vec(x, y))

    values.mkString.grouped(edges.bottomRight.x + 1).mkString("\n")
  }
}

object OctoMap {
  def apply(input: List[String]): OctoMap = {
    val map = for {
      (line, y) <- input.zipWithIndex
      (energy, x) <- line.zipWithIndex
    } yield Vec(x, y) -> energy.toString.toInt

    OctoMap(map.toMap)
  }
}
