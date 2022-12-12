package aoc.y2022.week2

import cats.effect.IO
import cats.implicits._
import common.v2.AdventApp
import common.v2.Reads
import common.Vec
import common.Grid

// https://adventofcode.com/2022/day/12

object Day12 extends AdventApp(year = 2022, day = 12) {
  import Day12Helper._
  type Input = MapGrid
  def reads(raw: String): Input    = MapGrid.fromRaw(raw)
  def part1(input: Input): IO[Any] = IO(input.part1.length)
  def part2(input: Input): IO[Any] = IO(input.part2.length)
}

object Day12Helper {
  val allDirections = List(Vec(0, 1), Vec(0, -1), Vec(1, 0), Vec(-1, 0))
  case class MapGrid(grid: Grid[Int], start: Vec, end: Vec) {
    def loop(paths: LazyList[List[Vec]], visited: Set[Vec]): LazyList[List[Vec]] = {
      val newVisited = visited ++ paths.map(_.head)

      def nextPaths = paths
        .flatMap { path =>
          val head   = path.head
          val height = grid(head)
          for {
            direction <- allDirections
            nextStep = head + direction
            nextHeight <- grid.lift(nextStep)
            heightDiff = nextHeight - height
            if heightDiff <= 1 && !newVisited(nextStep)
          } yield nextStep :: path
        }
        .groupBy(_.head)
        .values
        .map(_.head)
        .to(LazyList)

      paths #::: loop(nextPaths, newVisited)
    }

    def part1 = loop(LazyList(List(start)), visited = Set.empty)
      .find { case head :: _ => head == end }
      .get
      .init

    def part2 = {
      val allStarts = grid.filter(_ == 0).keys
      loop(allStarts.map(_.pure[List]).to(LazyList), visited = Set.empty)
        .find { case head :: _ => head == end }
        .get
        .init
    }
  }

  object MapGrid {
    def fromRaw(raw: String): MapGrid = {
      val heightMap: Map[Char, Int] = ('a' to 'z').zipWithIndex.toMap ++ Map('S' -> 0, 'E' -> 25)

      val charGrid   = Grid.fromChars(raw)
      val specialPos = charGrid.filter(value => value == 'S' || value == 'E').map(_.swap)

      MapGrid(charGrid.map(heightMap), specialPos('S'), specialPos('E'))
    }
  }
}
