package coa_2021.week2

import cats.effect.IO
import common.AdventApp

// https://adventofcode.com/2021/day/10

object Day10 extends AdventApp[String](year = 2021, day = 10) {
  def part1(input: List[Input]): IO[Any] = IO {
    (input zip input.map(fixedLine)).collect {
      case (left, right) if left != right.take(left.length) =>
        val score = (left zip right).collectFirst {
          case (left, right) if left != right => errorScore(left)
        }.sum
        println(s"Corrupted ($score): $left => $right")
        score
      case (left, right) =>
        println(s"Incomplete: $left => $right")
        0
    }.sum
  }
  def part2(input: List[Input]): IO[Any] = IO {
    val scores = (input zip input.map(fixedLine)).collect {
      case (left, right) if left == right.take(left.length) =>
        right.drop(left.length).foldLeft(0L) {
          case (score, char) => score * 5 + incompleteScore(char)
        }
    }.sorted

    println(s"Incomplete scores: $scores")
    scores(scores.length / 2)
  }

  def fixedLine(line: String): String = {
    def loop(line: List[Char], expectedClosing: List[Char] = Nil, acc: List[Char] = Nil): String = line match {
      case Nil => (acc.reverse ++ expectedClosing).mkString
      case (head @ ('(' | '[' | '{' | '<')) :: tail => loop(tail, BracketMap(head) :: expectedClosing, head :: acc)
      case         (')' | ']' | '}' | '>')  :: tail => loop(tail, expectedClosing.tail, expectedClosing.head :: acc)
    }

    loop(line.toList)
  }

  def errorScore(illegalChar: Char): Int = illegalChar match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  def incompleteScore(missingChar: Char): Int = missingChar match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }

  val BracketMap: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>',
  )
}