package coa_2021.week1

import cats.effect.IO
import common.AdventApp

// https://adventofcode.com/2021/day/4

object Day4 extends AdventApp[List[Int]](year = 2021, day = 4) {
  def part1(input: List[Input]): IO[Any] = IO {
    val numbers = input.head
    val boards = for {
      board <- input.tail.grouped(6).map(_.drop(1))
    } yield Board(board)

    def loop(numbers: List[Int] = numbers, boards: List[Board] = boards.toList): Int = numbers match {
      case head :: tail =>
        val (newBoards, scores) = boards.map(_.guess(head)).unzip
        scores.flatten.maxOption match {
          case Some(value) => value
          case None => loop(tail, newBoards)
        }
      case Nil => -1
    }

    loop()
  }

  def part2(input: List[Input]): IO[Int] = IO {
    val numbers = input.head
    val boards = for {
      board <- input.tail.grouped(6).map(_.drop(1))
    } yield Board(board)

    def loop(numbers: List[Int] = numbers, boards: List[Board] = boards.toList): Int = numbers match {
      case Nil => -1
      case head :: tail => boards.map(_.guess(head)) match {
        case Nil => -1
        case (_, Some(score)) :: Nil => score
        case boards =>
          val (newBoards, _) = boards.filterNot(_._2.nonEmpty).unzip
          loop(tail, newBoards)
      }
    }

    loop()
  }
}

case class Pos(x: Int, y: Int)

case class Board(board: Map[Int, Pos], totalSum: Int, guessed: Set[Pos] = Set.empty) { self =>
  private lazy val scoreBoard = board.map(_.swap)

  def guess(value: Int): (Board, Option[Int]) = board.get(value) match {
    case Some(pos) =>
      val board = copy(guessed = guessed + pos)
      (board, board.score(value))
    case None      => (self, None)
  }

  private def score(lastGuessed: Int): Option[Int] = {
    val allWins = for {
      line <- Board.allWins
      if line.subsetOf(guessed)
      markedSum = guessed.map(scoreBoard).sum
    } yield lastGuessed * (totalSum - markedSum)

    allWins.maxOption
  }
}

object Board {
  def apply(board: List[List[Int]]): Board = {
    val boardMap = for {
      x <- 0 to 4
      y <- 0 to 4
    } yield board(x)(y) -> Pos(x, y)

    Board(boardMap.toMap, board.flatten.sum)
  }

  private val verticalWins = for {
    x <- (0 to 4).toList
  } yield for {
    y <- (0 to 4).toList
  } yield Pos(x, y)

  private val horizontalWins = verticalWins.transpose

  val allWins = for {
    winSet <- verticalWins ++ horizontalWins
  } yield winSet.toSet
}