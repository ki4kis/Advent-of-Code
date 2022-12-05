package aoc.y2020.week1

import cats.effect.IO
import common.v1.AdventApp

// To bootstrap new days

object Day5 extends AdventApp[String](year = 2020, day = 5) {
  def part1(input: List[Input]): IO[String] = IO {
    input
      .map(Seat.apply)
      .map(_.id)
      .max
      .toString
  }
  def part2(input: List[Input]): IO[String] = IO {
    val Some(seatId) = input
      .map(Seat.apply)
      .map(_.id)
      .sorted
      .sliding(2)
      .collectFirst {
        case List(a, b) if a + 2 == b => a + 1
      }

    seatId.toString
  }
}

case class Seat(row: Int, column: Int) {
  def id: Int = row * 8 + column
  def code: String = {
    val rowCode = (("0" * 7) + row.toBinaryString)
      .replace('0', 'F')
      .replace('1', 'B')
      .takeRight(7)

    val colCode = (("0" * 3) + column.toBinaryString)
      .replace('0', 'L')
      .replace('1', 'R')
      .takeRight(3)

    rowCode + colCode
  }
  override def toString = s"$code: row $row, column $column, seatId $id"
}

object Seat {
  def apply(code: String): Seat = {
    val (row, col) = code.splitAt(7)
    val rowBin = row
      .replace('F', '0')
      .replace('B', '1')

    val colBin = col
      .replace('L', '0')
      .replace('R', '1')

    Seat(
      Integer.parseInt(rowBin, 2),
      Integer.parseInt(colBin, 2)
    )
  }
}
