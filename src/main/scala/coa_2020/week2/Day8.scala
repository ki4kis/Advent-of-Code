package coa_2020.week2

import cats.effect.IO
import cats.implicits.{catsSyntaxParallelSequence1, catsSyntaxParallelTraverse1, toTraverseOps}
import common.AdventApp

import scala.collection.immutable.Queue

// To bootstrap new days

object Day8 extends AdventApp[Operation](year = 2020, day = 8) {
  import Operation._

  def part1(input: List[Input]): IO[String] = IO {
    val code = input.toVector
    def loop(pos: Int = 0, acc: Int = 0, used: Set[Int] = Set.empty): Int =
      if (used(pos)) acc
      else code(pos) match {
        case Acc(arg) => loop(pos + 1, acc + arg, used + pos)
        case Jmp(arg) => loop(pos + arg, acc, used + pos)
        case Nop(_)   => loop(pos + 1, acc, used + pos)
      }

    loop().toString
  }
  def part2(input: List[Input]): IO[String] = IO {
    val end = input.length

    def permutations(code: List[Operation], acc: LazyList[List[Input]]): LazyList[List[Input]] = code match {
      case Nil                => acc.map(_.reverse)
      case (cmd: Acc) :: tail => permutations(tail, acc.map(cmd :: _))
      case (cmd: Jmp) :: tail =>
        val newAcc = acc.map(cmd :: _) #::: LazyList(cmd.toNop :: acc.head)
        permutations(tail, newAcc)
      case (cmd: Nop) :: tail =>
        val newAcc = acc.map(cmd :: _) #::: LazyList(cmd.toJmp :: acc.head)
        permutations(tail, newAcc)
    }

    def loop(pos: Int = 0, acc: Int = 0, used: Set[Int] = Set.empty)(code: Vector[Input]): Int =
      if (end == pos) acc
      else if (used(pos) || pos > end || pos < 0) -1
      else code(pos) match {
        case Acc(arg) => loop(pos + 1, acc + arg, used + pos)(code)
        case Jmp(arg) => loop(pos + arg, acc, used + pos)(code)
        case Nop(_) => loop(pos + 1, acc, used + pos)(code)
      }

    val allCodes = permutations(input.tail, LazyList(List(input.head))).map(_.toVector)

    allCodes.map(loop()).find(_ != -1).get.toString
  }
}

sealed trait Operation {
  def arg: Int
}

object Operation {
  case class Acc(arg: Int) extends Operation
  case class Jmp(arg: Int) extends Operation {
    def toNop: Nop = Nop(arg)
  }
  case class Nop(arg: Int) extends Operation {
    def toJmp: Jmp = Jmp(arg)
  }
}