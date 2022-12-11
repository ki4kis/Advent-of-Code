package aoc.y2022.week1

import cats.effect.IO
import cats.implicits._
import cats.data.StateT
import common.v2.AdventApp
import scala.collection.immutable.ListMap

// https://adventofcode.com/2022/day/5

object Day5 extends AdventApp(year = 2022, day = 5) {
  type Input = Ship
  def reads(raw: String): Input    = Ship.fromRaw(raw)
  def part1(input: Input): IO[Any] = Ship.part1.runA(input)
  def part2(input: Input): IO[Any] = Ship.part2.runA(input)
}

case class Ship(
    stacks: ListMap[Int, List[String]],
    instructions: List[Instruction],
) { self =>
  def readTop        = stacks.values.map(_.head).mkString
  def hasInstruction = instructions.nonEmpty

  def takeInstruction: (Ship, Instruction) = instructions match {
    case head :: tail => copy(instructions = tail) -> head
  }

  def move(amount: Int, from: Int, to: Int): Ship = {
    val fromStack = stacks(from)
    val head      = fromStack.take(amount)
    val tail      = fromStack.drop(amount)
    val toStack   = stacks(to)

    copy(stacks = stacks ++ Map(from -> tail, to -> (head ::: toStack)))
  }
}

object Ship {
  def fromRaw(raw: String): Ship = {
    val Array(rawStacks, rawInsts) = raw.split("\n\n")

    val stacks = Stack.fromLines(rawStacks.split("\n").toList)
    val insts  = rawInsts.split("\n").toList.map(Instruction.fromLine)

    Ship(stacks, insts)
  }

  type State[T] = StateT[IO, Ship, T]

  def move(amount: Int, from: Int, to: Int): State[Unit] = StateT.modify(_.move(amount, from, to))

  val takeInst: State[Instruction] = StateT(ship => IO(ship.takeInstruction))
  val hasInst: State[Boolean]      = StateT.inspect(_.hasInstruction)
  val readTop: State[String]       = StateT.inspect(_.readTop)

  def run(doInstruction: State[Unit]): State[String] = {
    lazy val loop: State[Unit] = doInstruction *> (hasInst >>= loop.whenA)
    loop *> readTop
  }

  val instPart1: State[Unit] = for {
    Instruction(amount, from, to) <- takeInst
    _                             <- move(amount = 1, from, to).replicateA(amount)
  } yield ()

  val instPart2: State[Unit] = for {
    Instruction(amount, from, to) <- takeInst
    _                             <- move(amount, from, to)
  } yield ()

  val part1 = run(instPart1)
  val part2 = run(instPart2)
}

object Stack {
  val Crate = " *\\[(\\w)\\] *".r
  val Idx   = " *(\\d) *".r

  def fromLines(lines: List[String]): ListMap[Int, List[String]] =
    ListMap.from {
      for {
        (Idx(idx) :: stack) <- lines
          .map(_.grouped(4).toList)
          .reverse
          .transpose
        parsedStack = stack.reverse.collect { case Crate(crate) => crate }
      } yield (idx.toInt) -> parsedStack
    }
}

case class Instruction(move: Int, from: Int, to: Int)

object Instruction {
  val Line = "move (\\d+) from (\\d+) to (\\d+)".r

  def fromLine(line: String): Instruction = line match {
    case Line(move, from, to) => Instruction(move.toInt, from.toInt, to.toInt)
  }
}
