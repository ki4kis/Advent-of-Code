package coa_2021

import language.experimental.macros, magnolia1._

trait Reads[T] {
  def fullReads(line: List[String]): Reads.Result[T]
  def reads(line: List[String]): T = fullReads(line).value
}

object Reads {
  def readLine[T: Reads](line: String): T = implicitly[Reads[T]].reads(line.split(' ').toList)
  case class Result[+T](value: T, tail: List[String] = Nil)

  type Typeclass[T] = Reads[T]

  def join[T](ctx: CaseClass[Reads, T]): Reads[T] = (line: List[String]) => {
    val (tail, params) = ctx.parameters.foldLeft[(List[String], List[Any])]((line, Nil)) { case ((input, acc), param) =>
      val res = param.typeclass.fullReads(input)
      (res.tail, acc :+ res.value)
    }

    Result(ctx.rawConstruct(params), tail)
  }

  def split[T](ctx: SealedTrait[Reads, T]): Reads[T] = {
    case head :: tail =>
      val subType = ctx.subtypes.find(subType => subType.typeName.short.toLowerCase == head).get
      subType.typeclass.fullReads(tail)
  }

  implicit def gen[T]: Reads[T] = macro Magnolia.gen[T]

  implicit val intReads: Reads[Int] = { case head :: tail => Result(head.toInt, tail) }
  implicit val strReads: Reads[String] = { case head :: tail => Result(head, tail) }
}


