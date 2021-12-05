package common

import magnolia1._

import scala.language.experimental.macros

trait Reads[T] {
  def fullReads(line: List[String]): Reads.Result[T]
  def reads(line: List[String]): T = fullReads(line).value
}

object Reads {
  case class Result[+T](value: T, tail: List[String] = Nil)

  def apply[T: Reads]: Reads[T] = implicitly[Reads[T]]
  def readLine[T: Reads](line: String): T = Reads[T].reads(line.split("[ ,:\\-(\\->)]").toList.filterNot(_ == ""))

  def manual[T](parser: String => T): Reads[T] = (line) => Result(parser(line mkString " "))

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
  implicit val charReads: Reads[Char] = { case head :: tail => Result(head.head, tail)}

  implicit def listReads[T: Reads]: Reads[List[T]] = {
    case Nil | "" :: Nil => Result(Nil)
    case list =>
      val Result(value, tail) = Reads[T].fullReads(list)
      Result(value :: listReads[T].reads(tail))
  }

  implicit def mapReads[K: Reads, V: Reads]: Reads[Map[K, V]] = { line =>
    val mapList = Reads[List[(K, V)]].reads(line.flatMap(_.split(':')))
    Result(mapList.toMap)
  }
}


