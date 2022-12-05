package aoc.y2020.week1

import cats.effect.IO
import common.v1.AdventApp

import scala.util.matching.Regex

// To bootstrap new days

object Day4 extends AdventApp[Map[Field, String]](year = 2020, day = 4) {
  def part1(input: List[Input]): IO[String] = IO {
    group(input)
      .count(hasRequired)
      .toString
  }
  def part2(input: List[Input]): IO[String] = IO {
    group(input)
      .filter(hasRequired)
      .count(isValid)
      .toString
  }

  private def group(input: List[Input]): List[Input] = {
    input.foldLeft[List[Input]](List(Map.empty)) {
      case (acc, fields) if fields.isEmpty => acc :+ Map.empty
      case (acc :+ map, fields)            => acc :+ (map ++ fields)
    }
  }

  private def hasRequired(fields: Input): Boolean =
    Field.Required subsetOf fields.keySet
  private def isValid(fields: Input): Boolean = fields.forall {
    case (key, value) => key(value)
  }
}

sealed trait Field {
  def apply(value: String): Boolean = PartialFunction.cond(value)(isValid)
  protected def isValid: PartialFunction[String, Boolean]
}

object Field {
  val Values: Set[Field] = Set(BYR, IYR, EYR, HGT, HCL, ECL, PID, CID)
  val Required: Set[Field] = Values - CID

  private def isBetween(str: String)(min: Int, max: Int): Boolean = {
    val int = str.toInt
    min <= int && int <= max
  }

  case object BYR extends Field { // Birth Year
    val Pattern = "(\\d{4})".r
    def isValid = { case Pattern(year) =>
      isBetween(year)(1920, 2002)
    }
  }

  case object IYR extends Field { // Issue Year
    val Pattern = "(\\d{4})".r
    def isValid = { case Pattern(year) =>
      isBetween(year)(2010, 2020)
    }
  }

  case object EYR extends Field { // Expiration Year
    val Pattern = "(\\d{4})".r
    def isValid = { case Pattern(year) =>
      isBetween(year)(2020, 2030)
    }
  }

  case object HGT extends Field { // Height
    val CM: Regex = "(\\d+)cm".r
    val IN: Regex = "(\\d+)in".r
    def isValid = {
      case CM(hgt) => isBetween(hgt)(150, 193)
      case IN(hgt) => isBetween(hgt)(59, 79)
    }
  }

  case object HCL extends Field { // Hair Color
    val Pattern: Regex = "(#[0-9a-f]{6})".r
    def isValid = { case Pattern(_) =>
      true
    }
  }

  case object ECL extends Field { // Eye Color
    val Pattern: Regex = "(amb|blu|brn|gry|grn|hzl|oth)".r
    def isValid = { case Pattern(_) =>
      true
    }
  }

  case object PID extends Field { // Passport ID
    val Pattern: Regex = "(\\d{9})".r
    def isValid = { case Pattern(_) =>
      true
    }
  }

  case object CID extends Field { // Country ID
    def isValid = { _ => true }
  }
}
