package common.v2

import cats.data.StateT
import cats.effect.IO
import scala.util.matching.Regex

object Reads {
  type State[T] = StateT[IO, String, T]

  private val Line = "(.*)\n".r
  private val Chunk = "(.*)\n\n".r
  private val Number = "(\\d+)".r

  def fromRegex[T](regex: Regex)(f: String => T): State[T] = {
    StateT { raw =>
      IO {
        val value = regex.findPrefixOf(raw).get
        raw.drop(value.length) -> f(value)
      }
    }
  }

  val takeLine = fromRegex(Line) { case Line(line) => line }
  val takeChunk = fromRegex(Chunk) { case Chunk(chunk) => chunk }
  val takeInt = fromRegex(Number) { case Number(int) => int.toInt }
}
