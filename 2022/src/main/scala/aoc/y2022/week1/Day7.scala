package aoc.y2022

import cats.effect.IO
import cats.implicits._
import common.v2.AdventApp
import common.v2.Reads
import scala.collection.immutable.SortedMap

// https://adventofcode.com/2022/day/7

object Day7 extends AdventApp[FileSystem.System](year = 2022, day = 7) {
  val totalSpace = 70000000L
  val requiredSpace = 30000000L

  def reads(raw: String): Input = FileSystem.parse(raw)
  def part1(input: Input): IO[Any] = IO(
    input.pathSizes.filter(_._2 <= 100000).values.sum
  )
  def part2(input: Input): IO[Any] = IO {
    val pathSizes = input.pathSizes
    val usedSpace = pathSizes(Path())

    val deleteSize = requiredSpace - (totalSpace - usedSpace)
    input.pathSizes.values.toList.sorted.find(_ >= deleteSize).get
  }
}

case class Path(path: List[String] = Nil) extends Ordered[Path] { self =>
  override def toString = "/" + path.reverse.mkString("/")
  protected val key = (path.length, toString)
  def compare(that: Path): Int =
    Ordering[(Int, String)].compare(self.key, that.key)

  def parent = Path(path.tail)

  def /(dir: String): Path = dir match {
    case "/"  => Path()
    case ".." => parent
    case dir  => Path(dir :: path)
  }
}

sealed trait FileSystem {
  def name: String
}

object FileSystem {
  private val RDir = "dir ([\\w\\.]+)".r
  private val RFile = "(\\d+) ([\\w\\.]+)".r

  def parse(raw: String): System = parse(
    raw.split("\n").map(Cmd.fromLine).toList.tail
  )

  def parse(
      cmds: List[Either[Cmd, FileSystem]],
      path: Path = Path(),
      files: Map[Path, List[FileSystem]] = Map.empty
  ): System = {
    import Cmd._

    cmds match {
      case Nil                   => System(SortedMap.from(files))
      case Left(cd(dir)) :: tail => parse(tail, path / dir, files)
      case Left(ls) :: tail      => parse(tail, path, files.updated(path, Nil))
      case Right(file) :: tail =>
        val currentList = files.getOrElse(path, Nil)
        parse(tail, path, files.updated(path, file :: currentList))
    }
  }

  def fromLine(raw: String) = raw match {
    case RDir(dir)         => Dir(dir)
    case RFile(size, name) => File(name, size.toLong)
  }

  case class System(files: SortedMap[Path, List[FileSystem]]) {
    override def toString = files.mkString("\n")
    lazy val pathSizes = {
      def loop(
          paths: List[Path],
          acc: Map[Path, Long] = Map.empty
      ): Map[Path, Long] = paths match {
        case Nil => acc
        case path :: tail =>
          val size = files(path).map {
            case File(_, size) => size
            case Dir(name)     => acc(path / name)
          }.sum

          loop(tail, acc.updated(path, size))
      }

      loop(files.keys.toList.reverse)
    }
  }

  case class Dir(name: String) extends FileSystem
  case class File(name: String, size: Long) extends FileSystem
}

sealed trait Cmd

object Cmd {
  private val Rcd = "\\$ cd ([\\/\\.\\w]+)".r
  private val Rls = "$ ls"

  def fromLine(raw: String): Either[Cmd, FileSystem] = raw match {
    case Rcd(dir) => Left(cd(dir))
    case Rls      => Left(ls)
    case file     => Right(FileSystem.fromLine(raw))
  }

  case class cd(dir: String) extends Cmd
  case object ls extends Cmd
}
