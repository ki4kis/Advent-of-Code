package common

final case class Grid[T](grid: Vector[Vector[T]]) {
  val rows: Range = 0 until grid.length
  val cols: Range = 0 until grid.head.length

  def lift: Vec => Option[T] = (pos: Vec) => grid.lift(pos.y).flatMap(_.lift(pos.x))
  def apply(pos: Vec): T     = lift(pos).get

  def map[S](f: T => S): Grid[S] = Grid(grid.map(_.map(f)))
  def filter(f: T => Boolean): Map[Vec, T] = {
    for {
      (row, y)   <- grid.zipWithIndex
      (value, x) <- row.zipWithIndex
      if f(value)
    } yield Vec(x, y) -> value
  }.toMap
}

object Grid {
  def fromChars(raw: String): Grid[Char] = Grid(raw.split("\n").toVector.map(_.toVector))
}
