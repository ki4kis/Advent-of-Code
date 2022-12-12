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

  def neighbors4(pos: Vec): Map[Vec, T] = {
    for {
      direction <- Grid.directions4
      neighbor = pos + direction
      value <- lift(neighbor)
    } yield neighbor -> value
  }.toMap

  def neighbors8(pos: Vec): Map[Vec, T] = {
    for {
      direction <- Grid.directions8
      neighbor = pos + direction
      value <- lift(neighbor)
    } yield neighbor -> value
  }.toMap
}

object Grid {
  def fromChars(raw: String): Grid[Char] = Grid(raw.split("\n").toVector.map(_.toVector))

  private[Grid] val directions4 = (-1 to 1 by 2).map(Vec(_, 0)) ++ (-1 to 1 by 2).map(Vec(0, _))
  private[Grid] val directions8 = directions4 ++ (for {
    x <- (-1 to 1 by 2).map(Vec(_, 0))
    y <- (-1 to 1 by 2).map(Vec(0, _))
  } yield x + y)
}
