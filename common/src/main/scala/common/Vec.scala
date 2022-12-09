package common

final case class Vec(vec: Vector[Int]) { self =>
  val dimentions    = vec.length
  def unary_- : Vec = Vec(vec.map(-_))

  override def toString: String = vec.mkString("[", ",", "]")

  def +(that: Vec): Vec = {
    require(self.dimentions == that.dimentions)
    Vec((self.vec zip that.vec).map { case (a, b) => a + b })
  }

  def -(that: Vec): Vec = {
    require(self.dimentions == that.dimentions)
    Vec((self.vec zip that.vec).map { case (a, b) => a - b })
  }

  def *(amount: Int): Vec = Vec(vec.map(_ * amount))

  def map(f: Int => Int): Vec = Vec(vec.map(f))
}

object Vec {
  def apply(x: Int, y: Int): Vec         = Vec(Vector(x, y))
  def apply(x: Int, y: Int, z: Int): Vec = Vec(Vector(x, y, z))
}
