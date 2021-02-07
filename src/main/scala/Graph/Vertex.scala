package Graph

import cats._
import cats.implicits._

case class Vertex[T](name: T)

object Vertex {
  def unapply[T](arg: Vertex[T]): Option[T] = arg.name.pure[Option]
}