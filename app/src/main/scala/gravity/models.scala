package gravity

import shapeless.{HList, Witness}
import shapeless.ops.record.Selector

object models {

  /*
  trait Referenceable[S <: Symbol] {
    def idField: S
  } */
  trait Referenceable[T, S <: Symbol] {
    type L <: HList
    def ev: Witness.Aux[S]
    def ev2: Selector.Aux[L, S, ObjectId]
    def idField: S
  }

  trait Reference[T]

  type ZeroOrOne[T] = Option[One[T]]

  trait Many[T] extends Reference[T]

  trait One[T] extends Reference[T] {
    def map[B](f: T => B): One[B] = ???
  }
  case class OneId[T](id: ObjectId) extends One[T]
  // resolved should always have an id too
  //case class ResolvedOne[T, R](r: R) extends One[T]

  case class Phone(phone: String)
  case class Url(url: String)

  case class ObjectId(id: Int)
}
