package gravity.ui

import generic._
import shapeless.{$up => _, _}

/**
  * Type class supporting injection of app page types into libraries that use those pages (either
  * generating page contents or linking to them)
  *
  * @tparam P page type
  * @tparam T route arguments from which the page can be instantiated or destructured to
  */
trait Routable[P, T] {
  def apply(t: T): P
  def unapply(p: P): T
}

object Routable {

  /**
    * @tparam C a case class
    * @tparam T a tuple type
    */
  implicit def routableTupleCaseClass[C <: Product, T]
  (implicit tg: TupleGeneric.Aux[C, T]) = new Routable[C, T] {
    def apply(t: T) = tg.from(t)
    def unapply(p: C) = tg.to(p)
  }

  /**
    * @tparam C a case class
    * @tparam A a non-product type (not a tuple)
    */
  implicit def routableTuple1CaseClass[C <: Product, A]
  (implicit tg: TupleGeneric.Aux[C, Tuple1[A]]) = new Routable[C, A] {
    def apply(a: A) = tg.from(Tuple1(a))
    def unapply(p: C) = tg.to(p)._1
  }

  // TODO: unit

  // custom routable
  def routable[P, T](a: T => P, u: P => T) = new Routable[P, T] {
    def apply(t: T) = a(t)
    def unapply(p: P) = u(p)
  }
}

