package gravity.ui

import generic._
import shapeless.{$up => _, _}

trait Routable[T, P] {
  def apply(t: T): P
  def unapply(p: P): T
}

object Routable {
  // FIXME: this is backwards
  implicit def routableTupleCaseClass[T <: Product, P]
  (implicit tg: TupleGeneric.Aux[T, P]) = new Routable[T, P] {
    def apply(t: T) = tg.to(t)
    def unapply(p: P) = tg.from(p)
  }

  // tuple1
  implicit def routableTuple1CaseClass[A, P <: Product]
  (implicit tg: TupleGeneric.Aux[P, Tuple1[A]]) = new Routable[A, P] {
    def apply(a: A) = tg.from(Tuple1(a))
    def unapply(p: P) = tg.to(p)._1
  }

  // TODO: unit

  // custom routable
  def routable[T, P](a: T => P, u: P => T) = new Routable[T, P] {
    def apply(t: T) = a(t)
    def unapply(p: P) = u(p)
  }
}

