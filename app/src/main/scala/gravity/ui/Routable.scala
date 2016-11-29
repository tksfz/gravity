package gravity.ui

import generic._
import shapeless.HList
import shapeless.ops.hlist.{LiftAll, Selector, SubtypeUnifier}

/**
  * Type class supporting linking to a page type P that accepts argument T
  *
  * @tparam P page type
  * @tparam T route arguments from which the page can be instantiated
  */
trait Linkable[+P, -T] {
  def apply(t: T): P
}

// TODO: static pages
object Linkable {

  type Lamb[S] = { type Da[Q] = Linkable[Q, S] }

  // liftall implicit
  case class Links[L <: HList]()

  // routable => linkable
  implicit def linkableFromRoutable[C <: Product, T]
  (implicit routable: Routable[C, T]) = new Linkable[C, T] {
    override def apply(t: T): C = routable.apply(t)
  }

  implicit def linkableFromLinks2[Sup, T, L <: HList, I <: HList, J <: HList]
  (implicit
    links: Links[L],
    liftAll: LiftAll.Aux[Lamb[T]#Da, L, I], // this is wrong: we want LiftSome
    unify: SubtypeUnifier.Aux[I, Linkable[Sup, T], J],
    select: Selector[J, Linkable[Sup, T]]
  ) = new Linkable[Sup, T] {
    override def apply(t: T): Sup = select(unify(liftAll.instances)).apply(t)
  }

  implicit def some[P, T]
  (implicit linkable: Linkable[P, T]): Option[Linkable[P, T]] = Some(linkable)
}

trait Pathable[-P, +T] {
  def unapply(p: P): T
}

/**
  * Type class supporting injection of app page types into libraries that use those pages (either
  * generating page contents or linking to them)
  *
  * @tparam P page type
  * @tparam T route arguments from which the page can be instantiated or destructured to
  */
trait Routable[P, T] extends Linkable[P, T] with Pathable[P, T]

object Routable {

  class Curried[P] {
    def apply[T]
    (implicit routable: Routable[P, T]) = routable
  }

  def apply[P] = new Curried[P]

  /**
    * @tparam C a case class
    * @tparam T a tuple type
    */
  implicit def routableTupleCaseClass[C <: Product, T <: Product]
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

