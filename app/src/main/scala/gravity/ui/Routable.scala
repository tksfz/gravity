package gravity.ui

import generic._

/**
  * Type class supporting linking to a page type P that accepts argument T
  *
  * @tparam P page type
  * @tparam T route arguments from which the page can be instantiated
  */
trait Linkable[+P] {
  type Repr
  def apply(t: Repr): P
}

// TODO: static pages
object Linkable {

  type Aux[Page, T] = Linkable[Page] { type Repr = T }

  // routable => linkable
  implicit def linkableFromRoutable[C <: Product]
  (implicit routable: Routable[C]) = new Linkable[C] {
    type Repr = routable.Repr
    override def apply(t: Repr): C = routable.apply(t)
  }

  implicit def some[P, T]
  (implicit linkable: Linkable[P]): Option[Linkable[P]] = Some(linkable)
}

trait Pathable[-P] {
  type Repr
  def unapply(p: P): Repr
}

/**
  * Type class supporting injection of app page types into libraries that use those pages (either
  * generating page contents or linking to them)
  *
  * @tparam P page type
  * @tparam T route arguments from which the page can be instantiated or destructured to
  */
trait Routable[P] extends Linkable[P] with Pathable[P]

object Routable {

  type Aux[Page, T] = Routable[Page] { type Repr = T }

  def apply[P : Routable] = implicitly[Routable[P]]

  /**
    * @tparam C a case class
    * @tparam T a tuple type
    */
  implicit def routableTupleCaseClass[C <: Product]
  (implicit tg: TupleGeneric[C]) = new Routable[C] {
    type Repr = tg.Repr
    def apply(t: Repr) = tg.from(t)
    def unapply(p: C) = tg.to(p)
  }

  /**
    * @tparam C a case class
    * @tparam A a non-product type (not a tuple)
    */
  implicit def routableTuple1CaseClass[C <: Product, A]
  (implicit tg: TupleGeneric.Aux[C, Tuple1[A]]) = new Routable[C] {
    type Repr = A
    def apply(a: Repr) = tg.from(Tuple1(a))
    def unapply(p: C) = tg.to(p)._1
  }

  // TODO: unit

  // custom routable
  def routable[P, T](a: T => P, u: P => T) = new Routable[P] {
    type Repr = T
    def apply(t: T) = a(t)
    def unapply(p: P) = u(p)
  }
}

