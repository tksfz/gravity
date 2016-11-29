package gravity.ui

import generic._
import shapeless.Witness

/**
  * Type class supporting linking to a page type P that accepts argument T
  *
  * @tparam P page type
  */
trait Linkable[+P] {
  /** route arguments from which the page can be instantiated */
  type Args
  def apply(t: Args): P
}

// TODO: static pages
object Linkable {

  type Aux[Page, T] = Linkable[Page] { type Args = T }

  // routable => linkable
  implicit def linkableFromRoutable[C <: Product]
  (implicit routable: Routable[C]) = new Linkable[C] {
    type Args = routable.Args
    override def apply(t: Args): C = routable.apply(t)
  }

  implicit def some[P, T]
  (implicit linkable: Linkable[P]): Option[Linkable[P]] = Some(linkable)
}

trait Pathable[-P] {
  /** route arguments from which the page can be instantiated or destructured to */
  type Args
  def unapply(p: P): Args
}

/**
  * Type class supporting injection of app page types into libraries that use those pages (either
  * generating page contents or linking to them)
  *
  * @tparam P page type
  */
trait Routable[P] extends Linkable[P] with Pathable[P]

object Routable {

  type Aux[Page, T] = Routable[Page] { type Args = T }

  /**
    * Helper to inject Linkable instances. In app code:
    *     caes class MyPage(..) extends SomeInjectableTrait
    *     implicit val _ = Routable[MyPage]
    */
  def apply[P : Routable] = implicitly[Routable[P]]

  /**
    * @tparam C a case class
    */
  implicit def routableTupleCaseClass[C <: Product]
  (implicit tg: TupleGeneric[C]) = new Routable[C] {
    override type Args = tg.Repr
    def apply(t: Args) = tg.from(t)
    def unapply(p: C) = tg.to(p)
  }

  /**
    * @tparam C a case class
    * @tparam A a non-product type (not a tuple)
    */
  implicit def routableTuple1CaseClass[C <: Product, A]
  (implicit tg: TupleGeneric.Aux[C, Tuple1[A]]) = new Routable[C] {
    override type Args = A
    def apply(a: Args) = tg.from(Tuple1(a))
    def unapply(p: C) = tg.to(p)._1
  }

  implicit def routableSingleton[S]
  (implicit isSingleton: Witness.Aux[S]) = new Routable[S] {
    override type Args = Unit
    def apply(a: Unit) = isSingleton.value
    def unapply(p: S) = ()
  }

  // TODO: unit

  // custom routable
  def routable[P, T](a: T => P, u: P => T) = new Routable[P] {
    override type Args = T
    def apply(t: T) = a(t)
    def unapply(p: P) = u(p)
  }
}

