package gravity.ui

import japgolly.scalajs.react.extra.router.StaticDsl.RouteB
import shapeless._
import shapeless.ops.hlist.Tupler

object generic {

  // https://gist.github.com/milessabin/fbd9da3361611b91da17
  trait TupleGeneric[C <: Product] extends Serializable {
    type Repr <: Product

    def to(t : C) : Repr

    def from(r : Repr) : C
  }

  object TupleGeneric {
    type Aux[C <: Product, R] = TupleGeneric[C] { type Repr = R }

    def apply[C <: Product](implicit tgc: TupleGeneric[C]): Aux[C, tgc.Repr] = tgc

    implicit def mkTG[C <: Product, L <: HList, R <: Product]
    (implicit cGen: Generic.Aux[C, L], tup: Tupler.Aux[L, R], tGen: Generic.Aux[R, L]): Aux[C, R] =
      new TupleGeneric[C] {
        type Repr = R

        def to(t : C) : Repr = cGen.to(t).tupled

        def from(r : Repr) : C = cGen.from(tGen.to(r))
      }
  }

}
