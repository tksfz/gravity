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


  /**
    * Typeclass witnessing that all the elements of an HList have instances of the given typeclass.
    * Courtesy of mpilquist.
    *
    * @author Tin Pavlinic
    */
  sealed trait LiftAll2[F[_, _], In <: HList] {
    type Out <: HList
    def instances: Out
  }

  object LiftAll2 {
    type Aux[F[_, _], In0 <: HList, Out0 <: HList] = LiftAll2[F, In0] {type Out = Out0}
    class Curried[F[_, _]] {def apply[In <: HList](in: In)(implicit ev: LiftAll2[F, In]) = ev}

    def apply[F[_, _]] = new Curried[F]
    def apply[F[_, _], In <: HList](implicit ev: LiftAll2[F, In]) = ev

    implicit def hnil[F[_, _]]: LiftAll2.Aux[F, HNil, HNil] = new LiftAll2[F, HNil] {
      type Out = HNil
      def instances = HNil
    }

    implicit def hcons[F[_, _], X, H, T <: HList, TI <: HList]
    (implicit headInstance: F[H, X], tailInstances: Aux[F, T, TI]): Aux[F, H :: T, F[H, X] :: TI] =
      new LiftAll2[F, H :: T] {
        type Out = F[H, X] :: TI
        def instances = headInstance :: tailInstances.instances
      }
  }

  /**
    * Typeclass witnessing that all the elements of an HList have instances of the given typeclass.
    * Courtesy of mpilquist.
    *
    * @author Tin Pavlinic
    */
  sealed trait LiftSome[F[_], In <: HList] {
    type Out <: HList
    def instances: Out
  }

  object LiftSome {
    type Aux[F[_], In0 <: HList, Out0 <: HList] = LiftSome[F, In0] {type Out = Out0}
    class Curried[F[_]] {def apply[In <: HList](in: In)(implicit ev: LiftSome[F, In]) = ev}

    def apply[F[_]] = new Curried[F]
    def apply[F[_], In <: HList](implicit ev: LiftSome[F, In]) = ev

    implicit def hnil[F[_]]: LiftSome.Aux[F, HNil, HNil] = new LiftSome[F, HNil] {
      type Out = HNil
      def instances = HNil
    }

    implicit def hcons[F[_], H, T <: HList, TI <: HList]
    (implicit headInstance: F[H], tailInstances: Aux[F, T, TI]): Aux[F, H :: T, F[H] :: TI] =
      new LiftSome[F, H :: T] {
        type Out = F[H] :: TI
        def instances = headInstance :: tailInstances.instances
      }

    // I really doubt this works
    implicit def hconsNegative[F[_], H, T <: HList, TI <: HList]
    (implicit tailInstances: Aux[F, T, TI]): Aux[F, H :: T, TI] =
      new LiftSome[F, H :: T] {
        type Out = TI
        def instances = tailInstances.instances
      }

  }



}
