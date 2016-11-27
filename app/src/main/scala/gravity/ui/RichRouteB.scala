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


  implicit class RichRouteB[A <: Product](val ra: RouteB[A]) {

    class Curried[B] {
      def apply[AL <: HList, BL <: HList]
      (implicit
        ag: Generic.Aux[A, AL],
        bg: Generic.Aux[B, BL],
        eq: AL =:= BL,
        eq2: BL =:= AL
      ): RouteB[B] = {
        new RouteB[B](
          regex = ra.regex,
          matchGroups = ra.matchGroups,
          parse = ra.parse(_) map { a =>
            val al = ag.to(a)
            val bl: BL = al
            bg.from(bl)
          },
          build = { b =>
            val bl = bg.to(b)
            val al: AL = bl
            val a = ag.from(al)
            ra.build(a)
          }
        )
      }
    }

    def caseClassShapeless[B] = new Curried[B]

    def caseClassShapeless2[B]
    (implicit tg: TupleGeneric.Aux[A, B]): RouteB[B] = {
      new RouteB[B](
        regex = ra.regex,
        matchGroups = ra.matchGroups,
        parse = ra.parse(_) map { a =>
          tg.to(a)
        },
        build = { b =>
          ra.build(tg.from(b))
        }
      )
    }
  }
}
