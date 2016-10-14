package gravity

import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.Align
import shapeless.ops.record.{Keys, Remover}
import shapeless.syntax.singleton._

object util {

  // https://github.com/milessabin/shapeless/issues/73
  abstract class Poly1WithDefault[V](defaultValue: V) extends Poly1 {
    implicit def default[T] = at[T] { _ => defaultValue }
  }

  def zipByKey[T, G <: HList, R <: HList, O <: HList](t: T, r: R)
    (implicit generic: LabelledGeneric.Aux[T, G],
      zipByKey: ZipByKey.Aux[G, R, O]): O = {
    zipByKey.apply(generic.to(t), r)
  }

}

import shapeless.tag._

/**
  * Type class supporting mapping a higher ranked function over this `HList`.
  *
  * @author Miles Sabin
  */
trait Tagger[T, In <: HList] extends DepFn1[In] with Serializable { type Out <: HList }

object Tagger {
  def apply[T, L <: HList](implicit tagger: Tagger[T, L]): Aux[T, L, tagger.Out] = tagger

  class Curried[T] {
    def apply[L <: HList](l: L)(implicit tagger: Tagger[T, L]) = tagger.apply(l)
  }

  def apply[T] = new Curried[T]

  type Aux[T, In <: HList, Out0 <: HList] = Tagger[T, In] { type Out = Out0 }

  implicit def hnilMapper1[T]: Aux[T, HNil, HNil] =
    new Tagger[T, HNil] {
      type Out = HNil
      def apply(l : HNil): Out = HNil
    }

  implicit def hlistMapper1[T, InH, InT <: HList]
  (implicit mt : Tagger[T, InT]): Aux[T, InH :: InT, (InH @@ T) :: mt.Out] =
    new Tagger[T, InH :: InT] {
      type Out = (InH @@ T) :: mt.Out
      def apply(l : InH :: InT): Out = tag[T](l.head) :: mt(l.tail)
    }
}

trait ZipByKey[L <: HList, R <: HList] extends DepFn2[L, R] {
  type Out <: HList
}

object ZipByKey {

  type Aux[L <: HList, R <: HList, O <: HList] = ZipByKey[L, R] { type Out = O }

  implicit def hnilZip[R <: HList] = new ZipByKey[HNil, R] { type Out = HNil; override def apply(l: HNil, r: R) = HNil }

  implicit def hlistZip[K, V, T <: HList, R <: HList, RV, Remainder <: HList, TO <: HList]
  (implicit
    remover: Remover.Aux[R, K, (RV, Remainder)],
    recurse: ZipByKey.Aux[T, Remainder, TO]
  ) = new ZipByKey[FieldType[K, V] :: T, R] {
    type Out = FieldType[K, (V, RV)] :: TO

    def apply(l: FieldType[K, V] :: T, r: R): Out = {
      val (rv, remainder) = remover.apply(r)
      val newValue = (l.head, rv)
      labelled.field[K](newValue) :: recurse.apply(l.tail, remainder)
    }
  }
}

trait HasSameKeys[L <: HList, R <: HList]

object HasSameKeys {
  implicit def hlistSameKeys[L <: HList, R <: HList, KL <: HList, KR <: HList]
  (implicit kl: Keys.Aux[L, KL],
    kr: Keys.Aux[R, KR],
    align: Align[KL, KR]) = new HasSameKeys[L, R] {

  }
}

object main {
  case class Book(author: String, title: String, quantity: Int)
  val labels = ('author ->> "Id") :: ('title ->> "Name") :: ('quantity ->> "Number Of") :: HNil

  val generic = LabelledGeneric[Book]

  def main(args: Array[String]): Unit = {
    println(util.zipByKey(Book("Foo", "Bar", 3), labels))
  }
}
