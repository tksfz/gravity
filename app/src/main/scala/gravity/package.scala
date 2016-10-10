import shapeless.{::, HList}
import shapeless.labelled._
import shapeless.ops.record.Selector
import shapeless.tag._

/**
  * Created by thom on 10/10/16.
  */
package object gravity {

  /**
    * Selector instances for fields (FieldType[K, V]) tagged with the classes they originally
    * came from (@@ H).
    */
  implicit def select[K, V, H, T <: HList]: Selector.Aux[@@[FieldType[K, V], H] :: T, K, V] =
    new Selector[@@[FieldType[K, V], H] :: T, K] {
      type Out = V
      def apply(l : @@[FieldType[K, V], H] :: T) = l.head
    }

  implicit def recurse[H, T <: HList, U, V]
  (implicit st : Selector.Aux[T, U, V]): Selector.Aux[H :: T, U, V] =
    new Selector[H :: T, U] {
      type Out = V
      def apply(l : H :: T) = st(l.tail)
    }

}
