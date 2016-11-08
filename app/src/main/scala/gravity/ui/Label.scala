package gravity.ui

import shapeless.labelled._
import shapeless.{HList, RecordArgs, ops}
import shapeless.tag._

import scala.reflect.ClassTag

/**
  * For a field, `T` is of the form `FieldType[K, V] @@ C == V with KeyTag[K, V] with Tagged[C]` where C is
  * the case class from which a record is derived containing field K with value V.  This allows us to define
  * low-priority default implicits for T's based on K, V, or C alone or in some combination GIVEN THAT
  * we have contravariant View[-T] http://stackoverflow.com/questions/6682824/how-can-i-combine-the-typeclass-pattern-with-subtyping
  *
  * "Label" is too broad here.  Really, these are always labels of a noun Thing.  E.g. we should add plural label here
  *
  * @tparam T
  */
trait Label[T] {
  def label: String
}

object Label {
  // Note that V is unconstrained so effectively
  // we generate labels for any FieldType[K, _] @@ T regardless of value type V
  implicit def fromLabelsData[T, K, V, R <: HList]
  (implicit
    data: Labels[T, R],
    kInR: ops.record.Selector.Aux[R, K, String]) = new Label[FieldType[K, V] @@ T] {
    def label = kInR.apply(data.labels)
  }

  def forType[T](implicit ct: ClassTag[T]): Label[T] = new Label[T] {
    override def label = ct.runtimeClass.getSimpleName
  }

}

// Or rename to MetaMap or MetadataMap
abstract class Metadata[C, M <: HList](val map: M)

// SelectMany might be better here
// to traversable String
// TODO: how do we add the label for T itself here?
// Instead of LabelledGeneric we might want to check labels against a set including labelledgeneric plus methods
case class Labels[T, R <: HList](labels: R) extends Metadata[T, R](labels)

object Labels {
  class Curried[T] extends RecordArgs {
    def applyRecord[R <: HList](labels: R) = {
      Labels[T, R](labels)
    }

    //def apply[R <: HList](labels: R) = applyRecord(labels)
  }
  def apply[T] = new Curried[T]
}

