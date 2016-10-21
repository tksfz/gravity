package gravity.ui

import japgolly.scalajs.react.ReactNode
import shapeless.Witness
import shapeless.labelled._
import shapeless.tag._

// come up with a better name than Label
// FieldHeader or FieldLabel? header is fine actually.  it doesn't have to be just a field header
// e.g. it could be a tab header
// this should possibly just be pushed to the View typeclass
// primitives don't have obvious headers though:  String, Int, etc.  only fields and more complex objects do?
trait Header[T] {
  def header: ReactNode
}

trait RelaxedHeaderImplicits {
  implicit def header[K <: Symbol, V, C]
  (implicit
    relax: RelaxedImplicits,
    w: Witness.Aux[K]) = new Header[FieldType[K, V] @@ C] {
    def header = w.value.name.capitalize
  }
}

object Header extends RelaxedHeaderImplicits {
  implicit def headerFromLabel[T](implicit label: Label[T]) = new Header[T] {
    override def header: ReactNode = label.label
  }

  implicit def fieldHeaderFromLabel[K, V, C](implicit label: Label[FieldType[K, V] @@ C]) = new Header[FieldType[K, V] @@ C] {
    override def header: ReactNode = label.label
  }
}

