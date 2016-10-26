package gravity.ui

import japgolly.scalajs.react.ReactNode
import shapeless.Witness
import shapeless.labelled._
import shapeless.tag._

/**
  * Typeclass provides a ReactNode identifying the data type T. Default implementation
  * is simply based on the Label instance for T.
  */
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
