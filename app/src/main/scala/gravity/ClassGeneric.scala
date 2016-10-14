package gravity

import gravity.ui.Edit
import shapeless._

/**
  * Same as LabelledGeneric[T] but tags all fields with the type T, indicating that the
  * fields originated from T.
  */
trait ClassGeneric[T] {
  type Repr
  def to(t : T) : Repr
  //def from(r : Repr) : T
}

object ClassGeneric {
  type Aux[T, TL <: HList] = ClassGeneric[T] { type Repr = TL }

  implicit def instance[T, L <: HList, TL <: HList, E <: HList](
    implicit l: LabelledGeneric.Aux[T, L],
    tagger: Tagger.Aux[T, L, TL]
  ) = new ClassGeneric[T] {
    override type Repr = TL

    def to(t: T) = tagger(l.to(t))
    //def from(r: Repr) = ??? // requires untagger
  }
}