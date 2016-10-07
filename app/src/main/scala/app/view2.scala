package app

import app.HasSameKeys
import app.models.Account
import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn}
import japgolly.scalajs.react.{ReactComponentB, ReactElement, ReactNode}
import shapeless.PolyDefns.~>
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{LiftAll, Mapped, Mapper, ToTraversable, ZipConst}
import shapeless.syntax.singleton._
import shapeless.tag.@@

object view2 {

  /**
    * Some questions to think about:
    * How do we handle, say at the data model level, foreign key lookups and displaying the name of the reference?
    *   (1) One[T] can resolve to a name - how do we represent that?
    *   (2) do we have some kind of name_denorm system?
    *   (3) How does the View reach back to the query and say:  I want this data?
    *
    *   The answer to (3) is it doesn't.  The data drives the view, not the other way around.
    *   Maybe as a convenience there's a consolidated way to derive a Query from a View.
    *
    *   For (1) we need a sophistcated system of "model derivation" or "model transformation":
    *
    *   Starting with one model, transform it into another by adding fields, changing data types, etc.
    * @tparam T
    */
  trait View[-T] {
    def view(t: T): ReactNode
    // def header?
    // def asString
    // def toReactElement
    // def inTable etc.
    // def withField
    //def edit[P, S, B, N](t: T): ReactComponentU[P, S, B, N]
  }

  /*
trait Renderable {
def toString: String // for use in attributes
def toReactElement: ReactElement
// also could have options in lists
// detail views, list views
}
 */

  // TODO: Detail[T]?
  // Actually for a case class C, View[C] is the detail view
  // while View[FieldType[?, C] @@ D] is the field view
  // well we should have a foreign key type e.g. One[C] anyway

  /**
    * For a field, `T` is of the form `FieldType[K, V] @@ C == V with KeyTag[K, V] with Tagged[C]` where C is
    * the case class from which a record is derived containing field K with value V.  This allows us to define
    * low-priority default implicits for T's based on K, V, or C alone or in some combination GIVEN THAT
    * we have contravariant View[-T] http://stackoverflow.com/questions/6682824/how-can-i-combine-the-typeclass-pattern-with-subtyping
    *
    * "Label" is too broad here.  Really, these are always labels of a noun Thing.  E.g. we should add plural label here
    * @tparam T
    */
  trait Label[T] {
    def label: String
  }

  // come up with a better name than Label
  // FieldHeader or FieldLabel? header is fine actually.  it doesn't have to be just a field header
  // e.g. it could be a tab header
  trait Header[T] {
    def header: ReactNode
  }

  implicit object StringView extends View[String] {
    override def view(t: String): ReactNode = t
  }

  implicit object IntView extends View[Int] {
    def view(n: Int) = n.toString
  }

  implicit def headerFromLabel[T](implicit label: Label[T]) = new Header[T] {
    override def header: ReactNode = label.label
  }

  // Or rename to MetaMap or MetadataMap
  abstract class Metadata[C, M <: HList](val map: M)

  // SelectMany might be better here
  // to traversable String
  // TODO: how do we add the label for T itself here?
  case class Labels[T, L <: HList, R <: HList](labels: R)
    (implicit
      l: LabelledGeneric.Aux[T, L],
      hasSameKeys: HasSameKeys[L, R]) extends Metadata[T, R](labels)


  object Labels {
    class Curried[T] {
      def apply[L <: HList, R <: HList](labels: R)
        (implicit
          l: LabelledGeneric.Aux[T, L],
          hasSameKeys: HasSameKeys[L, R]) = {
        Labels[T, L, R](labels)
      }
    }
    def apply[T] = new Curried[T]
  }

  implicit val mylabelsData = Labels[Account](
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") ::
      HNil
  )

  implicit def fromLabelsData[T, L <: HList, F, V, R <: HList]
  (implicit
    l: LabelledGeneric.Aux[T, L],
    fInL: ops.record.Selector.Aux[L, F, V],
    data: Labels[T, L, R],
    fInRToo: ops.record.Selector.Aux[R, F, String]) = new Label[FieldType[F, V] @@ T] {
    def label = fInRToo.apply(data.labels)
  }

  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[T, L <: HList, TL <: HList, O <: HList]
  (implicit
    l: LabelledGeneric.Aux[T, L],
    tagger: Tagger.Aux[T, L, TL],
    mapper: Mapper.Aux[headerAndView.type, TL, O],
    trav: ToTraversable.Aux[O, List, (ReactNode, ReactNode)]) = new View[T] {
    def view(t: T): ReactNode = {
      val fieldElems: List[(ReactNode, ReactNode)] =
        (tagger.apply(l.to(t)) map headerAndView).toList
      val mui =
        ReactComponentB[Unit]("blah")
          .render(_ =>
            MuiTable()(
              MuiTableBody(displayRowCheckbox = false)(
                fieldElems map { case (l, r) =>
                  MuiTableRow()(
                    MuiTableRowColumn()(l),
                    MuiTableRowColumn()(r)
                  )
                }
              )
            )
          )
          .build
      mui()
    }
  }

  // TODO: add a default so some fields can be skipped.  use Poly1WithDefault
  object headerAndView extends Poly1 {
    implicit def headerAndView[T]
    (implicit
      header: Header[T], view: View[T]) = at[T] { t =>
      (header.header, view.view(t))
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

}