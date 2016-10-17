package gravity.ui

import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn}
import gravity.methods._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{Mapper, ToTraversable, ZipConst}
import shapeless.tag.@@

// come up with a better name than Label
// FieldHeader or FieldLabel? header is fine actually.  it doesn't have to be just a field header
// e.g. it could be a tab header
// this should possibly just be pushed to the View typeclass
// primitives don't have obvious headers though:  String, Int, etc.  only fields and more complex objects do?
trait Header[-T] {
  def header: ReactNode
}

object Header {
  implicit def headerFromLabel[T](implicit label: Label[T]) = new Header[T] {
    override def header: ReactNode = label.label
  }

  implicit def header[K <: Symbol, V]
  (implicit
    relax: RelaxedImplicits,
    w: Witness.Aux[K]) = new Header[KeyTag[K, V]] {
    def header = w.value.name
  }
}

/**
  * Some questions to think about:
  * How do we handle, say at the data model level, foreign key lookups and displaying the name of the reference?
  *   (1) One[T] can resolve to a name - how do we represent that?  Transformations over the record representation.
  *   (2) do we have some kind of name_denorm system?               Maybe.
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

object View {

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

  implicit object StringView extends View[String] {
    override def view(t: String): ReactNode = t
  }

  implicit object IntView extends View[Int] {
    def view(n: Int) = n.toString
  }

  // Should we actually be making this implicit be available by default?
  // For certain datatypes, we want them to be treated like a single field
  // rather than exposing its sub-fields
  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[L <: HList, LL <: HList, O <: HList]
  (implicit
    zippy: ZipConst.Aux[L, L, LL],
    mapper: Mapper.Aux[accessThenView.type, LL, O],
    //mapper: Mapper.Aux[headerAndView.type, L, O]
    trav: ToTraversable.Aux[O, List, (ReactNode, ReactNode)]) = new View[L] {
    def view(l: L): ReactNode = {
      val fieldElems: List[(ReactNode, ReactNode)] =
        ((l zipConst l) map accessThenView).toList
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

  object accessThenView extends Poly1 {

    // Note that T and V are free
    implicit def accessThenView[T, L <: HList, K, V, R](
      implicit
      access: Access.Aux[L, K, R],
      header: Header[FieldType[K, R] @@ T],
      v: View[FieldType[K, R] @@ T]
    ) = at[(FieldType[K, V] @@ T, L)] {
      case (fieldValue, l) =>
        val f = tag[T](field[K](access(l)))
        (header.header, v.view(f))
    }
  }

}