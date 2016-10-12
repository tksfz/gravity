package gravity.ui

import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn}
import gravity.methods._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{Mapper, ToTraversable, ZipConst}
import shapeless.tag.@@

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

object Label {
  // Note that V is unconstrained
  implicit def fromLabelsData[T, K, V, R <: HList]
  (implicit
    data: Labels[T, R],
    kInR: ops.record.Selector.Aux[R, K, String]) = new Label[FieldType[K, V] @@ T] {
    def label = kInR.apply(data.labels)
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



// come up with a better name than Label
// FieldHeader or FieldLabel? header is fine actually.  it doesn't have to be just a field header
// e.g. it could be a tab header
// this should possibly just be pushed to the View typeclass
// primitives don't have obvious headers though:  String, Int, etc.  only fields and more complex objects do?
trait Header[T] {
  def header: ReactNode
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

  implicit def headerFromLabel[T](implicit label: Label[T]) = new Header[T] {
    override def header: ReactNode = label.label
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