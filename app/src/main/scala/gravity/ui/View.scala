package gravity.ui

import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn, MuiTextField}
import gravity.ClassGeneric
import japgolly.scalajs.react.{ReactComponentB, ReactComponentC, ReactNode, TopNode}
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{LiftAll, Mapper, ToTraversable, ZipConst}
import shapeless.tag.@@
import japgolly.scalajs.react.vdom.EmptyTag

import scalajs.js
import chandu0101.scalajs.react.components.Implicits._
import gravity.ui.ClassRoutes.EditPage
import japgolly.scalajs.react.extra.router.RouterCtl

import scala.reflect.ClassTag


/**
  * Typeclass for displaying a data type T as a ReactNode. T could be a primitive type or a
  * complex type (case class). You can consider the View for a case class as a "detail" page.
  *
  * Note that Views are limited to displaying the data received. There's no mechanism for
  * fetching more data from within a View. Thus, the model dictates display. To customize
  * the display of a model, there are three approaches:
  *
  * (1) The model needs to be transformed into particular types (that drive implicit
  *     selection)
  * (2) A ViewCustomizer typeclass instance allows tweaks to the generic view
  * (3) The model can declare its own typeclass instance rather than relying on generic
  *     derivation. It can still call out to the standard typeclass instances if it so
  *     desires.
  */
trait View[T] {
  // alternatively makes this return a ReactComponentC[T]?

  def view(router: RouterCtl[AnyPage], t: T): ReactNode // ReactElement?
}

trait RelaxedViewImplicits {
  implicit def defaultView[T]
  (implicit
    relax: RelaxedImplicits,
    classTag: ClassTag[T]) = new View[T] {
    override def view(router: RouterCtl[AnyPage], t: T): ReactNode =
      Seq(classTag.toString, "(no View instance found): ", t.toString)
      // TODO: mouseover describing the type
  }

}

object View extends RelaxedViewImplicits {

  implicit object StringView extends View[String] {
    override def view(router: RouterCtl[AnyPage], t: String): ReactNode = t
  }

  implicit object IntView extends View[Int] {
    override def view(router: RouterCtl[AnyPage], t: Int): ReactNode = t.toString
  }

  import js.JSConverters._

  implicit def optionView[T](implicit v: View[T]) =
    new View[Option[T]] {
      override def view(router: RouterCtl[AnyPage], t: Option[T]): ReactNode = t.map((t: T) => v.view(router, t)).getOrElse("")
    }

  import syntax._

  /**
    * View for `Seq[T]` where `T` is Generic and we're able to get a `View` instance for each of
    * `T`'s components. This allows us to display a table where each field of `T` is rendered in its own column,
    * rather than simply displaying a sequence of detail views.
    */
  implicit def viewSeqHListAsTable[L <: HList, LR <: HList, H <: HList, V <: HList, VM <: HList, M <: HList]
  (implicit
    lr: ZipConst.Aux[RouterCtl[AnyPage], L, LR],
    //liftHeaders: LiftAll.Aux[Header, TL, H],
    //headersList: ToTraversable.Aux[H, List, Header[_]],
    mapper: Mapper.Aux[viewPoly.type, LR, M],
    outList: ToTraversable.Aux[M, List, ReactNode]
  ) = new View[Seq[L]] {
    override def view(router: RouterCtl[AnyPage], ls: Seq[L]): ReactNode = {
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable(selectable = false)(
            MuiTableBody(displayRowCheckbox = false)(
              ls map { l =>
                MuiTableRow(displayBorder = false) {
                  val elems = ((l zipConst router) map viewPoly).toList
                  elems map { e =>
                    MuiTableRowColumn()(e)
                  }
                }
              }
            )
          )
        )
        .build
        .apply()
    }
  }

  implicit def viewSeqClassAsTable[T, TL <: HList]
  (implicit
    g: ClassGeneric.Aux[T, TL],
    v: View[Seq[TL]]
  ) = new View[Seq[T]] {
    override def view(router: RouterCtl[AnyPage], ts: Seq[T]): ReactNode = {
      v.view(router, ts.map(g.to(_)))
    }
  }

  implicit def viewClassTaggedField[K, V, M, C](
    implicit v: View[V],
    header: Header[FieldType[K, V] @@ C]
  ) = new View[FieldType[K, V] @@ C] {
    override def view(router: RouterCtl[AnyPage], t: @@[FieldType[K, V], C]): ReactNode = {
      v.view(router, t)
    }
  }

  // Should we actually be making this implicit be available by default?
  // For certain datatypes, we want them to be treated like a single field
  // rather than exposing its sub-fields
  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[L <: HList, LR <: HList, O <: HList, H <: HList]
  (implicit
    lr: ZipConst.Aux[RouterCtl[AnyPage], L, LR],
    liftHeaders: LiftAll.Aux[Header, L, H],
    headersToList: ToTraversable.Aux[H, List, Header[_]],
    mapper: Mapper.Aux[viewPoly.type, LR, O],
    trav: ToTraversable.Aux[O, List, ReactNode]
  ) = new View[L] {
    def view(router: RouterCtl[AnyPage], l: L): ReactNode = {
      val headers = liftHeaders.instances.toList.map(_.header)
      val elements: List[ReactNode] = ((l zipConst router) map viewPoly).toList
      val headersAndElements = headers zip elements
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable(selectable = false)(
            MuiTableBody(displayRowCheckbox = false)(
              headersAndElements.grouped(2) map { seq =>
                // Four column view: 2 columns of (field header + field value)
                MuiTableRow(displayBorder = false)(
                  (seq flatMap { case (h, e) =>
                    Seq(
                      MuiTableRowColumn(style = js.Dynamic.literal("textAlign" -> "right"))(h),
                      MuiTableRowColumn()(e)
                    )
                  }) :+ MuiTableRowColumn()()
                  // fifth empty column evens out the whitespace on the rhs with the lhs
                )
              }
            )
          )
        )
        .build
        .apply()
    }
  }

  implicit def classView[T, L <: HList]
  (implicit
    l: ClassGeneric.Aux[T, L],
    v: View[L]) = new View[T] {
    override def view(router: RouterCtl[AnyPage], t: T): ReactNode = {
      // link to edit page
      // or maybe that needs to be done at the router level
      // e.g. the Layout calls into a typeclass NavBar[Detail[T]]

      // we need the id for t
      //router.link(EditPage[T](t.))
      v.view(router, l.to(t))
    }
  }
}

object viewPoly extends Poly1 {
  implicit def view[C, K, V]
  (implicit
    view: View[FieldType[K, V] @@ C]) = at[(FieldType[K, V] @@ C, RouterCtl[AnyPage])] { case (t, router) =>
    view.view(router, t)
  }
}

