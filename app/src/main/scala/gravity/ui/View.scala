package gravity.ui

import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn, MuiTextField}
import gravity.ClassGeneric
import gravity.methods._
import gravity.util.Poly1WithDefault
import japgolly.scalajs.react.{ReactComponentB, ReactComponentC, ReactNode, TopNode}
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{LiftAll, Mapper, ToTraversable, ZipConst}
import shapeless.ops.record.Selector
import shapeless.tag.@@
import japgolly.scalajs.react.vdom.EmptyTag

import scalajs.js
import chandu0101.scalajs.react.components.Implicits._
import japgolly.scalajs.react.ReactComponentC.DefaultProps

import scala.reflect.ClassTag
import scala.scalajs.js.UndefOr


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

  def view(t: T): ReactNode
}

trait RelaxedViewImplicits {
  implicit def defaultView[T]
  (implicit
    relax: RelaxedImplicits,
    classTag: ClassTag[T]) = new View[T] {
    override def view(t: T): ReactNode =
      Seq(classTag.toString, "(no View instance found): ", t.toString)
      // TODO: mouseover describing the type
  }

}

object View extends RelaxedViewImplicits {

  implicit object StringView extends View[String] {
    override def view(t: String): ReactNode = t
  }

  implicit object IntView extends View[Int] {
    override def view(t: Int): ReactNode = t.toString
  }

  import js.JSConverters._

  implicit def optionView[T](implicit v: View[T]) =
    new View[Option[T]] {
      override def view(t: Option[T]): ReactNode = t.map(v.view).getOrElse("")
    }

  implicit def viewClassTaggedField[K, V, M, C](
    implicit v: View[V],
    header: Header[FieldType[K, V] @@ C]
  ) = new View[FieldType[K, V] @@ C] {
    override def view(t: @@[FieldType[K, V], C]): ReactNode = {
      v.view(t)
    }
  }

  // Should we actually be making this implicit be available by default?
  // For certain datatypes, we want them to be treated like a single field
  // rather than exposing its sub-fields
  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[L <: HList, O <: HList, V <: HList]
  (implicit
    mapper: Mapper.Aux[headerAndView.type, L, O],
    trav: ToTraversable.Aux[O, List, (ReactNode, ReactNode)],
    liftAll: LiftAll.Aux[View, L, V],
    trav2: ToTraversable.Aux[V, List, View[_]]
  ) = new View[L] {
    def view(l: L): ReactNode = {
      val elements: List[(ReactNode, ReactNode)] = (l map headerAndView).toList
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable(selectable = false)(
            MuiTableBody(displayRowCheckbox = false)(
              elements.grouped(2) map { seq =>
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
    override def view(t: T): ReactNode = v.view(l.to(t))
  }
}

object headerAndView extends Poly1 {

  import View._
  /**
    * For some reason, the View[FieldType..] instance doesn't seem to be able to pick up
    * the View[V] instances. So we pick up both here, and use whatever works.
    */
  implicit def view[C, K, V]
  (implicit
    header: Header[FieldType[K, V] @@ C],
    v: View[V],
    view: View[FieldType[K, V] @@ C]) = at[FieldType[K, V] @@ C] { t =>
      (header.header, view.view(t))
    }
}
