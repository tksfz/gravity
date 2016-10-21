package gravity.ui

import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn, MuiTextField}
import gravity.ClassGeneric
import gravity.methods._
import gravity.util.Poly1WithDefault
import japgolly.scalajs.react.{ReactComponentB, ReactComponentC, ReactNode, TopNode}
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{Mapper, ToTraversable, ZipConst}
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

  trait ComponentBasedView[T] {
    def component(t: T): ReactComponentC.DefaultProps[UndefOr[ReactNode], Unit, Unit, TopNode]

    def view(t: T) = component(t)()
  }

  implicit object StringView extends View[String] with ComponentBasedView[String] {

    override def component(t: String) = {
      //val str = t.getOrElse("")
      //tf.copy(id = rand.nextString(6), defaultValue = str)
      ReactComponentB[UndefOr[ReactNode]]("blah")
        .render(P => MuiTextField(floatingLabelText = P.props.orElse("asdf".asInstanceOf[UndefOr[ReactNode]]), floatingLabelFixed = true,
          defaultValue = t, underlineShow = false)())
        .build
        .withDefaultProps(js.undefined)
    }
  }

  implicit object IntView extends View[Int] with ComponentBasedView[Int] {
    override def component(t: Int) = {
      //val str = t.getOrElse("")
      //tf.copy(id = rand.nextString(6), defaultValue = str)
      ReactComponentB[UndefOr[ReactNode]]("blah")
        .render(P => MuiTextField(floatingLabelText = P.props, defaultValue = t.toString, disabled = true, underlineShow = false)())
        .build
        .withDefaultProps(js.undefined)
    }
  }

  implicit def optionView[T]
  (implicit v: View[T]) = new View[Option[T]] {
    def view(t: Option[T]) = t.map(v.view(_)).getOrElse("")
  }

  implicit def viewClassTaggedField[K, V, M, C](
    implicit v: View[V],
    header: Header[FieldType[K, V] @@ C]
  ) = new View[FieldType[K, V] @@ C] {
    override def view(t: @@[FieldType[K, V], C]): ReactNode = {
      v match {
        case hc: ComponentBasedView[V @unchecked] =>
          //hc.component(t)(Some(header.header.asInstanceOf[UndefOr[ReactNode]]))
          hc.component(t)(Some("asdf".asInstanceOf[UndefOr[ReactNode]]))
        case _ =>
          val n: ReactNode = Seq(header.header, "(ctf): ".asInstanceOf[ReactNode], v.view(t))
          n
      }
    }
  }
  // Should we actually be making this implicit be available by default?
  // For certain datatypes, we want them to be treated like a single field
  // rather than exposing its sub-fields
  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[L <: HList, LL <: HList, O <: HList]
  (implicit
    mapper: Mapper.Aux[headerAndView.type, L, O],
    trav: ToTraversable.Aux[O, List, ReactNode]) = new View[L] {
    def view(l: L): ReactNode = {
      val elements: List[ReactNode] =
        (l map headerAndView).toList
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable(selectable = false)(
            MuiTableBody(displayRowCheckbox = false)(
              elements.grouped(2) map { seq =>
                MuiTableRow(displayBorder = false)(
                  seq map { e =>
                    MuiTableRowColumn()(e)
                  }
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

  object headerAndView extends Poly1 {

    /**
      * For some reason, the View[FieldType..] instance doesn't seem to be able to pick up
      * the View[V] instances. So we pick up both here, and use whatever works.
      */
    implicit def view[C, K, V]
    (implicit
      header: Header[FieldType[K, V] @@ C],
      v: View[V],
      view: View[FieldType[K, V] @@ C]) = at[FieldType[K, V] @@ C] { t =>
      v match {
        case hc: ComponentBasedView[V @unchecked] =>
          hc.component(t)(Some(header.header.asInstanceOf[UndefOr[ReactNode]]))
        case _ =>
          val n: ReactNode = Seq(header.header, ": ".asInstanceOf[ReactNode], view.view(t))
          n
      }
    }
  }

}