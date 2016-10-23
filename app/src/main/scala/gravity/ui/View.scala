package gravity.ui

import cats.kernel.Monoid
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
  // alternatively makes this return a ReactComponentC[UndefOr[T]]?
  def view(t: T): ReactNode
  def emptyView: ReactNode
}

trait ComponentBasedView[T] {
  def component(t: T): ReactComponentC.DefaultProps[UndefOr[ReactNode], Unit, Unit, TopNode]

  def emptyComponent: ReactComponentC.DefaultProps[UndefOr[ReactNode], Unit, Unit, TopNode]

  def view(t: T) = component(t)()

  def emptyView = emptyComponent()
}

trait RelaxedViewImplicits {
  implicit def defaultView[T]
  (implicit
    relax: RelaxedImplicits,
    classTag: ClassTag[T]) = new View[T] {
    override def view(t: T): ReactNode =
      Seq(classTag.toString, "(no View instance found): ", t.toString)
      // TODO: mouseover describing the type
    def emptyView = Seq(classTag.toString, "(no View instance found)")
  }

}

object View extends RelaxedViewImplicits {

  implicit object StringView extends View[String] with ComponentBasedView[String] {
    override def component(t: String) = {
      //val str = t.getOrElse("")
      //tf.copy(id = rand.nextString(6), defaultValue = str)
      ReactComponentB[UndefOr[ReactNode]]("blah")
        .render(P => MuiTextField(floatingLabelText = P.props, floatingLabelFixed = true,
          defaultValue = t, underlineShow = false)())
        .build
        .withDefaultProps(js.undefined)
    }

    override def emptyComponent =
      ReactComponentB[UndefOr[ReactNode]]("blah")
        .render(P => MuiTextField(floatingLabelText = P.props, floatingLabelFixed = true,
          underlineShow = false)())
        .build
        .withDefaultProps(js.undefined)

  }

  implicit object IntView extends View[Int] with ComponentBasedView[Int] {
    override def component(t: Int) = {
      //val str = t.getOrElse("")
      //tf.copy(id = rand.nextString(6), defaultValue = str)
      val str = t.toString
      ReactComponentB[UndefOr[ReactNode]]("blah")
        .render(P => MuiTextField(floatingLabelText = P.props, defaultValue = str, disabled = true, underlineShow = false)())
        .build
        .withDefaultProps(js.undefined)
    }
    override def emptyComponent = {
      //val str = t.getOrElse("")
      //tf.copy(id = rand.nextString(6), defaultValue = str)
      ReactComponentB[UndefOr[ReactNode]]("blah")
        .render(P => MuiTextField(floatingLabelText = P.props, disabled = true, underlineShow = false)())
        .build
        .withDefaultProps(js.undefined)
    }
  }

  implicit def optionView[T](
    implicit
    v: View[T] with ComponentBasedView[T]
  )
  = new View[Option[T]] with ComponentBasedView[Option[T]] {
    override def component(t: Option[T]) = t.map(v.component).getOrElse(v.emptyComponent)
    override def emptyComponent = v.emptyComponent
  }

  implicit def optionView2[T](
    implicit
    v: View[T]
  )
  = new View[Option[T]] {
    // alternatively makes this return a ReactComponentC[UndefOr[T]]?
    override def view(t: Option[T]): ReactNode = t.map(v.view).getOrElse(v.emptyView)

    override def emptyView: ReactNode = v.emptyView
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

    override def emptyView: ReactNode =
      v match {
        case hc: ComponentBasedView[V @unchecked] =>
          //hc.component(t)(Some(header.header.asInstanceOf[UndefOr[ReactNode]]))
          hc.emptyComponent(Some(header.header.asInstanceOf[UndefOr[ReactNode]]))
        case _ =>
          val n: ReactNode = Seq(header.header, "(ctf non-cbv empty): ".asInstanceOf[ReactNode], v.emptyView)
          n
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
    trav: ToTraversable.Aux[O, List, ReactNode],
    liftAll: LiftAll.Aux[View, L, V],
    trav2: ToTraversable.Aux[V, List, View[_]]
  ) = new View[L] {
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

    def emptyView = {
      val instances: List[View[_]] = liftAll.instances.toList
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable(selectable = false)(
            MuiTableBody(displayRowCheckbox = false)(
              instances.grouped(2) map { seq =>
                MuiTableRow(displayBorder = false)(
                  seq map { e =>
                    MuiTableRowColumn()(e.emptyView)
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

    override def emptyView: ReactNode = v.emptyView
  }

  /*
  implicit def optionView[T]
  (implicit
    v: View[T]
  ) = new View[Option[T]] {
    override def view(t: Option[T]) = {
      //t.map(v.view).getOrElse("empty")
      v.view(t.getOrElse(""))
    }
  }*/

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
    v match {
      case hc: ComponentBasedView[V @unchecked] =>
        hc.component(t)(Some(header.header.asInstanceOf[UndefOr[ReactNode]]))
      case _ =>
        val n: ReactNode = Seq(header.header, ": ".asInstanceOf[ReactNode], view.view(t))
        n
    }
  }
}

object headerAndView2 extends Poly1 {

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
    v match {
      case hc: ComponentBasedView[V @unchecked] =>
        hc.emptyComponent(Some(header.header.asInstanceOf[UndefOr[ReactNode]]))
      case _ =>
        val n: ReactNode = Seq(header.header, ": ".asInstanceOf[ReactNode], view.emptyView)
        n
    }
  }
}

