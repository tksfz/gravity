package gravity.ui

import chandu0101.scalajs.react.components.materialui.MuiTextField
import chandu0101.scalajs.react.components.Implicits._
import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless._

/**
  * Created by thom on 10/10/16.
  */
trait Edit[-T] {
  type Model

  // TODO: create a component that takes the Model as a parameter
  def toModel(t: T): Model
  def empty: Model
  def element(t: Model): ReactNode
}

object Edit {
  // These are generaally client-side models since server-side is typically working
  // with data models only
  // although these do get serialized to the server-side sometimes

  trait Duplicate[T]

  trait Input[T]

  // TODO: if it's already optional then just leave it as-is

  implicit object EditString extends Edit[String] {
    type Model = Option[String]
    def toModel(t: String) = Some(t)
    def empty = None
    def element(t: Option[String]) = ReactComponentB[Unit]("blah")
      .render(P => {
        val str = t.getOrElse("")
        MuiTextField(hintText = "Hint text", defaultValue = str)()
      })
      .build()
  }

  implicit object EditInt extends Edit[Int] {
    type Model = Option[Int]
    def toModel(t: Int) = Some(t)
    def empty = None
    def element(t: Option[Int]) = MuiTextField()()
  }

  // todo headers
  // TODO version of LabelledGeneric that class-tags the fields
  // how do we make certain fields not editable?
  implicit def classEdit[T, L <: HList, E <: HList](
    implicit
    l: LabelledGeneric.Aux[T, L],
    e: Edit.Aux[L, E]
  ) = new Edit[T] {
    type Model = E

    def toModel(t: T) = e.toModel(l.to(t))

    // TODO: create a component that takes the Model as a parameter
    override def empty: E = e.empty

    override def element(t: E): ReactNode = e.element(t)
  }

  type Aux[T, M] = Edit[T] { type Model = M }

  implicit def hlistEdit[L <: HList, M <: HList](
    implicit
    edit2: Edit2.Aux[L, M]
  ) = new Edit[L] {
    override type Model = M

    def toModel(t: L) = edit2.toModel(t)

    // TODO: create a component that takes the Model as a parameter
    override def empty: M = edit2.empty

    override def element(t: M) = {
      edit2.elements(t)
    }
  }

  // I can't come up with a better way to do this
  // but we can certainly keep trying
  trait Edit2[T] {
    type Model

    def toModel(t: T): Model

    // TODO: create a component that takes the Model as a parameter
    def empty: Model
    def elements(t: Model): Seq[ReactNode]
  }

  // recursive style
  object Edit2 {
    type Aux[T, M] = Edit2[T] { type Model = M }

    implicit def editHNil = new Edit2[HNil] {
      type Model = HNil
      def toModel(t: HNil) = t
      def empty = HNil
      def elements(t: HNil) = Nil
    }

    // FieldType vs non-FieldType

    implicit def editField[H, T <: HList, TM <: HList](
      implicit hedit: Edit[H],
      tedit: Edit2.Aux[T, TM]
    ) = new Edit2[H :: T] {
      type Model = hedit.Model :: TM

      def toModel(t: H :: T) = hedit.toModel(t.head) :: tedit.toModel(t.tail)

      //def element()
      def empty = hedit.empty :: tedit.empty

      def elements(t: hedit.Model :: TM) = hedit.element(t.head) +: tedit.elements(t.tail)
    }
  }

  //import shapeless.ops.record._

  //val editContactModel = Edit[Contact]//.derived
  //val editContactModel = Derive[Contact].apply.apply(_.map(wrapper))

  //editContactModel.create()

  trait Create[T]

}
