package gravity.ui

import java.time.{LocalDate}
import java.util.Date

import chandu0101.scalajs.react.components.materialui.{MuiDatePicker, MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn, MuiTextField}
import chandu0101.scalajs.react.components.Implicits._
import gravity.ClassGeneric
import gravity.models.{One, OneId, Phone}
import japgolly.scalajs.react.ReactComponentC.ReqProps
import japgolly.scalajs.react.{ReactComponentB, ReactComponentC, ReactNode, TopNode}
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless._
import shapeless.labelled.FieldType
import shapeless.tag.@@

import scala.reflect.ClassTag
import scala.util.Random

/**
  * Typeclass for visually editing a value
  *
  * We may not be able to keep contravariance here if/when we actually output the
  * new model.  But we should be able to recover it with an appropriate typeclass adapter.
  */
trait Edit[T] {
  type Model

  // TODO: create a component that takes the Model as a parameter
  def toModel(t: T): Model
  def empty: Model

  def element(t: Model): ReactNode
}

trait ComponentBasedEdit[T] extends Edit[T] {
  /**
    * If the editor for this field consists of a ReactComponent, then expose
    * that component so that it can be customized by the editOption typeclass instance.
    * MuiTextField is used as the props here only as a shortcut way of
    * exposing all the options we might want to customize.
    */
  def component(t: Model): ReactComponentC.DefaultProps[MuiTextField, Unit, Unit, TopNode]

  def element(t: Model): ReactNode = component(t)()

}

trait RelaxedEditImplicits {
  implicit def missingEdit[T]
  (implicit
    relax: RelaxedImplicits,
    classTag: ClassTag[T]) = new Edit[T] {
    override type Model = Unit //TODO: Either[Unit, T] would allow us to use T.toString
    override def toModel(t: T) = ()
    override def empty = ()
    override def element(t: Unit): ReactNode = classTag.toString
  }

}

object Edit extends RelaxedEditImplicits {

  // TODO: figure out a better way to set id's on TextField's
  val rand = new Random

  type Aux[T, M] = Edit[T] { type Model = M }
  
  // These are generally client-side models since server-side is typically working
  // with data models only
  // although these do get serialized to the server-side sometimes

  trait Duplicate[T]

  trait Input[T]

  trait EditInput {
    type Model
    // TODO: instead of exposing textField like this
    // we should be using props/state to allow passing in the header
    def textField(t: Option[String]) = {
      // Having Props = MuiTextField is just a shortcut to allow whoever
      // invokes this component to pass in whatever text field options they like
      ReactComponentB[MuiTextField]("blah")
        .render { P =>
          val str = t.getOrElse("")
          P.props.copy(id = rand.nextString(6), defaultValue = str)()
        }
        .build
        .withDefaultProps(MuiTextField())
    }
  }

  implicit object EditString extends ComponentBasedEdit[String] with EditInput {
    type Model = Option[String]
    def toModel(t: String) = Some(t)
    def empty = None

    def component(t: Option[String]) = {
      textField(t)
    }

  }

  implicit object EditInt extends ComponentBasedEdit[Int] with EditInput {
    type Model = Option[Int] // should everything actually be Option[String]?
    def toModel(t: Int) = Some(t)
    def empty = None

    override def component(t: Option[Int]) = {
      textField(t.map(_.toString))
    }
  }

  implicit object EditLocalDate extends Edit[LocalDate] {
    type Model = Option[LocalDate]
    def toModel(t: LocalDate) = Some(t)
    def empty = None
    def element(t: Option[LocalDate]) =
      ReactComponentB[Unit]("blah")
        .render(P => MuiDatePicker()())
        .build()
  }

  implicit object EditDateTime extends Edit[Date] {
    type Model = Option[Date]
    def toModel(t: Date) = Some(t)
    def empty = None
    def element(t: Option[Date]) =
      ReactComponentB[Unit]("blah")
        .render(P => MuiDatePicker()())
        .build()
  }

  implicit def editOneReference[T] = new ComponentBasedEdit[One[T]] with EditInput {
    override type Model = Option[String]

    // TODO: create a component that takes the Model as a parameter
    override def toModel(t: One[T]) = t match {
      case OneId(id) => Some(id.toString)
    }

    override def empty = None

    /**
      * If the editor for this field consists of a ReactComponent, then expose
      * that component so that it can be customized by the editOption typeclass instance.
      * MuiTextField is used as the props here only as a shortcut way of
      * exposing all the options we might want to customize.
      */
    override def component(t: Option[String]) =
      textField(t.map(_.toString))

  }

  implicit def editOptionBasic[T](implicit edit: Edit.Aux[T, Option[T]]) = new Edit[Option[T]] {
    override type Model = Option[T]
    override def toModel(t: Option[T]) = t
    override def empty = None
    override def element(t: Option[T]) = edit.element(t)
  }

  /**
    * Edits Option[T] where T is a "basic" type, having an edit model of Option[T]. In that
    * case, we don't want to produce a model of type Option[Option[T]], we just want to take
    * the Option[T] edit model as-is.
    */
  implicit def editComponentBasedOptionBasic[T]
  (implicit edit: Edit.Aux[T, Option[T]] with ComponentBasedEdit[T]) = new ComponentBasedEdit[Option[T]] {
    override type Model = Option[T]
    override def toModel(t: Option[T]) = t
    override def empty = None
    override def component(t: Option[T]) = edit.component(t)
    override def element(t: Option[T]) = edit.element(t)
  }

  /**
    * Edits Option[T] where the edit model for T is not itself an Option, for example T
    * a case class.  Those don't use Option[T] as a model, instead they use HList's of
    * Options as their models.
    */
  implicit def editOptionComplex[T, M](implicit edit: Edit.Aux[T, M]) = new Edit[Option[T]] {
    override type Model = Option[M]
    override def toModel(ot: Option[T]) = ot.map(t => edit.toModel(t))
    override def empty = None
    override def element(om: Option[M]) = edit.element(om.getOrElse(edit.empty))
  }

  import shapeless.labelled._

  implicit def editClassTaggedField[K, V, M, C](
    implicit edit: Edit.Aux[V, M],
    header: Header[FieldType[K, V] @@ C]
  ) = new Edit[FieldType[K, V] @@ C] {
    override type Model = FieldType[K, M] @@ C

    // TODO: create a component that takes the Model as a parameter
    override def toModel(t: FieldType[K, V] @@ C): FieldType[K, edit.Model] @@ C = {
      tag[C](field[K](edit.toModel(t)))
    }

    override def empty: FieldType[K, edit.Model] @@ C = tag[C](field[K](edit.empty))

    //override def component(t: @@[FieldType[K, M], C]) = edit.component(t)

    override def element(t: Model): ReactNode = {
      //edit.component(t)
        //.map(_.apply(MuiTextField(floatingLabelText = header.header)))
        //.getOrElse {
      Seq(header.header, ": ".asInstanceOf[ReactNode], edit.element(t))
        //}
    }
  }

  // how do we make certain fields not editable?
  // we might consider making this not implicit and forcing model classes
  // to declare explicit views etc.
  // they can always have a choice of implementations
  // one thing that does is it allows the compiler to show an error
  // at the time the implicit is explicitly defined
  // where the model is declared, rather than where it's used
  implicit def editClass[T, L <: HList, E <: HList](
    implicit
    l: ClassGeneric.Aux[T, L],
    e: Edit.Aux[L, E]
  ) = new Edit[T] {
    type Model = E

    def toModel(t: T) = e.toModel(l.to(t))

    // TODO: create a component that takes the Model as a parameter
    override def empty: E = e.empty

    override def element(t: E): ReactNode = e.element(t)
  }

  // conceivably Edit2.Aux[L, M] could drop some fields of L
  // e.g. Id fields get dropped
  // so we really need to work off of M directly after Edit2
  // e.g. we can't iterate over L to get headers
  implicit def hlistEdit[L <: HList, O <: HList, M <: HList, H <: HList](
    implicit
    edit2: Edit2.Aux[L, M]
  ) = new Edit[L] {
    override type Model = M

    def toModel(t: L) = edit2.toModel(t)

    // TODO: create a component that takes the Model as a parameter
    override def empty: M = edit2.empty

    override def element(t: M) = {
      // TODO: we should be using a grid system here rather than a table
      val elements = edit2.elements(t)
      // TODO: figure out whether we really need keys here
      val rand = new Random
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable(selectable = false)(
            MuiTableBody(displayRowCheckbox = false)(
              elements.grouped(2) map { seq =>
                MuiTableRow(key = rand.nextString(5), displayBorder = false)(
                  seq map { e =>
                    MuiTableRowColumn(key = rand.nextString(5))(e)
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
