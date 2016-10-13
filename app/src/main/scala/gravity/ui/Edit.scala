package gravity.ui

import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn, MuiTextField}
import chandu0101.scalajs.react.components.Implicits._
import gravity.ClassGeneric
import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.tag.@@

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

  import shapeless.labelled._

  implicit def editClassTaggedField[K, V, M, C](
    implicit edit: Edit.Aux[V, M]
  ) = new Edit[FieldType[K, V] @@ C] {
    override type Model = FieldType[K, M] @@ C

    // TODO: create a component that takes the Model as a parameter
    override def toModel(t: FieldType[K, V] @@ C): FieldType[K, edit.Model] @@ C = {
      tag[C](field[K](edit.toModel(t)))
    }

    override def empty: FieldType[K, edit.Model] @@ C = tag[C](field[K](edit.empty))

    override def element(t: Model): ReactNode = edit.element(t)
  }

  implicit def editField[K, V](
    implicit edit: Edit[V]
  ) = new Edit[FieldType[K, V]] {
    override type Model = FieldType[K, edit.Model]

    // TODO: create a component that takes the Model as a parameter
    override def toModel(t: FieldType[K, V]): FieldType[K, edit.Model] = {
      field[K](edit.toModel(t))
    }

    override def empty: FieldType[K, edit.Model] = field[K](edit.empty)

    override def element(t: Model): ReactNode = edit.element(t)
  }

  // todo headers
  // how do we make certain fields not editable?
  // we might consider making this not implicit and forcing model classes
  // to declare explicit views etc.
  // they can always have a choice of implementations
  // one thing that does is it allows the compiler to show an error
  // at the time the implicit is explicitly defined
  // where the model is declared, rather than where it's used
  implicit def classEdit[T, L <: HList, E <: HList](
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

  type Aux[T, M] = Edit[T] { type Model = M }

  // conceivably Edit2.Aux[L, M] could drop some fields of L
  // e.g. Id fields get dropped
  // so we really need to work off of M directly after Edit2
  // e.g. we can't iterate over L to get headers
  implicit def hlistEdit[L <: HList, O <: HList, M <: HList, H <: HList](
    implicit
    edit2: Edit2.Aux[L, M],
    mapper: Mapper.Aux[headerPoly.type, M, H],
    trav: ToTraversable.Aux[H, List, ReactNode]
  ) = new Edit[L] {
    override type Model = M

    def toModel(t: L) = edit2.toModel(t)

    // TODO: create a component that takes the Model as a parameter
    override def empty: M = edit2.empty

    override def element(t: M) = {
      val elements = edit2.elements(t)
      val headers = (t map headerPoly).toList
      val fields = headers zip elements
      ReactComponentB[Unit]("blah")
        .render(_ =>
          MuiTable()(
            MuiTableBody(displayRowCheckbox = false)(
              fields map { case (l, r) =>
                MuiTableRow()(
                  MuiTableRowColumn()(l),
                  MuiTableRowColumn()(r)
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

  object headerPoly extends Poly1 {
    implicit def foo[T]
    (implicit
      header: Header[T]) = at[T] { t => header.header }
  }

  trait Create[T]

}
