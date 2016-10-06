package app

import japgolly.scalajs.react.{ReactComponentB, ReactElement}
import shapeless._
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless.ops.hlist._
import chandu0101.scalajs.react.components.materialui._
import shapeless.labelled.FieldType
import shapeless.ops.record.{Fields, Keys, Values}

object ui {

  trait HasLabels[T] {
    def label: String

    //def fieldLabels[R : R
  }

  // P, S, B, N probably need to be pushed up
  trait View[T] {
    def view(t: T): Either[String, ReactElement]
    // def asString
    // def toReactElement
    // def inTable etc.
    // def withField
    //def edit[P, S, B, N](t: T): ReactComponentU[P, S, B, N]
  }

  implicit object StringView extends View[String] {
    def view(t: String) = Left(t)
  }

  implicit object IntView extends View[Int] {
    def view(n: Int) = Left(n.toString)
  }

  /*
  trait Renderable {
  def toString: String // for use in attributes
  def toReactElement: ReactElement
  // also could have options in lists
  // detail views, list views
  }
   */

  // https://github.com/milessabin/shapeless/issues/73
  abstract class Poly1WithDefault[V](defaultValue: V) extends Poly1 {
    implicit def default[T] = at[T] { _ => defaultValue }
  }

  object getGenerator extends Poly1WithDefault(Left("")) {
    implicit def hasGenerator[T : View] = at[T].apply[Either[String, ReactElement]]({ x: T =>
      implicitly[View[T]].view(x)
    })
  }

  implicit def fieldLabelGenerator[L <: Symbol]
    (implicit widen: Widen.Aux[L, Symbol]) = new View[L] {
    override def view(t: L): Either[String, ReactElement] = {
      Left(widen(t).name)
      //Left(t.name)
    }
  }

  type StringOrElement = Either[String, ReactElement]

  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[T, L <: HList, TL <: HList, O <: HList]
    (implicit
      l: LabelledGeneric.Aux[T, L],
      zipConst: ZipConst.Aux[T, L, TL],
      mapped: Mapper.Aux[labelledFieldView2.type, TL, O],
      trav: ToTraversable.Aux[O, List, (StringOrElement, StringOrElement)]) = new View[T] {
    def view(t: T): Either[String, ReactElement] = {
      val fieldElems: List[(StringOrElement, StringOrElement)] =
        ((l.to(t) zipConst t) map labelledFieldView2).toList
      val mui =
        ReactComponentB[Unit]("blah")
          .render(_ =>
            MuiTable()(
              MuiTableBody(displayRowCheckbox = false)(
                fieldElems map { case (l, r) =>
                  MuiTableRow()(
                    l match {
                      case Left(s) => MuiTableRowColumn()(s)
                      case Right(e) => MuiTableRowColumn()(e)
                    },
                    r match {
                      case Left(s) => MuiTableRowColumn()(s)
                      case Right(e) => MuiTableRowColumn()(e)
                    }
                  )
                }
              )
            )
          )
          .build
      Right(mui())
    }
  }

  trait View2[T, A] {
    def view(a: A): Either[String, ReactElement]
  }

  implicit def fieldLabelGenerator2[T, L <: Symbol]
    (implicit widen: Widen.Aux[L, Symbol]) = new View2[T, L] {
    override def view(t: L): Either[String, ReactElement] = {
      Left(widen(t).name)
      //Left(t.name)
    }
  }

  // should we let clients define a Poly1[(T, A)]? to return labels? a record?

  trait FieldView[T] {
    def view[A]
  }

  //trait ClassKeyData[C, K, D]

  case class LabelsData[T, L <: HList, R <: HList](labels: R)
    (implicit
      l: LabelledGeneric.Aux[T, L],
      hasSameKeys: HasSameKeys[L, R]) {
  }

  object LabelsData {
    class X[T] {
      def apply[L <: HList, R <: HList](labels: R)
        (implicit
          l: LabelledGeneric.Aux[T, L],
          hasSameKeys: HasSameKeys[L, R]) = {
        LabelsData[T, L, R](labels)
      }
    }
    def apply[T] = new X[T]
  }

  // Implement all these things over L a hlist or record
  // instead of T
  // This is kind of like a Lens in a way
  // i need instances of T, F for every F in T based on a common record R
  // maybe F should be pushed up just like for a Lens?
  trait Label[T, F] {
    def label: String
  }

  // where F is in L
  // in fact we can even relax the restriction that F is in T/L
  // we can fetch the labels for arbitrary F, under T with R
  // so you can fetch labels for random things like
  // Unit, or whatever
  // Basically we have labels for T for arbitrary
  // label types F
  // and this is just a repository of labels related to T
  // the fInL check is useful to enforce
  // that the elements of R are drawn from L
  implicit def fromLabelsData[T, L <: HList, F, R <: HList]
    (implicit
      l: LabelledGeneric.Aux[T, L],
      //fInL: ops.record.Selector.Aux[L, F, _],
      data: LabelsData[T, L, R],
      fInRToo: ops.record.Selector.Aux[R, F, String]) = new Label[T, F] {
    def label = fInRToo.apply(data.labels)
  }

  import shapeless._
  import shapeless.syntax.singleton._

  // and then have an instance for [T, X] where X
  // is the keys of R

  /**
    * Accepts T as a type parameter where T is the original case class containing the field K (with value V)
    * this allows us to potentially generate field labels for the field K knowing what T was.
    *
    * Note that a value of type T is also passed in through the at[] methods, but is unused.
    * We should probably fix this up so that T is passed in on the left-hand side of the tuple.
    * Or, we should make it so that a value of type T never needs to be passed in in the first place.
    */
  object fieldView2 extends Poly1WithDefault((Left(""), Left(""))) {
    implicit def when[T, K, V]
    (implicit
      witness: Witness.Aux[K],
      kview: View[K], vview: View[V]) =
      at[(FieldType[K, V], T)].apply[(StringOrElement, StringOrElement)] { case (f, _) =>
        fieldView(f)
      }
  }

  object labelledFieldView2 extends Poly1WithDefault((Left(""), Left(""))) {
    implicit def when[T, K, V]
      (implicit label: Label[T, K], kview: View[K], vview: View[V]) =
      at[(FieldType[K, V], T)].apply { case (f, _) => labeledFieldView(f) }
    /*
    implicit def when[T, K, V](implicit witness: Witness.Aux[K], kview: View[K], vview: View[V]) =
      at[(FieldType[K, V], T)].apply[(StringOrElement, StringOrElement)] { case (f, _) =>
        fieldView(f)
      } */
  }

  object fieldViews extends Poly1WithDefault((Left(""), Left(""))) {
    implicit def when[K, V](implicit witness: Witness.Aux[K], kview: View[K], vview: View[V]) =
      at[FieldType[K, V]].apply[(StringOrElement, StringOrElement)] {
        fieldView(_)
      }
  }

  // maybe View should always be [C, K]
  def labeledFieldView[T, K, V](f: FieldType[K, V])
    (implicit
      label: Label[T, K],
      kview: View[K], vview: View[V]): (StringOrElement, StringOrElement) = {
    (Left(label.label), vview.view(f))
  }

  // View[K] or View[W] where W is the Widen of K?
  def fieldView[K, V](f: FieldType[K, V])
    (implicit
      witness: Witness.Aux[K],
      kview: View[K], vview: View[V]): (StringOrElement, StringOrElement) = {
    (kview.view(witness.value), vview.view(f))
  }
}