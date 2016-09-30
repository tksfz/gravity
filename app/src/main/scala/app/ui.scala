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

  /*
  trait Renderable {
  def toString: String // for use in attributes
  def toReactElement: ReactElement
  // also could have options in lists
  // detail views, list views
  }
   */

  abstract class Poly1WithDefault[V](defaultValue: V) extends Poly1 {
    implicit def default[T] = at[T] { _ => defaultValue }
  }

  // https://github.com/milessabin/shapeless/issues/73
  trait DefaultForPoly extends Poly1 {
    implicit def default[T] = at[T] { _ => Left("") }
  }

  object getGenerator extends DefaultForPoly {
    implicit def hasGenerator[T : View] = at[T].apply[Either[String, ReactElement]]({ x: T =>
      implicitly[View[T]].view(x)
    })
  }

  /*
  implicit def caseClassGenerator[T, L <: HList, O <: HList]
    (implicit l: Generic.Aux[T, L],
      mapped: Mapper.Aux[getGenerator.type, L, O],
      trav: ToTraversable.Aux[O, List, Either[String, ReactElement]]
    ) = new View[T] {
    def view(t: T): Either[String, ReactElement] = {
      val fieldGenerators: List[Either[String, ReactElement]] = (l.to(t) map getGenerator).toList
      val elems: Seq[ReactElement] = fieldGenerators map {
        case Left(str) => <.div(str).render
        case Right(elem) => elem
      }
      val rcb = ReactComponentB[Unit]("rcb")
        .render(P =>
          <.div(
            elems
          )
        )
        .build
      Right(rcb())
    }
  } */


  /*

  trait Labels[T] {
    type L <: HList

    //def label: String
    def fieldLabels: L

    // where F is a field in the generic record representation of T
    def fieldLabel[F]: String
  }

  case class SomeLabels[T, GT, KGT, FieldsToLabels <: HList, KFTL](labels: FieldsToLabels)
    (implicit generic: LabelledGeneric.Aux[T, GT],
      kgt: Keys.Aux[GT, KGT],
      kftl: Keys.Aux[FieldsToLabels, KFTL],
      align: Align[KGT, KFTL]) extends Labels[T] {
    type L = FieldsToLabels
    def fieldLabels = labels
    def fieldLabel[F]
  } */

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
  implicit def makeTableView[T, L <: HList, O <: HList]
  (implicit l: LabelledGeneric.Aux[T, L],
    mapped: Mapper.Aux[getFieldGenerators.type, L, O],
    trav: ToTraversable.Aux[O, List, (StringOrElement, StringOrElement)]
   ) = new View[T] {
    def view(t: T): Either[String, ReactElement] = {
      val fieldElems: List[(StringOrElement, StringOrElement)] = (l.to(t) map getFieldGenerators).toList
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

  object getFieldGenerators extends Poly1WithDefault((Left(""), Left(""))) {
    implicit def when[K, V]
      (implicit witness: Witness.Aux[K],
        kview: View[K], vview: View[V]) = at[FieldType[K, V]].apply[(StringOrElement, StringOrElement)] { generateViewsForField(_) }
  }

  // View[K] or View[W] where W is the Widen of K?
  def generateViewsForField[K, V](f: FieldType[K, V])
    (implicit
      witness: Witness.Aux[K],
      kview: View[K], vview: View[V]): (StringOrElement, StringOrElement) = {
    (kview.view(witness.value), vview.view(f))
  }
}