package app

import japgolly.scalajs.react.{ReactComponentB, ReactElement}
import shapeless._
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless.ops.hlist.{Mapper, ToTraversable}


object generator {

  trait HasLabels[T] {
    def label: String

    //def fieldLabels[R : R
  }

  // P, S, B, N probably need to be pushed up
  trait ComponentGenerator[T] {
    def view(t: T): Either[String, ReactElement]
    // def asString
    // def toReactElement
    // def inTable etc.
    // def withField
    //def edit[P, S, B, N](t: T): ReactComponentU[P, S, B, N]
  }

  implicit object StringComponentGenerator extends ComponentGenerator[String] {
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

  // https://github.com/milessabin/shapeless/issues/73
  trait DefaultForPoly extends Poly1 {
    implicit def default[T] = at[T] { _ => Left("") }
  }

  object getGenerator extends DefaultForPoly {
    implicit def hasGenerator[T : ComponentGenerator] = at[T].apply[Either[String, ReactElement]]({ x: T =>
      implicitly[ComponentGenerator[T]].view(x)
    })
  }

  implicit def caseClassGenerator[T <: Product, L <: HList, O <: HList]
    (implicit l: Generic.Aux[T, L],
      mapped: Mapper.Aux[getGenerator.type, L, O],
      trav: ToTraversable.Aux[O, List, Either[String, ReactElement]]
    ) = new ComponentGenerator[T] {
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
  }

}