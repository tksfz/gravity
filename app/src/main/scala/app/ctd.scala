import app.models.Account
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.Frag
import shapeless._
import shapeless.labelled._
import shapeless.syntax.singleton._

object ctd {

  // An attempt at a highly generic abstraction for a wide variety of metadata

  /**
    * In the context of phantom type C, for a value of type T, fetch some data of type D.
    * Typically, C will be a class; T will be a field label, value or field-value; and D can be a rendering or
    * some metadata.
    *
    * Sometimes T ranges over an ordinary set of values (e.g. T = Int).  Other times, T is a singleton type.
    */
  trait ClassTargetData[C, T, D] {
    def get(t: T): D
  }

  trait Labels[C, S <: Symbol] extends ClassTargetData[C, S, String]

  trait View[C, T] extends ClassTargetData[C, T, ReactNode] {
    // of course here we could have a asString method etc.
  }

  trait FieldView[C, K, V] extends ClassTargetData[C, FieldType[K, V], (ReactNode, ReactNode)]


  implicit def viewString[C] = new View[C, String] {
    override def get(t: String): ReactNode = {
      t
    }
  }

  /**
    * We can get a View for any [C, K] for which we have labels.
    */
  implicit def symbolView[C, K <: Symbol](implicit labels: Labels[C, K]) = new View[C, K] {
    override def get(t: K): ReactNode = {
      labels.get(t) // implicit conversion from String => ReactNode
    }
  }

  implicit def viewField[C, K, V]
    (implicit
      witness:  Witness.Aux[K],
      kview: View[C, K],
      vview: View[C, FieldType[K, V]]) = new FieldView[C, K, V] {
    override def get(t: FieldType[K, V]): (ReactNode, ReactNode) = {
      (kview.get(witness.value), vview.get(t))
    }
  }


  // case class Labels

  // implicits from mapping over a Poly2

  import shapeless.record._

  def labelsFromRecord[C, S <: Symbol, L <: HList, R <: HList](data: R)
    (implicit s: ops.record.Selector.Aux[R, S, String]) = new Labels[C, S] {
    override def get(t: S): String = s.apply(data)
  }

  object labelsFromRecord3 {
    def apply[C, S <: Symbol] = new Curry2[C, S]
    class Curry2[C, S <: Symbol] {
      def apply[R <: HList](data: R)(implicit s: ops.record.Selector.Aux[R, S, String]) = new Labels[C, S] {
        override def get(t: S): String = s.apply(data)
      }
    }
  }

  // If we want to hide the [S] bit from app devs
  // we could add another level of implicits 
  implicit def accountLabels[S <: Symbol] = labelsFromRecord3[Account, S] {
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") :: HNil

  }

}