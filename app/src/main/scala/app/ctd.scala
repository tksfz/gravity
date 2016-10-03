import app.HasSameKeys
import app.models.Account
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.Frag
import shapeless._
import shapeless.labelled._
import shapeless.ops.record.Selector
import shapeless.syntax.singleton._
import shapeless.tag.@@

object ctd {

  // An attempt at a highly generic abstraction for a wide variety of metadata

  /**
    * In the context of phantom type `C`, compute a function `A => B`.
    * Typically, C will be a model class; T will be a field label, value or field-value; and D can be a rendering or
    * some metadata.
    *
    * Sometimes T ranges over an ordinary set of values (e.g. T = Int).  Other times, T is a singleton type.
    */
  trait ContextFunction[C, A, B] {
    def apply(t: A): B
  }

  /**
    * In situations where we want to ensure that a Label exists for all F in C, then you'd have to add
    * an implicit LiftAll over the curried Label[C, ?] against the hlist representation of C. (or we write a custom
    * typeclass for it)
    * @tparam C
    * @tparam S
    */
  trait Label[C, S /*<: Symbol*/] extends ContextFunction[C, S, String]

  /**
    * Is it really useful for View to be a ContextFunction and accept C as a type parameter?
    *
    * It seems like it should be sufficient for FieldView to be a context function.
    * FieldView rather than delegating to kview, fetches the Label directly and generates the ReactNode
    * *by default*.  FieldView can still be overridden to provide a custom label element when desired.
    * @tparam C
    * @tparam T
    */
  trait View[C, T] extends ContextFunction[C, T, ReactNode] {
    // of course here we could have a asString method etc.
  }

  trait FieldView[C, K, V] extends ContextFunction[C, FieldType[K, V], (ReactNode, ReactNode)]


  implicit def stringView[C] = new View[C, String] {
    override def apply(t: String): ReactNode = {
      t
    }
  }

  /**
    * We can get a View for any [C, K] for which a Label exists.
    */
  implicit def labelView[C, K](implicit label: Label[C, K]) = new View[C, K] {
    override def apply(t: K): ReactNode = {
      label.apply(t) // implicit conversion from String => ReactNode
    }
  }

  implicit def fieldView[C, K, V]
    (implicit
      witness: Witness.Aux[K],
      kview: View[C, K],
      vview: View[C, V]) = new FieldView[C, K, V] {
    override def apply(t: FieldType[K, V]): (ReactNode, ReactNode) = {
      (kview.apply(witness.value), vview.apply(t))
    }
  }


  // case class Labels

  // implicits from mapping over a Poly2

  import shapeless.record._

  def labelsFromRecord[C, S <: Symbol, L <: HList, R <: HList](data: R)
    (implicit s: ops.record.Selector.Aux[R, S, String]) = new Label[C, S] {
    override def apply(t: S): String = s.apply(data)
  }

  object labelsFromRecord3 {
    def apply[C, S, R <: HList] = new Curry2[C, S, R]
    class Curry2[C, S, R <: HList] {
      def apply[L <: HList](data: R)
        (implicit l: LabelledGeneric.Aux[C, L],
          s: ops.record.Selector.Aux[R, S, String],
          k: HasSameKeys[L, R]) = new Label[C, S] {
        override def apply(t: S): String = s.apply(data)
      }
    }
  }

  implicit def selectorTransitive[R1 <: HList, R2 <: HList, K]
  (implicit
    k: ops.record.Selector[R1, K],
    r1Andr2: HasSameKeys[R1, R2]
  ): Selector[R2, K] = ???

  // If we want to hide the [S] bit from app devs
  // we could add another level of implicits
  implicit def accountLabels[S, R <: HList]
  (implicit s: ops.record.Selector.Aux[R, S, String]) = labelsFromRecord3[Account, S, R].apply {
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") :: HNil
  }


  class ClassLabels[C, R <: HList](data: R) extends ContextMap[C, R](data)
  object ClassLabels {
    def apply[C] = new Curried[C]
    class Curried[C] {
      def apply[M <: HList](map: M) = new ClassLabels[C, M](map)
    }
  }

  implicit val accountLabels2 = ClassLabels[Account](('id ->> "Id") :: HNil)

  implicit def labelFromClassLabels[C, F, R <: HList]
  (implicit cl: ClassLabels[C, R], f: ops.record.Selector.Aux[R, F, String]
  ) = new Label[C, F] {
    override def apply(t: F): String = f.apply(cl.map)
  }

  trait IsFieldOf[T, F]

  object IsFieldOf {
    type Aux[T] = { type Lambda[F] = IsFieldOf[T, F] }
  }

  implicit def isFieldOf[T, L, F]
  (implicit
    l: LabelledGeneric.Aux[T, L],
    f: ops.record.Selector[L, F]) = new IsFieldOf[T, F] { }

  /*
  implicit def label[C, F]
  (implicit

  ) = new Label[C, F] {

  }*/

  trait ContextMap[C, M <: HList] {
    def map: M
  }

  abstract class ContextMap[C, M <: HList](val map: M)

  implicit def contextFunctionFromMap[C, M <: HList, K, V]
  (implicit map: ContextMap[C, M],
    s: ops.record.Selector.Aux[M, K, V]) = new ContextFunction[C, K, V] {
    override def apply(t: K): V = s.apply(map.map)
  }

}