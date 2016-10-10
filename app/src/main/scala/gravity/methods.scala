package gravity


import shapeless._
import shapeless.ops.record._
import poly._

/**
  * Methods for records. These are encoded as polymorphic functions as record values.
  */
object methods {

  trait Method2[T]

  // ALTERNATIVE idea:
  // allow methods to be declared on the originating case class
  case class Contact2(id: String, firstName: String, lastName: String, fullName: Method2[String])

  // adding a monomorphic function doesn't work because our record may change
  // thing may be added after the function is added

  trait Method[HF <: Poly, In <: HList] extends DepFn1[In] with Serializable { type Out }

  // note there's a case1.aux
  object Method {
    type Aux[HF <: Poly, In <: HList, Out0] = Method[HF, In] { type Out = Out0 }

    implicit def method[HF <: Poly, In <: HList](implicit hc: Case1[HF, In]): Aux[HF, In, hc.Result] = new Method[HF, In] {
      type Out = hc.Result
      def apply(l: In) = hc(l)
    }
  }

  /**
    * Get a field or invoke a method.  Uniform access principle.
    */
  // abstracts over simple Select and Compute apply
  trait Access[L <: HList, K] extends DepFn1[L]

  object Access {

    type Aux[L <: HList, K, V] = Access[L, K] { type Out = V }

    implicit def accessSelect[L <: HList, K, V](implicit sel: Selector.Aux[L, K, V]) = new Access[L, K] {
      type Out = V
      def apply(l: L) = sel(l)
    }

    implicit def accessMethod[L <: HList, K, HF <: Poly, V]
    (implicit
      sel: Selector.Aux[L, K, HF],
      method: Method.Aux[HF, L, V]) = new Access[L, K] {
      type Out = V
      def apply(l: L) = method(l)
    }
  }

  def invoke[L <: HList](l: L, f: Poly)(implicit invoke: Method[f.type, L]) = invoke.apply(l)

}