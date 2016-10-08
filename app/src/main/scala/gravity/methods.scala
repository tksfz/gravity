package gravity


import shapeless._
import shapeless.ops.record._
import shapeless.syntax.singleton._
import shapeless.record._
import poly._


// record defs
object methods {

  // adding a monomorphic function doesn't work because our record may change
  // thing may be added after the function is added

  trait Compute[HF <: Poly, In <: HList] extends DepFn1[In] with Serializable { type Out }

  // note there's a case1.aux
  object Compute {
    implicit def computer[HF <: Poly, In <: HList]
    (implicit hc: Case1[HF, In]) = new Compute[HF, In] {
      type Out = hc.Result
      def apply(l: In) = hc(l)
    }
  }

  // abstracts over simple Select and Compute apply
  /*
  trait Execute[L <: HList, K, V] extends DepFn1[L] { type Out = V }
  object Execute {
    implicit def executeBySelect[L, K, V](implicit sel: Selector.Aux[L, K, V]) = new Execute[L, K, V] {
      def apply(l: L) = sel(l)
    }

    // need a compute.aux
    implicit def executeByCompute[L, K, HF, V]
    (implicit sel: Selector.Aux[L, K, HF],
      compute: Compute[HF, L]) = new Execute[L, K, V] {
      def apply(l: L) = compute(l)
    }
  } */

  def compute[L <: HList](l: L, f: Poly)(implicit compute: Compute[f.type, L]) = compute.apply(l)

}