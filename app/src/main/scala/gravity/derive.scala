package gravity

import shapeless._

object derive {

  import shapeless.record._

  class Derive[A, L <: HList, B](fn: L => B)
    (implicit l: LabelledGeneric.Aux[A, L]) {
    type Out = B
  }

  object Derive {
    class Curried2[T, L <: HList](implicit l: LabelledGeneric.Aux[T, L]) {
      def apply[B](fn: L => B) = new Derive[T, L, B](fn)
    }

    class Curried[T] {
      // Force determination of L prior to receiving fn, so that the type of fn's argument can be inferred (rather than
      // having to be specified explicitly
      def apply[L <: HList](implicit l: LabelledGeneric.Aux[T, L]) = new Curried2[T, L]
    }
    def apply[T] = new Curried[T]
  }

  case class User(id: Int, username: String, password: String)

  val loginModel = Derive[User].apply.apply { _.remove('id)._2 }

  def doLogin[R](r: R)(implicit ev: R =:= loginModel.Out) = {
    //ev(r).fieldAt('id)
    ev(r).fieldAt('username)
  }
}