package gravity

import app.models.One
import shapeless._
import shapeless.ops.record.{Modifier, Remove, Remover, SelectAll}

object derive {

  import shapeless.record._

  /**
    * There are many situations where given some data structure `A` we'd like to derive some related data
    * structure `B`.  We could do this by simply declaring B explicitly and then writing functions which
    * map B back to A.  For example, take A = User and B = LoginForm.  User has many fields on it.  For LoginForm,
    * only username and password are relevant.  Rather than declare LoginForm as its own case class, I could
    * derive LoginForm as:
    *
    * val loginForm = Derive[User] { _.select('username, 'password).ensureOptional }
    *
    * This produces a shapeless record derived from User with only the username and password fields selected.
    * In addition, we make those fields optional since in the UI they start out empty.
    *
    * (This may not be a great example since LoginForm is fairly simply but you can imagine more complicated
    * derived structures with nesting, where it would be repetitive to copy and modify the existing case classes.)
    *
    * Another cool feature is that in addition to deriving the related class B, in the course of derivation we
    * can derive related structures over A and B.  In particular, we can automatically derive certain validations:
    * requiredness of username and password is derived from the fact that in order to form the originating
    * function B => A, the optional fields must be filled.
    *
    * By deriving B we can also derive the query necessary to "fill" B by taking the query known to "fill" A
    * and applying the analogous transforms for the query in parallel.
    *
    *
    * A stronger example where we do all this is detail pages:
    *
    * the model for AccountDetailPage is something like:
    *
    * Derive[Account] { a =>
    *   'theAccount ->> a.replaceLookupsWithNames :::
    *   findAllChildrenOfAccoun(_.selectRelatedListColumns.replaceLookupsWithNames) :::
    *   'newAccount ->> a.duplicate.ensureOptional |||
    *   'newContact ->> c.duplicate.ensureOptional
    * }
    *
    * can't the model depend on values sometime? i think that's OK
    *
    * do we need a better syntax for all this?
    *
    *
    * Allows us to define a type-level function A => B by way of a value-level transformation:
    *
    *     val derived = Derive[Foo] { _.removed('id) } // where does bar come from?
    *
    * The output type can then be extracted as derived.Out.
    *
    * You can think of this as "shape-changing".  Functions that do stuff at value-level aren't
    * meaningful here.  We only care about stuff that changes the shape of the type:  removals, adds, selections,
    * type changes, nesting, unnesting, duplications, possibly other transformations.
    */
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
    ev(r)('username)
  }

  implicit class MoreRecordOps[R <: HList](r: R) {
    def removed(w: Witness)(implicit remove: Remover[R, w.T]) = remove(r)

    def selectAll[L <: HList](l: L)(implicit select: SelectAll[R, L]) = select(r)

    // don't need this just need updateWith
    def modify[A, B](w: Witness)(f: A => B)(implicit modify: Modifier[R, w.T, A, B]) = modify(r, f)

    // Does a replaceWith for a reference
    // Take a reference such as One[T] and convert it to Y?
    def resolve[W <: Witness, X, Y](w: W)(f: X => Y)
      (implicit modifier: Modifier[R, w.T, One[X], One[Y]]) = modifier(r, { (ox: One[X]) => ox.map(f) })
    // What does updateWith do?
  }
}