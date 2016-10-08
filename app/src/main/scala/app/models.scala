package app

import gravity.methods.Compute
import shapeless.syntax.singleton._
import gravity.ui.Labels
import shapeless.ops.record.Selector
import shapeless._
import shapeless.record._

object models {
  case class Account(
    id: Int,
    name: String,
    numEmployees: Int
  )

  // allow a tuple syntax?
  implicit val mylabelsData = Labels[Account].apply(
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") ::
      shapeless.HNil
  )

  case class Contact(firstName: String, lastName: String) {
    def fullName = firstName + lastName
  }

  // can be attached to any record with the appropriate fields
  // tagged with Contact
  object defFullName extends Poly1 {
    implicit def forContact[R <: HList]
    (implicit
      fname: Selector.Aux[R, Witness.`'firstName`.T, String],
      lname: Selector.Aux[R, Witness.`'lastName`.T, String]) = at[R] { x => fname(x) + lname(x) }
  }

  // one thing is we don't know the result type of the higher-rank function
  // but i think there's an Aux for that
  val generic = LabelledGeneric[Contact].to(Contact("Mary", "Smith")).merge(('fullName ->> defFullName) :: HNil)

  def main(args: Array[String]): Unit = {
    println(execute(generic, 'fullName))
  }

  def execute[L <: HList, HF <: Poly](l: L, w: Witness)
    (implicit
      select: Selector.Aux[L, w.T, HF],
      compute: Compute[HF, L]) = {
    compute(l)
  }

  trait Reference[T]

  trait One[T] extends Reference[T] {
    def map[B](f: T => B): One[B]
  }
  //case class Unresolved[T](id: Int) extends One[T]

}


