package app

import gravity.methods.Method
import gravity.ui.Labels
import shapeless.ops.record.Selector
import shapeless._

object models {

  case class Address(
    street: Option[String],
    city: Option[String],
    country: Option[String],
    postalCode: Option[String]
  )

  case class User(
    id: Int,
    username: String,
    alias: String
  )

  // visibility - isDeleted, ownership or security, back-end details
  case class Account(
    id: Int,
    ownerId: One[User],
    name: String,
    numEmployees: Int,
    address: Address
  )

  implicit val accountLabels = Labels[Account].apply(
    id = "Id",
    name = "Name",
    numEmployees = "Number of employees"
  )

  case class Contact(
    //id: Int,
    //ownerId: One[User],
    lastName: String,
    firstName: String
    //title: Option[String],
    //account: ZeroOrOne[Account],
    //mailingAddress: Address,
    //otherAddress: Address
  ) {
    def fullName = firstName + lastName
  }

  implicit val contactLabels = Labels[Contact].apply (
    firstName = "First Name",
    lastName = "Last Name",
    fullName = "Name"
  )

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

  def execute[L <: HList, HF <: Poly](l: L, w: Witness)
    (implicit
      select: Selector.Aux[L, w.T, HF],
      compute: Method[HF, L]) = {
    compute(l)
  }

  trait Reference[T]

  trait ZeroOrOne[T] extends Reference[T]

  trait Many[T] extends Reference[T]

  trait One[T] extends Reference[T] {
    def map[B](f: T => B): One[B]
  }
  //case class Unresolved[T](id: Int) extends One[T]

}


