package app

import java.time.LocalDate
import java.util.Date

import gravity.methods.Method
import gravity.ui.Labels
import shapeless.ops.record.Selector
import shapeless._
import gravity.models._

object models {

  case class Address(
    street: Option[String] = None,
    city: Option[String] = None,
    country: Option[String] = None,
    postalCode: Option[String] = None
  )

  case class User(
    id: ObjectId,
    username: String,
    alias: String,
    name: String
  )

  // visibility - isDeleted, ownership or security, back-end details
  case class Account(
    id: ObjectId,
    owner: One[User],
    name: String,
    parentAccount: Option[One[Account]] = None,
    numEmployees: Option[Int] = None,
    phone: Option[Phone] = None,
    fax: Option[Phone] = None,
    accountNumber: Option[String] = None,
    website: Option[Url] = None,
    rating: Option[String] = None,
    site: Option[String] = None,
    source: Option[String] = None,
    tickerSymbol: Option[String] = None,
    accountType: Option[String] = None,
    ownership: Option[String] = None,
    industry: Option[String] = None,
    annualRevenue: Option[BigDecimal] = None,
    sicCode: Option[String] = None,
    // Sub-fields are already optional so this doesn't need to be optional
    billingAddress: Address = Address(),
    shippingAddress: Address = Address(),
    description: Option[String] = None,
    createdBy: Option[One[User]] = None,
    createdDate: Date = new Date
  )

  implicit val accountLabels = Labels[Account].apply(
    id = "Id",
    name = "Name",
    numEmployees = "Number of employees"
  )

  case class Contact(
    id: ObjectId,
    owner: One[User],
    lastName: String,
    firstName: Option[String] = None,
    account: Option[One[Account]] = None,
    title: Option[String] = None,
    department: Option[String] = None,
    birthdate: Option[LocalDate] = None,
    homePhone: Option[Phone] = None,
    mobilePhone: Option[Phone] = None,
    otherPhone: Option[Phone] = None,
    faxPhone: Option[Phone] = None,
    email: Option[String] = None,
    assistant: Option[String] = None,
    reportsTo: Option[One[Contact]] = None,
    leadSource: Option[String] = None,
    mailingAddress: Address = Address(),
    otherAddress: Address = Address(),
    description: Option[String] = None,
    createdBy: Option[One[User]] = None,
    createdDate: Date = new Date
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

}


