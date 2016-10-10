package gravity

import app.models.{Contact, One}

/**
  * TODO:
  * a few additinoal approaches to explore:
  * (1) allow people to attach metadata to a case class by declaring associated case classes and,
  * (2) possibly, (singleton) instance of that case class with related data
  * (3) type providers.  e.g. generate a case class from a shapeless record. this would make syntax soo nice.
  *
  * examples of (1) and (2):
  *
  * Ah maybe we get some of this using the trick at https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/records.scala
  *
  * For label declarations that seems to work.
  *
  * Then the only thing missing is the ability to get new case classes for extended data types
  * (e.g. appending CreatedDate, LastUpdatedDate automatically)
  *
  */
object associate {

  case class Labels[T]()
  trait Hidden[T]
  trait Method[T]

  case class Account(id: String, name: String, numEmployees: Int, primaryContact: One[Contact],
    fullName: Method[String] // A related idea
  )

  /** Transform of case class where everything is a string */
  case class AccountLabels(
    id: String,
    name: String,
    numEmployees: String,
    primaryContact: String
  )

  // maybe even use default values to set values?
  implicit val accountLabels = AccountLabels(id = "id", name = "Name", numEmployees = "Number of employees", primaryContact = "primary contact")

  case class AccountLayout(
    id: Hidden[String]
    //name: TextInputField = TextInputField(maxLength=30),
    //numEmployees: NumericInputField,
    //primaryContext: SearchableLookupField[Contact]
  )

  // Assuming we have a type provider
  //implicit val labels = Labels[Account](id = "Id", name = "Name") etc
}