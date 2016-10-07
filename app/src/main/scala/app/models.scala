package app

import shapeless.syntax.singleton._
import gravity.view2.Labels

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

}


