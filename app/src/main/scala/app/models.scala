package app

import app.ui.LabelsData
import shapeless.syntax.singleton._

object models {
  case class Account(
    id: Int,
    name: String,
    numEmployees: Int
  )

  // allow a tuple syntax?
  implicit val mylabelsData = LabelsData[Account].apply(
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") ::
      shapeless.HNil
  )


}


