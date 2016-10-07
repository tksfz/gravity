package app

import shapeless.syntax.singleton._
import gravity.ui.Labels

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

  trait Reference[T] {
  }
  trait One[T] extends Reference[T] {
    def map[B](f: T => B): One[B]
  }
  //case class Unresolved[T](id: Int) extends One[T]

}


