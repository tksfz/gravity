package gravity.ui

import scala.concurrent.Future

object apiclient {

  // TODO: replace this with a type Query[Id, Option[T]], or with some Auto-wire thing
  trait Get[T] {
    def get(id: Int): Future[Option[T]]
  }
}