package gravity.ui

import scala.concurrent.Future

object apiclient {
  trait Get[T] {
    def get(id: Int): Future[Option[T]]
  }
}