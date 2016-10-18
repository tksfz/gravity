package gravity

object models {

  trait Reference[T]

  type ZeroOrOne[T] = Option[One[T]]

  trait Many[T] extends Reference[T]

  trait One[T] extends Reference[T] {
    def map[B](f: T => B): One[B] = ???
  }
  case class OneId[T](id: ObjectId) extends One[T]
  //case class ResolvedOne[T, R](r: R) extends One[T]

  case class Phone(phone: String)
  case class Url(url: String)

  case class ObjectId(id: Int)
}
