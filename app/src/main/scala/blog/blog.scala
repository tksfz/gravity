package blog

import java.time.LocalDate

import scala.concurrent.Future

object models {

  import AllPosts._

  case class Post(
    id: Int,
    title: String,
    author: String,
    date: LocalDate,
    body: String
  )

  // TODO: move this implicit to the framework
  implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(_.toEpochDay)

  def allPostsQuery: Future[Seq[Post]] = Future.successful(AllPosts.posts.sortBy(_.date))
}
