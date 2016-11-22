package blog

import java.time.{LocalDate, Month}

import blog.models.Post
import gravity.ui.apiclient.Get

import scala.concurrent.Future

object AllPosts {
  val posts =
    Seq(
      Post(
        1, "Metadata as Types", "Thomas Kim", LocalDate.of(2016, Month.NOVEMBER, 5),
        """
          |Metadata should be types
        """.stripMargin
      )
    )

  implicit def toGet = new Get[Post] {
    override def get(id: Int): Future[Option[Post]] = Future.successful {
      posts.find(_.id == id)
    }
  }
}
