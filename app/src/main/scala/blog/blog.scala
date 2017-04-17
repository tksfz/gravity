package blog

import java.time.LocalDate

import gravity.ui.ClassRoutes.{EditTrait, ModulePage, ViewModulePage}
import gravity.ui._
import shapeless._

import scala.concurrent.Future

object models {

  import AllPosts._
  import gravity.ui.generic._

  case class Post(
    id: Int,
    title: String,
    author: String,
    date: LocalDate,
    body: String
  )

  case class PostEdit(id: Int) extends AnyPage with EditTrait[Post]
  case class PostView(id: Int) extends AnyPage
  case object PostList extends AnyPage

  // TODO: detail page code should accept P as a type param
  // and a function from id => P
  // that will be passed to the route.caseClass method
  // basically the page classes and constrcutors are passed in to the library
  // such that the caller defines the page classes
  // this is kind of necessary for a couple reasons
  // (1) the caller should be setting up the site map
  // (2) EditPage could go unused but then it's an orphaned route (unused in any actual route)
  // but still referenceable

  import EnableRelaxedImplicits._

  implicit val (_, _) = (Routable[PostEdit], Routable[PostList.type])

  import scala.reflect._

  implicit def linkFunction = LinkFunction[ModulePage, AnyPage] {
    case p@ViewModulePage(id) if p.ct == classTag[Post] => PostView(id)
  }

  // TODO: standard routes should take MainLayout as an implicit
  implicit val postRoutes = ClassRoutes[Post] {
      ClassRoutes.standardViewPageRoute[Post, AnyPage, PostView] |
        ClassRoutes.classListPageRoute[Post, AnyPage, PostList.type](allPostsQuery)
    }

  //def query = Query[Post].listAll
  // this should be of type
  // Query[Seq[Post]]

  // TODO: move this implicit to the framework
  implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(_.toEpochDay)

  def allPostsQuery: Future[Seq[Post]] = Future.successful(AllPosts.posts.sortBy(_.date))
}
