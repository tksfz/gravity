package gravity.ui

import shapeless._
import shapeless.ops.hlist.{Selector, SubtypeUnifier}

// TODO: change terminology. Page is misleading. really these are RouteKeys

/**
  * Dependency injection of route keys; supports linking to app-defined routes.
  *
  * Suppose a library generates a page L and would like to link from L to some app-defined
  * page. Then the library defines a marker trait `T`; and the app author provides some page `P <: T`.
  * Where `T` is wanted, the library declares `implicit t: MkPage[T]`:
  *
  * def doSomething(implicit editPage: MkPage[Int => EditPage[T]]) = {
  *   editPage.apply
  * }
  *
  * The dependency can be made optional by using `Option[MkPage[...]]` instead.
  *
  * To provide this dependency, the app author simply needs to put their page `P <: T` into implicit scope:
  *
  *   implicit val myPage = MyPage[Int => EditPage[Post]](PostEditPage.apply _)
  *
  * The convenience `MkPages` lets the app author inject several dependencies at once.
  *
  * When the desired route is "dynamic" meaning that it accepts arguments (e.g.
  * an edit page requiring a row id), the library depends on a function `T = Id => EditPage` and the
  * app author injects, typically, the route case class's object apply method.
  *
  * TODO: rename to `Link`?
  *
  * @tparam F typically a marker trait declared by a library that would like to link to a particular
  *           kind of page
  */
case class MkPage[F](apply: F)

object MkPage {
  /**
    * An optional dependency is declared as `implicit ev: Option[T] = None`. When implicit `t: T` is available,
    * this provides `Some(t)`.
    */
  implicit def some[F](implicit mkPage: MkPage[F]) = Some(mkPage)

  implicit def mkPageFromKnownPages[L <: HList, F, M <: HList]
  (implicit
    pages: MkPages[L],
    unify: SubtypeUnifier.Aux[L, F, M],
    select: Selector[M, F]) = new MkPage[F](select(unify(pages.pageFns)))
}

/**
  * Shortcut for declaring several `MkPage` instances at once:
  *
  *   implicit val pagesToInject = MkPages(
  *     MySingletonPage
  *       :: MyEditPage.apply _
  *       :: HNil
  *   )
  *
  * Elements should either be functions that return page instances of some known type,
  * or singleton pages.
  */
case class MkPages[L <: HList](pageFns: L)
