package gravity.ui

import apiclient.Get
import chandu0101.scalajs.react.components.materialui.{Mui, MuiAppBar, MuiIconButton, MuiMuiThemeProvider, MuiSvgIcon}
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentC, ReactElement, ReactNode, TopNode}
import japgolly.scalajs.react.extra.router.StaticDsl.{Route, RouteB, Rule}
import japgolly.scalajs.react.extra.router.{Path, RouterConfigDsl, RouterCtl}
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless.{$up => _, _}
import shapeless.ops.hlist.{Selector, SubtypeUnifier}

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.scalajs.js

trait AnyPage

/**
  * Some automatically generated routes closely associated with a class:
  *
  * - CRUD: Create, View, Edit, and Delete pages.
  * - In the future possibly an overview page, list views, recents, etc.
  *
  * This is merely provided as a convenience. There is a more general API for defining arbitrary routes.
  * This just allows routes to brought in when the top-level classes are passed to App.main()
  */
trait ClassRoutes[T] {
  def routes: Rule[AnyPage]
}

object ClassRoutes {

  /** Get the routes implicitly associated with a type `T` using `ClassRoutes[T]` */
  def apply[T : ClassRoutes] = implicitly[ClassRoutes[T]].routes

  /** Define routes with this using `implicit val fooRoutes = ClassRoutes[Foo]( ...some routes ...)` */
  def apply[T](someRoutes: Rule[AnyPage]) = new ClassRoutes[T] {
    def routes = someRoutes
  }

  /**
    * Not sure if T : ClassTag will be adequate here:
    * - Note that routes are two way, so I believe that given an instance of Detail we should be able to map back to
    * the path. So we need to distinguish Detail[Account] vs Detail[Contact].
    * - I'm not sure if the ClassTag will work for doing that, but we'll have to try and see.
    */
  case class ViewPage[T : ClassTag](id: Int) extends AnyPage
  case class EditPage[T : ClassTag](id: Int) extends AnyPage
  case class ListPage[T : ClassTag]() extends AnyPage

  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  import japgolly.scalajs.react.vdom.Implicits._

  // TODO: use optional implicits
  // TODO: change all references to ClassTag to use Label[T]
  implicit def standardRoutes[T]
  (implicit
    v: View[T],
    e: Edit[T],
    get: Get[T],
    ct: ClassTag[T]) = ClassRoutes[T](standardViewPageRoute | standardEditPageRoute)

  import chandu0101.scalajs.react.components.Implicits._

  implicit def toLazyUndefOrReactElement[T <: TopNode](f: () => ReactTagOf[T]): () => js.UndefOr[ReactElement] = { () => f() }

  /**
    * Dependency injection of route keys, used to support linking to app-defined routes.
    *
    * Suppose a library generates a page L and would like to link from the L to some app-defined
    * page P. Then the library defines a marker trait T and the app author ensures that P extends T.
    * Where `P <: T` is wanted, the library declares `implicit t: MkPage[T]`:
    *
    * def doSomething(implicit editPage: MkPage[Int => EditPage[T]]) = {
    *   editPage.apply
    * }
    *
    * To provide this dependency,
    *
    * When the desired route is "dynamic" meaning that it should accept some arguments (e.g.
    * an edit page requiring a row id), the library uses `T = Id => EditPage` and the app author
    * can provide route case class's object apply method.
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
  }

  type MkEditPage[T] = MkPage[Int => EditPage[T]]

  implicit val mkEditPage = MkPage(PostEdit.apply _)

  implicit def mkPageFromKnownPages[L <: HList, F, M <: HList]
  (implicit
    pages: MkPages[L],
    unify: SubtypeUnifier.Aux[L, F, M],
    select: Selector[M, F]) = new MkPage[F](select(unify(pages.pageFns)))

  case class PostEdit(id: Int)

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
    //(implicit allAreFunctions: LiftAll[FnToProduct, L])

  implicit val MyKnownPages = MkPages(
    PostEdit.apply _
    :: HNil
  )

  // TODO: change terminology. Page is misleading. really these are RouteKeys

  // ClassTag is used to provide an api-name but we might need to do better
  def standardViewPageRoute[T]
    (implicit
      ct: ClassTag[T],
      get: Get[T],
      v: View[T],
      mkEditPage: Option[MkPage[Int => EditPage[T]]] = None) = RouterConfigDsl[AnyPage].buildRule { dsl =>

    import dsl._

    val DetailPageComponent = singleRowPageComponent[T](v.view(_, _))

    dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int).caseClass[ViewPage[T]]) ~>
      dynRenderR { (detailPage, router) =>
        val editLink = { () =>
          mkEditPage map { mk =>
            val editPage = mk.apply(detailPage.id)
            router.link(editPage)(MuiIconButton()(Mui.SvgIcons.ImageEdit()()))
          } getOrElse {
            EmptyTag
          }
        }
        val mainProps = MainLayoutProps(router/*, iconElementRight = editLink*/)
        DetailPageComponent((detailPage.id, mainProps))
      }
  }

  def standardEditPageRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    e: Edit[T]) = RouterConfigDsl[AnyPage].buildRule { dsl =>
    import dsl._

    val EditPageComponent = singleRowPageComponent[T]({ case (router, t) => e.element(e.toModel(t)) })

    dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int / "edit").caseClass[EditPage[T]]) ~>
      dynRenderR { (editPage, router) =>
        // TODO: only show backlink if there is a back in the history
        val backLink = { () =>
          <.a(^.href := "javascript:history.back()")(MuiIconButton()(Mui.SvgIcons.NavigationArrowBack()()))
        }
        val mainProps = MainLayoutProps(router, backLink)
        EditPageComponent((editPage.id, mainProps))
      }
  }

  /**
    * ReactComponent for a Page that accepts an Id and uses a Get instance
    * to fetch that Id.
    */
  def singleRowPageComponent[T](fn: (RouterCtl[AnyPage], T) => ReactNode)
  (implicit ct: ClassTag[T], get: Get[T]) =
    ReactComponentB[(Int, MainLayoutProps)]("detailpage")
      .initialState(Option.empty[T])
      .render(P =>
        MainLayout(P.props._2,
          P.state map { t =>
            fn(P.props._2.router, t)
          } getOrElse {
            s"${ct.runtimeClass.getSimpleName} ${P.props._1} not found".asInstanceOf[ReactNode]
          }
        )
      )
      .componentDidMount(P => Callback.future {
        get.get(P.props._1).map(P.setState(_))
      })
      .build

  /**
    * Page-level component for viewing a query
    */
  def queryViewPageComponent[T](initial: T, query: => Future[T])
    (implicit v: View[T]): ReactComponentC.ReqProps[MainLayoutProps, T, Unit, TopNode] = queryViewPageComponent(initial, query, v.view)

  /**
    * Page-level component for viewing a query
    * TODO: abstract this so that it can also be used with Edit[T] or arbitrary F[T]
    */
  def queryViewPageComponent[T](initial: T, query: => Future[T],
    renderFn: (RouterCtl[AnyPage], T) => ReactNode): ReactComponentC.ReqProps[MainLayoutProps, T, Unit, TopNode] =
  ReactComponentB[MainLayoutProps]("viewpage")
    .initialState(initial)
    .render(P => MainLayout(P.props, renderFn(P.props.router, P.state)))
    .componentDidMount(P => Callback.future {
      query.map(P.setState(_))
    })
    .build

  def classListPageRoute[T](getData: => Future[Seq[T]])
  (implicit
    ct: ClassTag[T],
    v: View[Seq[T]]) = {
    // these implicits copied from Dsl.scala should be put somewhere more accessible
    implicit def _auto_routeB_from_str(l: String) = RouteB.literal(l)
    implicit def _auto_route_from_routeB[A, R <% RouteB[A]](r: R) = r.route

    staticListPageRoute("#" / ct.runtimeClass.getSimpleName, ListPage[T](), getData)
  }

  def staticListPageRoute[P <: AnyPage, T](route: Route[Unit], page: P, getData: => Future[Seq[T]])
    (implicit v: View[Seq[T]]) = {
    staticQueryPageRoute(route, page, Nil, getData)
  }

  def staticQueryPageRoute[P <: AnyPage, T](route: Route[Unit], page: P, initial: T, getData: => Future[T])
    (implicit
      v: View[T]): Rule[AnyPage] = RouterConfigDsl[AnyPage].buildRule { dsl =>
    import dsl._

    // break this up into helper functions and lift up to caller
    staticRoute(route, page) ~>
      renderR {
        staticQueryPageRender(initial, getData)
      }
    //dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName).caseClass[ListPage[T]]) ~>
    //  dynRenderR { dynListPageRender({ _ => getData }) }
  }

  def staticQueryPageRender[T](initial: T, getData: => Future[T])
    (implicit
      v: View[T]): RouterCtl[AnyPage] => ReactElement = { router =>
    dynQueryPageRender(initial, { (_: Unit) => getData }).apply((), router)
  }

  def dynQueryPageRender[P, T](initial: T, getData: P => Future[T])
    (implicit
      v: View[T]): (P, RouterCtl[AnyPage]) => ReactElement = { (p: P, router) =>
    val ListPageComponent = queryViewPageComponent[T](initial, getData(p))
    ListPageComponent(MainLayoutProps(router))
  }

}