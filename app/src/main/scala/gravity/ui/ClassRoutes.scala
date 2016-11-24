package gravity.ui

import apiclient.Get
import blog.models.Post
import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui.{Mui, MuiAppBar, MuiIconButton, MuiMuiThemeProvider, MuiSvgIcon}
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentC, ReactElement, ReactNode, TopNode}
import japgolly.scalajs.react.extra.router.StaticDsl.{Route, RouteB, Rule}
import japgolly.scalajs.react.extra.router.{Path, RouterConfigDsl, RouterCtl}
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.html
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.{LiftAll, Selector, ZipApply}
import shapeless.{HList, HNil, Poly1}

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

  // ClassTag is used to provide an api-name but we might need to do better
  def standardViewPageRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    v: View[T]) = RouterConfigDsl[AnyPage].buildRule { dsl =>
    import dsl._

    val DetailPageComponent = singleRowPageComponent[T](v.view(_, _))

    dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int).caseClass[ViewPage[T]]) ~>
      dynRenderR { (detailPage, router) =>
        val editLink = { () =>
          router.link(EditPage[T](detailPage.id))(MuiIconButton()(Mui.SvgIcons.ImageEdit()()))
        }
        val mainProps = MainLayoutProps(router/*, iconElementRight = editLink*/)
        DetailPageComponent((detailPage.id, mainProps))
      }
  }

  /**
    * Typeful Patterns
    *
    * Part I: Type classes
    * 0: Type Classes
    * 1. Constraints
    * 2. Multiple Dispatch
    *
    * Part II: Generic Derivation
    *
    * Part III:
    */

  /**
    * What should we call this pattern? Provider pattern
    * type-level map?
    *
    * what is the syntax for the client code that provides all these instances?
    *
    * @tparam P pseudo-page
    */
  trait HasPage[P] {
    type Page

    def apply: Page = page

    def page: Page

    def pathFor(target: P): Path
    def set(target: P): Callback
  }





  // in order to make the implicit machinery generic all of these
  // may nede to extend HasPageWithArgs
  trait HasPage1[P, A] {
    type Page

    def page(a: A): Page
  }

  trait HasPage2[P, A, B] {
    type Page

    def page(a: A, b: B): Page
  }

  trait HasPage3[P, A, B, C] {
    type Page
    def page(a: A, b: B, c: D): Page
  }

  trait EditPageX[T]

  type HasEditPageX[T] = HasPage1[EditPageX[T], Int]

  // this doesn't use P at all. might be OK. P is just for selection.
  trait HasPageWithArgs[P, A <: HList] {
    type Page

    def page(args: A): Page
  }

  // a macro may be suitable or even necessary here
  // to capture the apply parameters on EditPage
  // note this doesn't use the case class aspectse of EditPage at all
  type HasEditPageWithArgs[T] = HasPageWithArgs[EditPage[T], Int :: HNil]

  trait HasEditPage[T] {
    type Page

    def create(id: Int): Page
  }

  trait EditPage2[T]

  type HasEditPage2[T] = HasPageWithArgs[EditPage2[T], Int :: HNil]

  type HasEditPageUsingMacro[T] = HasPageLike[EditPage[T]]

  // the problem with this trait is it doesn't have anything to drive implicit selection
  trait Factory[A <: HList] {
    type Out

    def create(a: A): Out
  }

  //trait FactoryLike[F]

  trait HasEditPage extends Factory[Int :: HNil]

  case class PostEdit(id: Int)

  implicit object PostHasEditPage extends HasEditPageWithArgs[Post] {
    type Page = PostEdit

    // or use fnToProduct
    def page(args: Int :: HNil) = (PostEdit.apply _).tupled.apply(args.tupled)
  }

  // this all seems verbose if a particular page has several such attachment
  // points

  // we could collect multiple P's as P1 :: P2 :: P3, along with args A1 :: A2 :: A3
  // and have a helper HasPages

  // the short syntax is something like:

  // a macro may also be suitable here


  // mapping/function from [T] => (Args => P) and maybe we have P <: T
  // or [T] => (Args => T)
  HasPages { has[EditPage[Post]](PostEdit.apply _), has[ViewPage[Post]], }

  case class ConformingPages[L <: HList](pages: L)

  // unclear what the relationship is between this and HasPageWithArgs
  trait HasConformingPage[L <: HList, P, A] {
    def page(l: L, a: A): P
  }

  object AllMyPages extends Poly1 {
    implicit def postEdit = at[EditPage[Post]] { PostEdit.apply _ }
  }

  // if we do the marker trait thing then we can simplify to:
  implicit val AllMyPages2 = AllPages {
    // this is a function from A => (Page <: P)
    PostEdit.apply _
    :: HNil
    // etc.
  }

  // just use marker interfaces?
  // marker interfaces should definitely be the end-developer interface
  // but it goes through a bunch of machinery to make that work
  // we should also change the terminology because Page is misleading
  // really these are PageRef's or RouteRef's
  trait EditPageY[T]

  implicit val myConformingPages = ???

  // given that we have some definition of HasPages, how do we break it down
  // into individual page conformings

  implicit def hasPageFromHasPages[P, A <: HList]
  (implicit
    ev: HasPages.For[EditPage[Post]]
  )= new HasPageWithArgs[P, A] {

  }

  import shapeless._

  /**
    * this doesn't work for generic case classes with implicit classtag i.e. case class MyEditPage[T : ClassTag]()
    */
  object HasConformingPage {

    // TODO: subtypes of P

    // no instance for hnil
    implicit def instance[H, T <: HList, P, A]
    (implicit
      fnToProduct: FnToProduct.Aux[H, A => P]) = new HasConformingPage[H :: T, P, A] {
      override def page(l: H :: T, a: A): P = {
        fnToProduct.apply(l.head).apply(a)
      }
    }

    // recursive
    implicit def recursiveInstance[H, T <: HList, P, A]
    (implicit
      recurse: HasConformingPage[T, P, A]) = new HasConformingPage[H :: T, P, A] {
      override def page(l: H :: T, a: A): P = recurse.page(l.tail, a)
    }
  }

  type HasEditPage2[T] = HasPage[EditPage[T]]

  trait HasEditPage[A] {
    type Page

    def page(id: Int): Page

    def pathFor(target: EditPage[A]): Path
    def set(target: EditPage[A]): Callback

    final def link(target: EditPage[A]): ReactTagOf[html.Anchor] =
      <.a(^.href := urlFor(target).value, setOnLinkClick(target))
  }

  def standardViewPageRoute[T](editPageRule: HasEditPage[T])
    (implicit
      ct: ClassTag[T],
      get: Get[T],
      v: View[T]) = RouterConfigDsl[AnyPage].buildRule { dsl =>

    editPageRule.path

    import dsl._

    val DetailPageComponent = singleRowPageComponent[T](v.view(_, _))

    editPageRule.link(EditPage[T])

      dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int).caseClass[ViewPage[T]]) ~>
      dynRenderR { (detailPage, router) =>
        val editLink = { () =>
          router.link(EditPage[T](detailPage.id))(MuiIconButton()(Mui.SvgIcons.ImageEdit()()))
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