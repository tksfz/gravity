package gravity.ui

import apiclient.Get
import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui.{Mui, MuiAppBar, MuiIconButton, MuiMuiThemeProvider, MuiSvgIcon}
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactElement, ReactNode, TopNode}
import japgolly.scalajs.react.extra.router.StaticDsl.Rule
import japgolly.scalajs.react.extra.router.{RouterConfigDsl, RouterCtl}
import japgolly.scalajs.react.vdom.prefix_<^._

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
        DetailPageComponent(SingleRowPageProps(router, detailPage.id, iconElementRight = editLink))
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
        EditPageComponent(SingleRowPageProps(router, editPage.id, iconElementLeft = backLink))
      }
  }

  case class SingleRowPageProps(router: RouterCtl[AnyPage], id: Int,
    iconElementLeft: () => js.UndefOr[ReactElement] = { () => js.undefined },
    iconElementRight: () => js.UndefOr[ReactElement] = { () => js.undefined })

  /**
    * ReactComponent for a Page that accepts an Id and uses a Get instance
    * to fetch that Id.
    */
  def singleRowPageComponent[T](fn: (RouterCtl[AnyPage], T) => ReactNode)
  (implicit ct: ClassTag[T], get: Get[T]) =
    ReactComponentB[SingleRowPageProps]("detailpage")
      .initialState(Option.empty[T])
      .render(P =>
        MainLayout(P.props.iconElementLeft(), P.props.iconElementRight()) {
          P.state map { t =>
            fn(P.props.router, t)
          } getOrElse {
            s"${ct.runtimeClass.getSimpleName} ${P.props.id} not found".asInstanceOf[ReactNode]
          }
        }
      )
      .componentDidMount(P => Callback.future {
        get.get(P.props.id).map(P.setState(_))
      })
      .build

}