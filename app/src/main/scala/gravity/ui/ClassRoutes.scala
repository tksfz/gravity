package gravity.ui

import apiclient.Get
import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui.{MuiAppBar, MuiMuiThemeProvider}
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactNode}
import japgolly.scalajs.react.extra.router.StaticDsl.Rule
import japgolly.scalajs.react.extra.router.{RouterConfigDsl, RouterCtl}
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.reflect.ClassTag

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

  /**
    * Not sure if T : ClassTag will be adequate here:
    * - Note that routes are two way, so I believe that given an instance of Detail we should be able to map back to
    * the path. So we need to distinguish Detail[Account] vs Detail[Contact].
    * - I'm not sure if the ClassTag will work for doing that, but we'll have to try and see.
    */
  case class Detail[T : ClassTag](id: Int) extends AnyPage
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
    ct: ClassTag[T]) = new ClassRoutes[T] {
    override def routes = viewRoute | editRoute
  }

  // ClassTag is used to provide an api-name but we might need to do better
  def viewRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    v: View[T]) = RouterConfigDsl[AnyPage].buildRule {
    dsl =>
      import dsl._
      val DetailPageComponent = mkGetBasedPageComponent[T](v.view(_, _))

      dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int).caseClass[Detail[T]]) ~>
        dynRenderR((x: Detail[T], router) => DetailPageComponent((router, x.id)))
  }

  def editRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    e: Edit[T]) = RouterConfigDsl[AnyPage].buildRule {
    dsl =>
      import dsl._
      val EditPageComponent = mkGetBasedPageComponent[T]({ case (router, t) => e.element(e.toModel(t)) })
      dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int / "edit").caseClass[EditPage[T]]) ~>
        dynRenderR((x: EditPage[T], router) => EditPageComponent((router, x.id)))
  }

  /**
    * ReactComponent for a Page that accepts an Id and uses a Get instance
    * to fetch that Id.
    */
  def mkGetBasedPageComponent[T](fn: (RouterCtl[AnyPage], T) => ReactNode)
  (implicit ct: ClassTag[T], get: Get[T]) =
    ReactComponentB[(RouterCtl[AnyPage], Int)]("detailpage")
      .initialState(Option.empty[T])
      .render(P =>
        MainLayout {
          P.state map { t =>
            fn(P.props._1, t)
          } getOrElse {
            s"${ct.runtimeClass.getSimpleName} ${P.props._2} not found"
          }
        })
      .componentDidMount(P => Callback.future {
        get.get(P.props._2).map(P.setState(_))
      })
      .build

  import chandu0101.scalajs.react.components.Implicits._

  val MainLayout =
    ReactComponentB[Unit]("layout")
      .render(P =>
        WithAsyncScript("assets/material_ui-bundle.js") {
          MuiMuiThemeProvider()(
            <.div(
              MuiAppBar(
                title = "Title",
                showMenuIconButton = true
              )(),
              P.propsChildren
            )
          )
        }
      )
      .build

}