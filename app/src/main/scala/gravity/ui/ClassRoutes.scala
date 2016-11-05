package gravity.ui

import apiclient.Get
import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui.{Mui, MuiAppBar, MuiMuiThemeProvider, MuiSvgIcon}
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactElement, ReactNode}
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

  import chandu0101.scalajs.react.components.Implicits._

  // ClassTag is used to provide an api-name but we might need to do better
  def viewRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    v: View[T]) = RouterConfigDsl[AnyPage].buildRule {
    dsl =>
      import dsl._
      val DetailPageComponent = singleRowPageComponent[T](v.view(_, _))

      dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int).caseClass[Detail[T]]) ~>
        dynRenderR { (detailPage, router) =>
          val editLink = { () => router.link(EditPage[T](detailPage.id))(Mui.SvgIcons.ImageEdit()()).asInstanceOf[js.UndefOr[ReactElement]] }
          DetailPageComponent(SingleRowPageProps(router, detailPage.id, editLink))
        }
  }

  def editRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    e: Edit[T]) = RouterConfigDsl[AnyPage].buildRule {
    dsl =>
      import dsl._
      val EditPageComponent = singleRowPageComponent[T]({ case (router, t) => e.element(e.toModel(t)) })
      dynamicRouteCT(("#" / ct.runtimeClass.getSimpleName / int / "edit").caseClass[EditPage[T]]) ~>
        dynRenderR((x: EditPage[T], router) => EditPageComponent(SingleRowPageProps(router, x.id)))
  }

  case class SingleRowPageProps(router: RouterCtl[AnyPage], id: Int,
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
        MainLayout(
          Some(P.props.iconElementRight),
          P.state map { t =>
            fn(P.props.router, t)
          } getOrElse {
            s"${ct.runtimeClass.getSimpleName} ${P.props.id} not found".asInstanceOf[ReactNode]
          }
        )
      )
      .componentDidMount(P => Callback.future {
        get.get(P.props.id).map(P.setState(_))
      })
      .build

  /**
    * the Prop has to be lazy. otherwise if you reference Mui elements you get undefined errors
    * I think because the Mui global isn't defined until you get inside this component
    * TODO: make this a def and consider moving the lazy icon element to an argument
    */
  val MainLayout =
    ReactComponentB[() => js.UndefOr[ReactElement]]("layout")
      .render(P =>
        WithAsyncScript("assets/material_ui-bundle.js") {
          MuiMuiThemeProvider()(
            <.div(
              MuiAppBar(
                title = "Title",
                showMenuIconButton = true,
                iconElementRight = P.props()
              )(),
              P.propsChildren
            )
          )
        }
      )
      .build
      .withDefaultProps({() => js.undefined})

}