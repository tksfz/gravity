package gravity.ui

import apiclient.Get
import japgolly.scalajs.react.{Callback, ReactComponentB}
import japgolly.scalajs.react.extra.router.StaticDsl.Rule
import japgolly.scalajs.react.extra.router.RouterConfigDsl
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

  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  import japgolly.scalajs.react.vdom.Implicits._

  // TODO: use optional implicits
  implicit def standardRoutes[T]
  (implicit
    v: View[T],
    get: Get[T],
    ct: ClassTag[T]) = new ClassRoutes[T] {
    override def routes = viewRoute
  }

  // ClassTag is used to provide an api-name but we might need to do better
  def viewRoute[T]
  (implicit
    ct: ClassTag[T],
    get: Get[T],
    v: View[T]) = RouterConfigDsl[AnyPage].buildRule {
    dsl =>
      val DetailPage = ReactComponentB[Int]("detailpage")
        .initialState(Option.empty[T])
        .render(P => P.state map { t =>
            <.div(v.view(t))
          } getOrElse {
            <.div(s"${ct.runtimeClass.getSimpleName} ${P.props} not found")
          } render
        )
        .componentDidMount(P => Callback.future {
          get.get(P.props).map(P.setState(_))
        }
        )
        .build

      import dsl._

      dynamicRouteCT(("#/" ~ ct.runtimeClass.getSimpleName ~ "/" ~ int).caseClass[Detail[T]]) ~>
        dynRender((x: Detail[T]) => DetailPage(x.id))
  }

}