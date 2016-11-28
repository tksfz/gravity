package blog

import app.App.{Home, RouteNotFound}
import app.AppCSS
import blog.models._
import gravity.ui.{AnyPage, ClassRoutes}
import japgolly.scalajs.react.{ReactComponentB, ReactDOM}
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Router, RouterConfigDsl}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.Dynamic._
import scala.scalajs.js.JSApp
import scala.scalajs.js.Dynamic.{global => g}


/**
  * Created by thom on 11/18/16.
  */
object App extends JSApp {

  val homePage = ReactComponentB[Unit]("home")
    .render(P => <.div("Welcome Home"))
    .build

  override def main(): Unit = {
    // remove waiting page stuff
    if (!js.isUndefined(g.loadingElement)) {
      g.document.body.removeChild(g.loadingElement)
      //g.loadingElement = js.undefined
      dom.document.body.className.replace("pg-loading", "")
      dom.document.body.className += " pg-loaded"
    }
    AppCSS.load()

    val routerConfig = RouterConfigDsl[AnyPage].buildConfig { dsl =>
      import dsl._
      (emptyRule
        | staticRoute(root, Home) ~> render(homePage())
        | staticRoute("#/noroute", RouteNotFound) ~> render(<.div("no matching route for request"))
        | ClassRoutes[Post]
        ).notFound(redirectToPage(RouteNotFound)(Redirect.Replace))
    }

    val router = Router(BaseUrl.fromWindowOrigin_/, routerConfig)

    ReactDOM.render(router(), dom.document.getElementById("container"))
  }


}
