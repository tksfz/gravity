package app

import gravity._
import gravity.ui._
import gravity.ui.View._
import gravity.ui.Edit._
import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui._
import chandu0101.scalajs.react.components.Implicits._
import gravity.models.{ObjectId, OneId, Phone}

import scala.scalajs.js.JSApp
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Resolution, Router, RouterConfigDsl, RouterCtl}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless._
import models._
import shapeless.record._
import shapeless.syntax.singleton._

object App extends JSApp {

  import EnableRelaxedImplicits._

  case object Home extends AnyPage
  case object RouteNotFound extends AnyPage

  val homePage = ReactComponentB[Unit]("home")
    .render(P =>
      <.div("Welcome Home")
    )
    .build


  def layout(c: RouterCtl[AnyPage], r: Resolution[AnyPage]) = {
    WithAsyncScript("assets/material_ui-bundle.js") {
      MuiMuiThemeProvider()(
        <.div(
          MuiAppBar(
            title = "Title",
            showMenuIconButton = true
          )(),
          <.div(r.render())
        )
      )
    }
  }

  // top level schema with
  // Many[Account] :: Many[Contact]
  // or case class Schema(accounts: Many[Account], contacts: Many[Contact]) etc.
  override def main(): Unit = {
    // remove waiting page stuff
    if (!js.isUndefined(g.loadingElement)) {
      g.document.body.removeChild(g.loadingElement)
      //g.loadingElement = js.undefined
      dom.document.body.className.replace("pg-loading", "")
      dom.document.body.className += " pg-loaded"
    }
    AppCSS.load()
    //val comp = component(Account(3, "John Smith", 4))
    //val comp = component(Contact("Mary", "Johnson"))
    val user = User(ObjectId(1), "harry@potter.com", "hpotter", "Harry Potter")
    val contact = Contact(ObjectId(1), OneId(ObjectId(1)), "Washington", Some("Mary"), title = Some("Senior Engineer"), mobilePhone = Some(Phone("(415) 555-2121")))
    val generic = LabelledGeneric[Contact].to(contact).merge(('fullName ->> defFullName) :: HNil)
    val generic2 = Tagger[Contact].apply(generic)

    val routerConfig = RouterConfigDsl[AnyPage].buildConfig { dsl =>
      import dsl._
      (emptyRule
        | staticRoute(root, Home) ~> render(homePage())
        | staticRoute("#/noroute", RouteNotFound) ~> render(<.div("route not found"))
        | ClassRoutes.standardRoutes[Contact].routes(dsl)
        ).notFound(redirectToPage(RouteNotFound)(Redirect.Replace))
    }
      .renderWith(layout)

    val router = Router(BaseUrl.fromWindowOrigin_/, routerConfig)

    ReactDOM.render(router(), dom.document.getElementById("container"))
  }
}