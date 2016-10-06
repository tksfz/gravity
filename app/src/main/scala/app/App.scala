package app

import app.models.Account
import view2._
import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui._
import chandu0101.scalajs.react.components.Implicits._

import scala.scalajs.js.JSApp
import japgolly.scalajs.react._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import japgolly.scalajs.react.vdom.prefix_<^._
import shapeless._

object App extends JSApp {

  private[this] def component[T](t: T)(implicit g: View[T]) = {
    val content: ReactNode = g.view(t)
    WithAsyncScript("assets/material_ui-bundle.js") {
      MuiMuiThemeProvider()(
        <.div(
          MuiAppBar(
            title = "Title",
            showMenuIconButton = true
          )(),
          <.div(content)
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
    val comp = component(Account(3, "John Smith", 4))
    ReactDOM.render(comp, dom.document.getElementById("container"))
  }
}