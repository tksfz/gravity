package app

import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui.{MuiAppBar, MuiMuiThemeProvider}
import chandu0101.scalajs.react.components.Implicits._

import scala.scalajs.js.JSApp
import japgolly.scalajs.react._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

object App extends JSApp {

  private[this] val component =
      WithAsyncScript("assets/material_ui-bundle.js"){
        MuiMuiThemeProvider()(
          MuiAppBar(
            title = "Title",
            showMenuIconButton = true
          )()
        )
      }

  override def main(): Unit = {
    // remove waiting page stuff
    if (!js.isUndefined(g.loadingElement)) {
      g.document.body.removeChild(g.loadingElement)
      //g.loadingElement = js.undefined
      dom.document.body.className.replace("pg-loading", "")
      dom.document.body.className += " pg-loaded"
    }
    AppCSS.load()
    ReactDOM.render(component, dom.document.getElementById("container"))
  }
}