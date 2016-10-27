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

  private[this] def component[T](t: T)(implicit g: View[T]) = {
    val content: ReactNode = g.view(t) // g.element(g.toModel(t))
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
    //val comp = component(Account(3, "John Smith", 4))
    //val comp = component(Contact("Mary", "Johnson"))
    val user = User(ObjectId(1), "harry@potter.com", "hpotter", "Harry Potter")
    val contact = Contact(ObjectId(1), OneId(ObjectId(1)), "Washington", Some("Mary"), title = Some("Senior Engineer"), mobilePhone = Some(Phone("(415) 555-2121")))
    val generic = LabelledGeneric[Contact].to(contact).merge(('fullName ->> defFullName) :: HNil)
    val generic2 = Tagger[Contact].apply(generic)
    val comp = component(contact)
    ReactDOM.render(comp, dom.document.getElementById("container"))
  }
}