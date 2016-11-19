package gravity

import chandu0101.scalajs.react.components.WithAsyncScript
import chandu0101.scalajs.react.components.materialui.{MuiAppBar, MuiMuiThemeProvider}
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.{ReactComponentB, ReactElement}
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js

package object ui {

  /**
    * Enables "relaxed" typeclass instances across the UI that display
    * placeholder values for types (fields) that are otherwise missing
    * necessary typeclass instances.
    */
  trait RelaxedImplicits

  /**
    * Import gravity.ui.EnableRelxaedImplicits._ to enable relaxed
    * implicits.
    */
  object EnableRelaxedImplicits {
    implicit val relaxedImplicits = new RelaxedImplicits { }
  }

  import chandu0101.scalajs.react.components.Implicits._

  case class MainLayoutProps(
    router: RouterCtl[AnyPage],
    iconElementLeft: () => js.UndefOr[ReactElement] = { () => js.undefined },
    iconElementRight: () => js.UndefOr[ReactElement] = { () => js.undefined })

  /**
    * the Prop has to be lazy. otherwise if you reference Mui elements you get undefined errors
    * I think because the Mui global isn't defined until you get inside this component
    * TODO: consider moving to a super-trait Layouts.scala or some other file
    */
  val MainLayout =
    ReactComponentB[MainLayoutProps]("layout")
      .render(P =>
        WithAsyncScript("assets/material_ui-bundle.js") {
          MuiMuiThemeProvider()(
            <.div(
              MuiAppBar(
                title = "Title",
                showMenuIconButton = true,
                iconElementLeft = P.props.iconElementLeft(),
                iconElementRight = P.props.iconElementRight()
              )(),
              P.propsChildren
            )
          )
        }
      )
      .build


}
