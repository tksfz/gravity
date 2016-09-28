package app

import chandu0101.scalajs.react.components._
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.mutable.GlobalRegistry

object AppCSS {

  def load() = {
    GlobalRegistry.register(
      ReactTable.DefaultStyle,
      ReactListView.DefaultStyle,
      ReactSearchBox.DefaultStyle,
      Pager.DefaultStyle,
      ReactDraggable.Style)
    GlobalRegistry.addToDocumentOnRegistration()
  }
}
