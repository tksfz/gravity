package gravity

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
}
