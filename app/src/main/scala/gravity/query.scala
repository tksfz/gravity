package gravity

import gravity.derive.Derive
import gravity.ui.View
import japgolly.scalajs.react.ReactNode
import shapeless._
import shapeless.record._
import shapeless.tag._

import scala.scalajs.js.UndefOr

object query {


  /**
    * Query construction simultaneously derives a record type.  That record type can then drive UI generation.
    *
    * But then the query's record type is stuffed into a val.  We can drive UI generation from the val's
    * record type.  But can we customize based on the val?
    *
    * Typically customizations are triggered by the type of the case class.  But the record type of the val
    * is tagged with the original case classes, which is good because we inherit default annotations.  But is it
    * sufficient?
    *
    * Would it work to have implicits declared near the val that then override the inherited implicits?
    */


  // what do we do for derived field fullName? updateWithAppend operator
  case class Contact(id: String, firstName: String, lastName: String)

  val relatedContacts = Derive[Contact].apply.apply {
    _.remove('firstName)
    //_.select('id, 'lastName)
  }

  // ah so you can tag things with a path-dependent type
  tag[relatedContacts.Out](relatedContacts)

  // can we define implicits for a path-dependent type? yes we can
  implicit object RelatedContactsView extends View[relatedContacts.Out] {
    override def view(t: UndefOr[relatedContacts.Out]): ReactNode = ???
  }

}