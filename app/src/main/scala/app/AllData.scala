package app

import app.models.Contact
import gravity.models.{ObjectId, OneId, Phone}
import gravity.ui.apiclient.Get
import shapeless.{HNil, Poly0}

import scala.concurrent.Future

object AllData extends Poly0 {
  implicit def cont = at[Option[Contact]] {
    Some(Contact(ObjectId(1), OneId(ObjectId(1)), "Washington", Some("Mary"), title = Some("Senior Engineer"), mobilePhone = Some(Phone("(415) 555-2121"))))
  }

  implicit def default[T] = at[Option[T]] { None }


  implicit def toGet[T]
  (implicit
    allData: AllData.Case0[Option[T]]) = new Get[T] {
    override def get(id: Int): Future[Option[T]] = {
      Future.successful {
        if (id < 100) {
          allData.value(HNil)
        } else {
          None
        }
      }
    }
  }
}