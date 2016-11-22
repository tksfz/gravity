package blog

import java.time.LocalDate

object models {

  case class Post(
    id: Int,
    title: String,
    author: String,
    date: LocalDate,
    body: String
  )
}
