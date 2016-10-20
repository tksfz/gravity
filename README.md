

Gravity
=======

Gravity is a Scala library for enterprise app development. It makes extensive use of
typeclasses[1] and generic derivation[2]. This allows apps to be built using a "metadata-driven"[3]
approach: Start by defining your data models as Scala case classes. Automatically
generate functionality (user interfaces, persistence, API's, reporting) directly from these
case class declarations. Then customize that functionality through additional metadata
associated with your case classes via implicits[4].

For example, you could define a Contact class as follows:

```
case class Contact(
  id: ObjectId,
  firstName: String,
  lastName: String,
  age: Option[Int],
  lastContactDate: Option[Date]
)
```

_From this definition alone_, you would automatically get a user interface for creating and
browsing contacts, creating reports, persistence to Mongo, and a Web service API. Furthermore,
you can customize functionality through additional metadata, declared as implicits:

```
implicit val contactUi = Ui[Contact](
  lastContactDate = required & hidden &
)
```

Customizations can range from simple tweaks to completely novel re-implementations.
