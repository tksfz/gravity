import app.models.Account
import app.{HasSameKeys, ZipByKey}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.Frag
import shapeless._
import shapeless.labelled._

object ctd {

  // An attempt at a highly generic abstraction for a wide variety of metadata

  /**
    * In the context of phantom type C, for a value of type T, fetch some data of type D.
    * Typically, C will be a class; T will be a field label, value or field-value; and D can be a rendering or
    * some metadata.
    *
    * Sometimes T ranges over an ordinary set of values (e.g. T = Int).  Other times, T is a singleton type.
    */
  trait ClassTargetData[C, T, D] {
    def get(t: T): D
  }

  trait Labels[C, S <: Symbol] extends ClassTargetData[C, S, String]

  trait View[C, T] extends ClassTargetData[C, T, ReactNode] {
    // of course here we could have a asString method etc.
  }

  trait FieldView[C, K, V] extends ClassTargetData[C, FieldType[K, V], (ReactNode, ReactNode)]


  implicit def viewString[C] = new View[C, String] {
    override def get(t: String): ReactNode = {
      t
    }
  }

  /**
    * We can get a View for any [C, K] for which we have labels.
    */
  implicit def symbolView[C, K <: Symbol](implicit labels: Labels[C, K]) = new View[C, K] {
    override def get(t: K): ReactNode = {
      labels.get(t) // implicit conversion from String => ReactNode
    }
  }

  implicit def viewField[C, K, V]
    (implicit
      witness:  Witness.Aux[K],
      kview: View[C, K],
      vview: View[C, FieldType[K, V]]) = new FieldView[C, K, V] {
    override def get(t: FieldType[K, V]): (ReactNode, ReactNode) = {
      (kview.get(witness.value), vview.get(t))
    }
  }


  // case class Labels

  // implicits from mapping over a Poly2

  import shapeless.record._

  case class Metadata[C, R <: HList](data: R)

  // derive CTD's from Metadata
  // but then the other traits have to be type aliases
  // other traits will just have to duplicate some of this
  // this pattern only works when R is not dependent on D
  // for example what if for Booleans you want two labels (for radio buttons)
  implicit def ctdFromMetadata[C, K, D, R <: HList]
    (implicit
      md: Metadata[C, R],
      select: ops.record.Selector.Aux[R, K, D]
    ) = new ClassTargetData[C, K, D] {

    override def get(t: K): D = {
      select.apply(md.data)
    }
  }


  implicit def labelsFromMetadata[C, S <: Symbol, R <: HList]
    (implicit
      md: Metadata[C, R],
      select: ops.record.Selector.Aux[R, S, String]
    ) = new Labels[C, S] {
    override def get(t: S): String = {
      select.apply(md.data)
    }
  }

  // Metadata is labelled with an object type, indicating its purpose
  object labels
  case class MetadataFor[C, G, R <: HList](data: R)


  // instead of ZipByKey how about FieldTo

  // Metadata is labelled with an object type and that object type
  // specifies constraints of some kind over the keys and values of R
  // does ToTraversable work on a record?
  case class MetadataFor2[C, L, G, R <: HList](data: R)
    (implicit
      l: LabelledGeneric.Aux[C, L],
      sameKeys: ZipByKey.Aux[L, R, _],
      isStrings: ops.record.ToMap.Aux[R, Symbol, String]
    )

  //trait Metadata3[G, C]

  // can a subtype act as the instance of Metadata3?
  case class MetadataFor3[G, C, L, R <: HList](data: R)
    (implicit
      l: LabelledGeneric.Aux[C, L],
      constraint: Happy[G, L, R]
    ) //extends Metadata3[G, C]

  class Curry[G, C] {
    def apply[L, R <: HList](data: R) = MetadataFor3[G, C, L, R](data)
  }

  def metadataFor[G, C] = new Curry[G, C]

  import shapeless.syntax.singleton._

  trait Happy[G, L, R]

  implicit def happyLabels[L, R]
    (implicit
      sameKeys: ZipByKey.Aux[L, R, _],
      isStrings: ops.record.ToMap.Aux[R, Symbol, String]) = new Happy[labels.type, L, R] { }

  implicit def labelsFromMetadataFor3[C, L <: HList, S <: Symbol, R <: HList]
  (implicit
    md: MetadataFor3[labels.type, C, L, R],
    s: ops.record.Selector.Aux[R, S, String]
  ) = new Labels[C, S] {
    override def get(t: S): String = {
      s.apply(md.data)
    }
  }

  def labelsFromRecord[C, S <: Symbol, L <: HList, R <: HList](data: R)
    (implicit s: ops.record.Selector.Aux[R, S, String]) = new Labels[C, S] {
    override def get(t: S): String = s.apply(data)
  }

  object labelsFromRecord3 {
    def apply[C, S <: Symbol] = new Curry2[C, S]
    class Curry2[C, S <: Symbol] {
      def apply[R <: HList](data: R)(implicit s: ops.record.Selector.Aux[R, S, String]) = new Labels[C, S] {
        override def get(t: S): String = s.apply(data)
      }
    }
  }

  implicit def accountLabels[S <: Symbol] = labelsFromRecord3[Account, S] {
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") :: HNil

  }

  implicit val accountLabels = metadataFor[labels.type, Account] {
    ('id ->> "Id") :: HNil
  }


  // Abstract over different kinds of metadata M
  // Doesn't really help - doesn't reduce code
  /*
  class Builder2[M[_, _] : Build] {
    def apply[C, K, D, R <: HList]
      (implicit
        md: Metadata[C, D, R],
        select: ops.record.Selector.Aux[R, K, D]) = {
      implicitly[Build[D, M[_, _]]].create { k: K =>
        select.apply(md.data)
      }
    //new M[C, K, D] {
      //override def get(t: K): D = {
        //select.apply(md.data)
      //}
    }
  }

  trait Build[D, M[X, Y] <: ClassTargetData[X, Y, D]] {
    def create[A, B](fn: B => D): M[A, B]
  }

  implicit object LabelsBuild extends Build[String, Labels] {
    override def create[A, B](fn: (B) => String): Labels[A, B] = new Labels[A, B] {
      override def get(t: B): String = fn(t)
    }
  }*/

}