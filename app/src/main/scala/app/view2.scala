import app.HasSameKeys
import app.models.Account
import chandu0101.scalajs.react.components.materialui.{MuiTable, MuiTableBody, MuiTableRow, MuiTableRowColumn}
import japgolly.scalajs.react.{ReactComponentB, ReactElement, ReactNode}
import shapeless.PolyDefns.~>
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{LiftAll, Mapped, Mapper, ToTraversable, ZipConst}
import shapeless.syntax.singleton._
import shapeless.tag.@@

object view2 {

  // Note that our treat of context types C
  // can leave them not tightly bound to the records
  // such that we could support placeholder (empty) types C
  // that are just used as containers for record keys
  // to guide implicit selection to appropriate metadata for that record

  trait View[T] {
    def view(t: T): ReactNode
    // def shortView for links to foreign keys
  }

  // Do we want C here?
  /**
    * Labels for a field of a case class C are tagged as `F @@ C`.  Is that a reliable way of doing things?
    * What if the fields get splatted into a hlist.  Maybe we can keep information about C with them
    * everywhere?
    *
    * The downside of this approach is that records are by design divorced from C.
    * We no longer have a record that's.. hmm
    *
    * @tparam T
    */
  trait Label[T] {
    def label: String
  }

  // where is fieldview used
  trait FieldView[C, K, V] {
    def view(v: FieldType[K, V]): (ReactNode, ReactNode)
  }

  // come up with a better name than Label
  trait Header[T] {
    def header: ReactNode
  }

  // whether we really want to break this out into its own typeclass is questionable
  // it's really just a detail of how makeTableView is implemented
  trait FieldView2[T] {
    def view(t: T): (ReactNode, ReactNode)
  }

  // Depending on whether we have Label[C, T] or Label[T] we may or may not
  // need [C, T] here
  implicit def viewFromLabel[C, T](implicit label: Label[T]) = new View[T] {
    def view(t: T): ReactNode = label.label
  }

  implicit def headerFromLabel[T](implicit label: Label[T]) = new Header[T] {
    override def header: ReactNode = label.label
  }

  implicit def fieldView2[T](implicit header: Header[T], v: View[T]) = new FieldView2[T] {
    override def view(t: T): (ReactNode, ReactNode) = (header.header, v.view(t))
  }

  // should we tag V @@ C?
  implicit def fieldView[C, K, V]
  (implicit
    k: Witness.Aux[K],
    kview: View[K @@ C], // this is important
    vview: View[V]) = new FieldView[C, K, V] {
    override def view(v: FieldType[K, V]): (ReactNode, ReactNode) = {
      (kview.view(tag[C](k.value)), vview.view(v))
    }
  }

  case class LabelsData[T, L <: HList, R <: HList](labels: R)
    (implicit
      l: LabelledGeneric.Aux[T, L],
      hasSameKeys: HasSameKeys[L, R]) {
  }

  object LabelsData {
    class X[T] {
      def apply[L <: HList, R <: HList](labels: R)
        (implicit
          l: LabelledGeneric.Aux[T, L],
          hasSameKeys: HasSameKeys[L, R]) = {
        LabelsData[T, L, R](labels)
      }
    }
    def apply[T] = new X[T]
  }

  implicit def fromLabelsData[T, L <: HList, F, R <: HList]
  (implicit
    l: LabelledGeneric.Aux[T, L],
    //fInL: ops.record.Selector.Aux[L, F, _],
    data: LabelsData[T, L, R],
    fInRToo: ops.record.Selector.Aux[R, F, String]) = new Label[F @@ T] {
    def label = fInRToo.apply(data.labels)
  }

  implicit val mylabelsData = LabelsData[Account].apply(
    ('id ->> "Id") ::
      ('name ->> "Name") ::
      ('numEmployees ->> "Number of employees") ::
      HNil
  )

  implicit def fromLabelsData2[T, L <: HList, F, V, R <: HList]
  (implicit
    l: LabelledGeneric.Aux[T, L],
    fInL: ops.record.Selector.Aux[L, F, V],
    data: LabelsData[T, L, R],
    fInRToo: ops.record.Selector.Aux[R, F, String]) = new Label[FieldType[F, V] @@ T] {
    def label = fInRToo.apply(data.labels)
  }

  // TODO: we should probably be creating components that accept models
  // and somewhere higher-up the model gets passed in
  // i.e. returning ReactComponent here instead of ReactElement
  implicit def makeTableView[T, L <: HList, TL <: HList, O <: HList]
  (implicit
    l: LabelledGeneric.Aux[T, L],
    tagger: Tagger.Aux[T, L, TL],
    mapper: Mapper.Aux[lift.type, TL, O],
    trav: ToTraversable.Aux[O, List, (ReactNode, ReactNode)]) = new View[T] {
    def view(t: T): Either[String, ReactElement] = {
      val fieldElems: List[(ReactNode, ReactNode)] =
        (tagger.apply(l.to(t)) map lift).toList
      val mui =
        ReactComponentB[Unit]("blah")
          .render(_ =>
            MuiTable()(
              MuiTableBody(displayRowCheckbox = false)(
                fieldElems map { case (l, r) =>
                  MuiTableRow()(
                    l match {
                      case Left(s) => MuiTableRowColumn()(s)
                      case Right(e) => MuiTableRowColumn()(e)
                    },
                    r match {
                      case Left(s) => MuiTableRowColumn()(s)
                      case Right(e) => MuiTableRowColumn()(e)
                    }
                  )
                }
              )
            )
          )
          .build
      Right(mui())
    }
  }

  object lift extends Poly1 {
    implicit def asdf[K, V, C]
    (implicit fv: FieldView[C, K, V]) = at[FieldType[K, V] @@ C] { fv.view(_) }

    // OR
    // and rename Label to DisplayLabe
    // make Label[T] actually return a ReactNode?
    // LabelView
    // but often we need labels without values
    // so in those cases we need
    // but even in those cases we can have the type of the value
    // and make Label[T] usually take values as its parameter? e.g. FieldType[K, V] @@ C?
    implicit def blah[K, V, C]
    (implicit
      witness: Witness.Aux[K],
      kview: View[K @@ C],
      vview: View[FieldType[K, V] @@ C] // instead of View[V] we could do View[FieldType[K, V] @@ C] // note that FieldType[K, V] @@ C extends V
      // so the View[FieldType[K, V] @@ C] might actually be able to pick up a View[V] instance
      // maybe ineed a + on View[+V]?
    ) = at[FieldType[K, V] @@ C] { v =>
      (kview.view(tag[C](witness.value)), vview.view(v))
    }

    implicit def blah2[T]
    (implicit
      header: Header[T], view: View[T]) = at[T] { t =>
      (header.header, view.view(t))
    }

  }

  case class Labels[C, R <: HList](r: R)

  import shapeless.tag._

  type TaggedWithX[X] = { type Lambda[T] = T @@ X }

  // we can produce FieldType[K, V] @@ C
  // or maybe even FieldType[K @@ C, V]?
  /*
  private[this] def tagFields[C, L <: HList, R <: HList](l: L)
  (implicit
    gen: LabelledGeneric.Aux[C, L],
    //mapped: Mapped.Aux[L, TaggedWithX[C]#Lambda, R],
    zip: ZipConst.Aux[C, L, R],
    mapper: Mapper.Aux[tagger.type, L, R]): R = {
    (l zipConst null.asInstanceOf[C]) map tagger
  } */

  object tagger extends Poly2 {
    implicit def asdf[T, C] = at[T, C] { (x, _) => tag[C](x) }
  }

  private[this] def tagFields2[C, L <: HList, R <: HList](l: L)
    (implicit
      gen: LabelledGeneric.Aux[C, L],
      tagger: Tagger.Aux[C, L, R]): R = {
    //(l zipConst null.asInstanceOf[C]) map tagger
    tagger.apply(l)
  }

  /**
    * Type class supporting mapping a higher ranked function over this `HList`.
    *
    * @author Miles Sabin
    */
  trait Tagger[T, In <: HList] extends DepFn1[In] with Serializable { type Out <: HList }

  object Tagger {
    def apply[T, L <: HList](implicit tagger: Tagger[T, L]): Aux[T, L, tagger.Out] = tagger

    type Aux[T, In <: HList, Out0 <: HList] = Tagger[T, In] { type Out = Out0 }

    implicit def hnilMapper1[T]: Aux[T, HNil, HNil] =
      new Tagger[T, HNil] {
        type Out = HNil
        def apply(l : HNil): Out = HNil
      }

    implicit def hlistMapper1[T, InH, InT <: HList]
    (implicit mt : Tagger[T, InT]): Aux[T, InH :: InT, (InH @@ T) :: mt.Out] =
      new Tagger[T, InH :: InT] {
        type Out = (InH @@ T) :: mt.Out
        def apply(l : InH :: InT): Out = tag[T](l.head) :: mt(l.tail)
      }
  }

}