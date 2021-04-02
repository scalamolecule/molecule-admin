package moleculeadmin.client.app.logic.schema
import moleculeadmin.shared.ast.metaSchema.MetaSchema
import rx.Var


object SchemaState {

//  implicit val ctx = rx.Ctx.Owner.safe()

  //  // Schema
  //  implicit var nsMap: Map[String, Ns] = Map.empty[String, Ns]

  // Container for nested schema representation
  val schema = Var[MetaSchema](MetaSchema(Nil))

  // part - nss - active attr
  // Keeps track of
  // - expanded elements in tree menu
  // - visible elements in body
  val open = Var[Seq[(String, Seq[String], Option[String])]](Nil)

  // Level: 0: partitions, 1: nss, 2: attribute
  val level = Var[Int](0)

  // Current partition
  val curPart = Var[Seq[String]](Nil)

  // What is being being processed now? Concatenates part/ns/attr/pos-number as a coordinate in the tree
  val processing = Var[String]("")
}
