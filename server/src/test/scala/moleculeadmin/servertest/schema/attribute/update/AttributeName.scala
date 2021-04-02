package moleculeadmin.servertest.schema.attribute.update

import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeName extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Name") {
      val ps = new PartitionSetup
      import ps._

      // Rename attribute
      val schema1 = updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb7", 1, "Int").getOrElse(MetaSchema(Nil))
      schema1 ==> MetaSchema(List(
        MetaPart(1, "a", None, None, List(
          MetaNs(1, "Aa", "a_Aa", None, None, List()))),
        MetaPart(2, "b", None, None, List(
          MetaNs(1, "Bb", "b_Bb", None, None, List(
            MetaAttr(1, "bb7", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
            MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
          MetaNs(2, "Bc", "b_Bc", None, None, List(
            MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
        MetaPart(3, "c", None, None, List())
      ))


      // Create new attribute with previous name of renamed attribute.
      // This can be a two-step way of changing a value type of an attribute (history will be gone though)
      createAttribute(schema1, "Partition", "b", "Bb", "bb1", 1, "String") ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb7", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(3, "bb1", 1, "String", Nil, None, Seq("indexed"), None, None, None, None, None, List())
            )),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))))),
          MetaPart(3, "c", None, None, List())
        ))
      )
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}