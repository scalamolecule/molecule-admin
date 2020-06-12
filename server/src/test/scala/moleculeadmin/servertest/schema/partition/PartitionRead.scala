package moleculeadmin.servertest.schema.partition

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.testdata.TreeSchema
import sbtmolecule.Ast.{Definition, Namespace, Optional}
import sbtmolecule.{Ast, DefinitionParser}
import utest._


object PartitionRead extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Read") {
      val ps = new PartitionSetup
      import ps._

      // def file
      read ! partitionDefFilePath ==> partitionDefFile
      DefinitionParser("PartitionDefinition.scala", partitionDefFile.split("\n").toList).parse ==>
        Definition("db.migration", 0, 5, "Partition", "c", "", List(
          Namespace("a", None, "a_Aa", None, None, List()),
          Namespace("b", None, "b_Bb", None, None, List(
            Ast.Val("bb1", "bb1", "OneInt", "Int", "", "long", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None),
            Ast.Val("bb2", "bb2", "OneInt", "Int", "", "long", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)
          )),
          Namespace("b", None, "b_Bc", None, None, List(
            Ast.Val("bc1", "bc1", "OneInt", "Int", "", "long", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)
          )),
          Namespace("c", None, "", None, None, Nil)))


      // meta schema
      meta_Db.name_("Partition").Partitions.pos.name.get(moleculeAdminConn).sortBy(_._1) ==> List(
        (1, "a"),
        (2, "b"),
        (3, "c"),
      )

      // Check that nested meta schema is correct
      getMetaSchema("Partition") ==> partitionMetaSchema

      // live db
      Schema.part_("b").a.get(partitionConn).sorted ==> List(
        ":b_Bb/bb1",
        ":b_Bb/bb2",
        ":b_Bc/bc1",
      )
      getPartitions(partitionConn) ==> List("a", "b", "c")

      // Partitions with attributes
      Schema.part.get(partitionConn) ==> List("b")
    }
  }
}
