package moleculeadmin.servertest.schema.attribute.update

import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeDoc extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("doc") {
      val ps = new PartitionSetup
      import ps._

      // Add doc text
      val schema1 = updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1",
        1, "Int", Nil, None, Nil, Some("doc text")).getOrElse(MetaSchema(Nil))

      schema1 ==> MetaSchema(List(
        Part(1, "a", None, None, List(
          Ns(1, "Aa", "a_Aa", None, None, List()))),
        Part(2, "b", None, None, List(
          Ns(1, "Bb", "b_Bb", None, None, List(
            Attr(1, "bb1", 1, "Int", None, None, None, Some("doc text"), None, None, None, None, List()),
            Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
          Ns(2, "Bc", "b_Bc", None, None, List(
            Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
        Part(3, "c", None, None, List())
      ))

      read ! partitionDefFilePath ==>
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 5)
          |object PartitionDefinition {
          |
          |  object a {
          |
          |    trait Aa {
          |
          |    }
          |  }
          |
          |  object b {
          |
          |    trait Bb {
          |      val bb1 = oneInt.doc("doc text")
          |      val bb2 = oneInt
          |    }
          |
          |    trait Bc {
          |      val bc1 = oneInt
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin


      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").doc.get(moleculeAdminConn) ==> List("doc text")

      Schema.attr_("bb1").doc.get(partitionConn) ==> List("doc text")

      // Remove doc text
      updateAttribute(schema1, "Partition", "b", "Bb", "bb1", "bb1", 1, "Int",
        Nil, None, Nil, None) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

      read ! partitionDefFilePath ==> partitionDefFile

      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name("bb1").doc$.get(moleculeAdminConn) ==> List(("bb1", None))

      Schema.attr("bb1").doc$.get(partitionConn) ==> List(("bb1", None))
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}