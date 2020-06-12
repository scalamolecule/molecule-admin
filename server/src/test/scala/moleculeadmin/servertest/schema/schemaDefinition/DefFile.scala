package moleculeadmin.servertest.schema.schemaDefinition

import molecule.util.Helpers
import moleculeadmin.shared.testdata.TreeSchema
import sbtmolecule.Ast._
import sbtmolecule.DefinitionParser
import utest._


object DefFile extends TestSuite with TreeSchema with Helpers with moleculeadmin.server.Base {

  val tests = Tests {

    test("partition") {

      DefinitionParser("PartitionDefinition.scala",
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 3)
          |object PartitionDefinition {
          |
          |  trait Aa {
          |
          |  }
          |}
          |""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "", "", List(
          Namespace("db.part/user", None, "Aa", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 3)
          |object PartitionDefinition {
          |
          |  trait Aa {
          |    val aa = oneInt
          |  }
          |}
          |""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "", "", List(
          Namespace("db.part/user", None, "Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long", Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)))
        ))
    }


    test("test") {
      val defFile =
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 3)
          |object PartitionDefinition {
          |
          |  object b {
          |
          |    trait Bb {
          |
          |    }
          |  }
          |}
          |""".stripMargin

      DefinitionParser("PartitionDefinition.scala", defFile.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "b", "", List(
          Namespace("b", None, "b_Bb", None, None, Nil)))
    }

    test("test") {
      val defFile =
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 3)
          |object PartitionDefinition {
          |
          |  object a {
          |
          |    trait Aa {
          |      val aa = oneInt
          |    }
          |  }
          |
          |  object b {
          |
          |    trait Bb {
          |
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |}
          |""".stripMargin

      DefinitionParser("PartitionDefinition.scala", defFile.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "c", "", List(
          Namespace("a", None, "a_Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long", Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None))),
          Namespace("b", None, "b_Bb", None, None, Nil),
          Namespace("c", None, "", None, None, Nil)))

    }


    test("test2") {
      val defFile =
        """package db.migration.schema
          |import molecule.schema.definition._
          |
          |@InOut(0, 3)
          |object PartitionDefinition {
          |
          |  object a {
          |
          |    trait Aa {
          |      val aa = oneInt
          |    }
          |  }
          |
          |  object c {
          |
          |  }
          |
          |  object b {
          |
          |    trait Bb {
          |
          |    }
          |  }
          |}
          |""".stripMargin

      DefinitionParser("PartitionDefinition.scala", defFile.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "b", "", List(
          Namespace("a", None, "a_Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long", Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None))),
          Namespace("c", None, "", None, None, Nil),
          Namespace("b", None, "b_Bb", None, None, Nil)
        ))

    }
  }
}