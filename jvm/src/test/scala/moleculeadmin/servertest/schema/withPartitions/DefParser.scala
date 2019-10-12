package moleculeadmin.servertest.schema.withPartitions

import moleculeadmin.shared.lib.molecule.util.Helpers
import moleculeadmin.shared.testdata.TreeSchema
import sbtmolecule.Ast._
import sbtmolecule.{DefinitionParser, SchemaTransaction}
import utest._


object DefParser extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    val header =
      """package db.migration.schema
        |import molecule.schema.definition._
        |
        |@InOut(0, 3)
        |object PartitionDefinition {""".stripMargin


    test("Partition/ns") {

      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  // partDescr
           |  object a {
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "partDescr", List(
          Namespace("a", Some("partDescr"), "", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    // nsDescr
           |    trait Aa {
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", Some("nsDescr"), None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  // partDescr
           |  object a {
           |    // nsDescr
           |    trait Aa {
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "partDescr", List(
          Namespace("a", Some("partDescr"), "a_Aa", Some("nsDescr"), None, Nil)))
    }


    test("Attributes") {

      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |      val aa = oneInt
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long",
              Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)))
        ))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |      // valDescr
           |      val aa = oneInt
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long",
              Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", Some("valDescr"))))))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |      val aa = oneInt
           |      val ab = oneInt
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long",
              Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None),
            Val("ab", "ab", "OneInt", "Int", "", "long",
              Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None)))))


      // Lineshift between attribute definitions denotes "attribute group"
      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |      val aa = oneInt
           |
             |      val ab = oneInt
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", None, None, List(
            Val("aa", "aa", "OneInt", "Int", "", "long",
              Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None),
            Val("ab", "ab", "OneInt", "Int", "", "long",
              Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", Some(""))))))
    }


    test("Multiple partitions/nss") {

      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |  }
           |  object b {
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "b", "", List(
          Namespace("a", None, "", None, None, Nil),
          Namespace("b", None, "", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "a", "", List(
          Namespace("a", None, "a_Aa", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |    }
           |  }
           |  object b {
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "b", "", List(
          Namespace("a", None, "a_Aa", None, None, Nil),
          Namespace("b", None, "", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |  }
           |  object b {
           |    trait Bb {
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "b", "", List(
          Namespace("a", None, "", None, None, Nil),
          Namespace("b", None, "b_Bb", None, None, Nil)))


      DefinitionParser("PartitionDefinition.scala",
        s"""$header
           |  object a {
           |    trait Aa {
           |    }
           |  }
           |  object b {
           |    trait Bb {
           |    }
           |  }
           |}""".stripMargin.split("\n").toList).parse ==>
        Definition("db.migration", 0, 3, "Partition", "b", "", List(
          Namespace("a", None, "a_Aa", None, None, Nil),
          Namespace("b", None, "b_Bb", None, None, Nil)))
    }


    test("Reserved partition names") {


      try {
        DefinitionParser("PartitionDefinition.scala",
          s"""$header
             |  object tx {
             |  }
             |}""".stripMargin.split("\n").toList).parse
      } catch {
        case e: SchemaDefinitionException =>
          e.getMessage ==> //"Got the exception sbtmolecule.Ast$SchemaDefinitionException: " +
            s"Partition name 'tx' in PartitionDefinition.scala (line 6) is not allowed. `tx`, `db` and `molecule` are reserved partition names."
      }

      try {
        DefinitionParser("PartitionDefinition.scala",
          s"""$header
             |  object db {
             |  }
             |}""".stripMargin.split("\n").toList).parse
      } catch {
        case e: SchemaDefinitionException =>
          e.getMessage ==> //"Got the exception sbtmolecule.Ast$SchemaDefinitionException: " +
            s"Partition name 'db' in PartitionDefinition.scala (line 6) is not allowed. `tx`, `db` and `molecule` are reserved partition names."
      }



      //      (DefinitionParser("PartitionDefinition.scala",
      //        s"""$header
      //           |  object tx {
      //           |  }
      //           |}""".stripMargin.split("\n").toList).parse must throwA[SchemaDefinitionException])
      //        .message ==> "Got the exception sbtmolecule.Ast$SchemaDefinitionException: " +
      //        s"Partition name 'tx' in PartitionDefinition.scala (line 6) is not allowed. `tx`, `db` and `molecule` are reserved partition names."
      //
      //      (DefinitionParser("PartitionDefinition.scala",
      //        s"""$header
      //           |  object db {
      //           |  }
      //           |}""".stripMargin.split("\n").toList).parse must throwA[SchemaDefinitionException])
      //        .message ==> "Got the exception sbtmolecule.Ast$SchemaDefinitionException: " +
      //        s"Partition name 'db' in PartitionDefinition.scala (line 6) is not allowed. `tx`, `db` and `molecule` are reserved partition names."
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


      val d = DefinitionParser("PartitionDefinition.scala", defFile.split("\n").toList).parse
      d ==> Definition("db.migration", 0, 3, "Partition", "c", "", List(
        Namespace("a", None, "a_Aa", None, None, List(
          Val("aa", "aa", "OneInt", "Int", "", "long", Seq(Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed")), None, "", None))),
        Namespace("b", None, "b_Bb", None, None, Nil),
        Namespace("c", None, "", None, None, Nil)))


      SchemaTransaction(d) ==>
        """/*
          |* AUTO-GENERATED Molecule DSL schema boilerplate code
          |*
          |* To change:
          |* 1. edit schema definition file in `db.migration.schema/`
          |* 2. `sbt compile` in terminal
          |* 3. Refresh and re-compile project in IDE
          |*/
          |package db.migration.schema
          |import molecule.schema.SchemaTransaction
          |import datomic.{Util, Peer}
          |
          |object PartitionSchema extends SchemaTransaction {
          |
          |  lazy val partitions = Util.list(
          |
          |    Util.map(":db/ident"             , ":a",
          |             ":db/id"                , Peer.tempid(":db.part/db"),
          |             ":db.install/_partition", ":db.part/db"),
          |
          |    Util.map(":db/ident"             , ":b",
          |             ":db/id"                , Peer.tempid(":db.part/db"),
          |             ":db.install/_partition", ":db.part/db"),
          |
          |    Util.map(":db/ident"             , ":c",
          |             ":db/id"                , Peer.tempid(":db.part/db"),
          |             ":db.install/_partition", ":db.part/db")
          |  )
          |
          |
          |  lazy val namespaces = Util.list(
          |
          |    // a_Aa -------------------------------------------------------------
          |
          |    Util.map(":db/ident"             , ":a_Aa/aa",
          |             ":db/valueType"         , ":db.type/long",
          |             ":db/cardinality"       , ":db.cardinality/one",
          |             ":db/index"             , true.asInstanceOf[Object])
          |  )
          |}""".stripMargin

    }
  }
}
