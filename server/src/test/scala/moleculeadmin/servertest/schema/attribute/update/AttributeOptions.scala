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


object AttributeOptions extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Validation") {
      val ps = new PartitionSetup
      import ps._

      // 1 bad option
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int",
        1, "Int", Nil, None, Seq("bad")) ==> Left(
        "Successfully rolled back from error: Found unrecognized option(s): bad" +
          s"\nAvailable options: noHistory, uniqueValue, uniqueIdentity, isComponent, fulltext"
      )

      // Successfully rolled back (attribute `int` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("int").pos.card.tpe.options$.get(moleculeAdminConn) ==> List((2, 1, "Int", Some(Set("indexed"))))

      // live schema
      Schema.attr("int").card.tpe.index$.unique$.fulltext$.isComponent$.noHistory$.get(coretestConn) ==> List(
        ("int", "one", "long", Some(true), None, None, None, None)
      )

      // Multiple bad options
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int",
        1, "Int", Nil, None, Seq("bad", "worse")) ==> Left(
        "Successfully rolled back from error: Found unrecognized option(s): bad, worse" +
          s"\nAvailable options: noHistory, uniqueValue, uniqueIdentity, isComponent, fulltext"
      )

      // Successfully rolled back (attribute `int` unchanged)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("int").pos.card.tpe.options$.get(moleculeAdminConn) ==> List((2, 1, "Int", Some(Set("indexed"))))

      // live schema
      Schema.attr("int").card.tpe.index$.unique$.fulltext$.isComponent$.noHistory$.get(coretestConn) ==> List(
        ("int", "one", "long", Some(true), None, None, None, None)
      )

      // Mixed ok and bad options
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int",
        1, "Int", Nil, None, Seq("bad", "noHistory")) ==> Left(
        "Successfully rolled back from error: Found unrecognized option(s): bad" +
          s"\nAvailable options: noHistory, uniqueValue, uniqueIdentity, isComponent, fulltext"
      )

      // Successfully rolled back (attribute `int` unchanged. `noHistory` option discarded too.)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("int").pos.card.tpe.options$.get(moleculeAdminConn) ==> List((2, 1, "Int", Some(Set("indexed"))))

      // live schema
      Schema.attr("int").card.tpe.index$.unique$.fulltext$.isComponent$.noHistory$.get(coretestConn) ==> List(
        ("int", "one", "long", Some(true), None, None, None, None)
      )
    }


    test("noHistory") {
      val ps = new PartitionSetup
      import ps._

      updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1",
        1, "Int", Nil, None, Seq("noHistory")) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

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
          |      val bb1 = oneInt.noHistory
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


      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").options.get(moleculeAdminConn) ==> List(Set("noHistory"))

      Schema.attr_("bb1").noHistory.get(partitionConn) ==> List(true)

      // Removing option by applying empty option list
      updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1",
        1, "Int", Nil, None, Nil) ==> Right(
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
    }


    test("isComponent") {
      val ps = new PartitionSetup
      import ps._

      // `isComponent` only available to ref attributes
      updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1",
        1, "Int", Nil, None, Seq("isComponent")) ==> Left(
        "Successfully rolled back from error: Can only apply `isComponent` option to ref attributes."
      )

      // Successfully rolled back (attribute `bb1` unchanged. `isComponent` option discarded)

      // client
      getMetaSchema("Partition") ==> partitionMetaSchema

      // def file
      read ! partitionDefFilePath ==> partitionDefFile

      // meta
      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").pos.card.tpe.options$.get(moleculeAdminConn) ==> List((1, 1, "Int", Some(Set("indexed"))))

      // live schema
      Schema.attr("bb1").card.tpe.isComponent$.get(partitionConn) ==> List(("bb1", "one", "long", None))


      // Add ref attr
      val schema1: MetaSchema = createAttribute(partitionMetaSchema, "Partition", "b", "Bb", "refAttr",
        1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

      // Add isComponent to ref attr
      updateAttribute(schema1, "Partition", "b", "Bb", "refAttr", "refAttr",
        1, "ref", Nil, None, Seq("isComponent")) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(3, "refAttr", 1, "ref", None, Some("b_Bc"), Some(Set("isComponent")), None, None, None, None, None, List())
            )),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

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
          |      val bb1     = oneInt
          |      val bb2     = oneInt
          |      val refAttr = one[Bc].isComponent
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


      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("refAttr").options.get(moleculeAdminConn) ==> List(Set("isComponent"))

      Schema.attr_("refAttr").isComponent.get(partitionConn) ==> List(true)
    }


    test("unique") {
      val ps = new PartitionSetup
      import ps._

      // Add isComponent to ref attr
      val schema1 = updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1",
        1, "Int", Nil, None, Seq("uniqueValue")).getOrElse(MetaSchema(Nil))
      schema1 ==> MetaSchema(List(
        Part(1, "a", None, None, List(
          Ns(1, "Aa", "a_Aa", None, None, List()))),
        Part(2, "b", None, None, List(
          Ns(1, "Bb", "b_Bb", None, None, List(
            Attr(1, "bb1", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
            Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
          )),
          Ns(2, "Bc", "b_Bc", None, None, List(
            Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
        Part(3, "c", None, None, List()),
      ))

      updateAttribute(schema1, "Partition", "b", "Bb", "bb2", "bb2",
        1, "Int", Nil, None, Seq("uniqueIdentity")) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List())
            )),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

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
          |      val bb1 = oneInt.uniqueValue
          |      val bb2 = oneInt.uniqueIdentity
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


      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.options.get(moleculeAdminConn) ==> List(
        ("bb1", Set("uniqueValue")),
        ("bb2", Set("uniqueIdentity"))
      )

      Schema.ns_("Bb").a.unique.get(partitionConn) ==> List(
        (":b_Bb/bb1", "value"),
        (":b_Bb/bb2", "identity")
      )
    }


    test("fulltext") {
      val ps = new PartitionSetup
      import ps._

      // Can't add fulltext option
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ref2", "str2", "str2",
        1, "String", Nil, None, Seq("fulltext")) ==> Left(
        "Successfully rolled back from error: Can't add fulltext option to existing attribute."
      )

      // Successfully rolled back (attribute `str1` unchanged. `fulltext` option discarded)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest")
        .Partitions.name_("db.part/user")
        .Namespaces.name_("Ref2")
        .Attrs.name_("str2").pos.card.tpe.options$
        .get(moleculeAdminConn) ==> List(
        (1, 1, "String", Some(Set("indexed", "uniqueIdentity")))
      )

      // live schema
      Schema.attr("str2").card.tpe.fulltext$.get(coretestConn) ==> List(("str2", "one", "string", None))


      // Can't remove fulltext option
      updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "str", "str",
        1, "String", Nil, None, Nil) ==> Left(
        "Successfully rolled back from error: Can't remove fulltext option from existing attribute."
      )

      // Successfully rolled back (attribute `str1` unchanged. `fulltext` option still applied)

      // client
      getMetaSchema("CoreTest") ==> coreMetaSchema

      // def file
      read ! coreDefFilePath ==> coreDefFile

      // meta
      meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("str").pos.card.tpe.options$.get(moleculeAdminConn) ==> List((1, 1, "String", Some(Set("indexed", "fulltext"))))

      // live schema
      Schema.attr("str").card.tpe.fulltext$.get(coretestConn) ==> List(("str", "one", "string", Some(true)))
    }


    test("Multiple") {
      val ps = new PartitionSetup
      import ps._

      // Add ref attr so that we can add `isComponent` option
      val schema1: MetaSchema = createAttribute(partitionMetaSchema, "Partition", "b", "Bb", "ref",
        1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

      // Add some options
      val schema2 = updateAttribute(schema1, "Partition", "b", "Bb", "ref", "ref",
        1, "ref", Nil, None, Seq("noHistory", "uniqueValue")).getOrElse(MetaSchema(Nil))

      schema2 ==> MetaSchema(List(
        Part(1, "a", None, None, List(
          Ns(1, "Aa", "a_Aa", None, None, List()))),
        Part(2, "b", None, None, List(
          Ns(1, "Bb", "b_Bb", None, None, List(
            Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
            Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
            Attr(3, "ref", 1, "ref", None, Some("b_Bc"), Some(Set("noHistory", "uniqueValue")), None, None, None, None, None, List())
          )),
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
          |      val bb1 = oneInt
          |      val bb2 = oneInt
          |      val ref = one[Bc].noHistory.uniqueValue
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


      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("ref").refNs.options.get(moleculeAdminConn) ==> List(
        ("b_Bc", Set("noHistory", "uniqueValue"))
      )

      Schema.attr_("ref").unique.noHistory.get(partitionConn) ==> List(("value", true))


      // Keep `noHistory` option
      // Remove `uniqueValue` option (by not being present in passed options) and replace with `uniqueIdentity` option
      // Add `isComponent` option
      updateAttribute(schema2, "Partition", "b", "Bb", "ref", "ref",
        1, "ref", Nil, None, Seq("noHistory", "uniqueIdentity", "isComponent")
      ) ==> Right(
        MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(3, "ref", 1, "ref", None, Some("b_Bc"), Some(Set("isComponent", "noHistory", "uniqueIdentity")), None, None, None, None, None, List())
            )),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))
      )

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
          |      val bb1 = oneInt
          |      val bb2 = oneInt
          |      val ref = one[Bc].isComponent.noHistory.uniqueIdentity
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


      meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("ref").options.get(moleculeAdminConn) ==> List(Set("isComponent", "uniqueIdentity", "noHistory"))

      Schema.attr_("ref").isComponent.noHistory.unique.get(partitionConn) ==> List((true, true, "identity"))
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}