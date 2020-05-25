package moleculeadmin.servertest.schema.withPartitions

import java.util
import ammonite.ops._
import datomic.Util
import datomic.Util.list
import db.admin.dsl.moleculeAdmin._
import db.core.dsl.coreTest.{Ns => Nsx}
import molecule.api.out10._
import moleculeadmin.servertest.ResetDbs
import moleculeadmin.shared.ast.schema
import moleculeadmin.shared.ast.schema._
import molecule.util.Helpers
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object Attributes extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Create") {

      test("Basic validation") {
        val ps = new PartitionSetup
        import ps._

        createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "", 1, "Int") ==> Left(
          "Empty attribute name."
        )

        createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "Aa1", 1, "Int") ==> Left(
          "Attribute name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`."
        )

        createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "a-1", 1, "Int") ==> Left(
          "Attribute name should match `[a-z][a-zA-Z0-9]*`."
        )

        createAttribute(partitionMetaSchema, "Partition", "a", "Aa", "op", 1, "Int") ==> Left(
          """Reserved attribute names:
            |a
            |apply
            |assert
            |contains
            |e
            |insert
            |k
            |not
            |op
            |replace
            |retract
            |save
            |self
            |t
            |tx
            |txInstant
            |update
            |v""".stripMargin
        )

        createAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", 1, "Int") ==> Left(
          "Attribute `bb1` already exists in namespace `Bb` in partition `b` in database `Partition`. Please choose another attribute name."
        )

        createAttribute(partitionMetaSchema, "qqq", "a", "Bc", "aa1", 1, "Int") ==> Left(
          "Couldn't find database `qqq`."
        )

        createAttribute(partitionMetaSchema, "Partition", "z", "Bc", "aa1", 1, "Int") ==> Left(
          "Couldn't find partition `z` in database `Partition`."
        )

        createAttribute(partitionMetaSchema, "Partition", "b", "Bz", "aa1", 1, "Int") ==> Left(
          "Couldn't find namespace `Bz` in partition `b` in database `Partition`."
        )
      }


      test("Int, card 1, first pos, descr") {
        val ps = new PartitionSetup
        import ps._

        // Client schema
        createAttribute(partitionMetaSchema, "Partition", "a", "Aa",
          "number", 1, "Int", Nil, None, Nil, Some("number description")).getOrElse(MetaSchema(Nil)).parts.head ==>
          Part(1, "a", None, None, Seq(
            Ns(1, "Aa", "a_Aa", None, None, Seq(
              Attr(1, "number", 1, "Int", None, None, None, Some("number description"), None, None, None, None, List())))))

        // def file has namespace prepared, here positioned first
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
            |      val number = oneInt.doc("number description")
            |    }
            |  }
            |
            |  object b {
            |
            |    trait Bb {
            |      val bb1 = oneInt
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

        // meta schema (saving meta partition for possible ns/attr additions)
        meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name_("Aa").Attrs.name.get(moleculeAdminConn).sorted ==> List("number")

        // live namespaces having defined attributes
        Schema.ns_("Aa").attr.get(partitionConn).sorted ==> List("number")

        // Data - we can use the new attribute
        partitionConn.transact(list(list(":db/add", "#db/id[:a -1000001]", ":a_Aa/number", 42.asInstanceOf[Object])).asInstanceOf[util.List[AnyRef]])
        partitionConn.q("[:find ?value :where [_ :a_Aa/number ?value]]") ==> List(List(42))
      }


      test("Int, card 1, first pos, option, descr") {
        val ps = new PartitionSetup
        import ps._

        // Client schema
        createAttribute(partitionMetaSchema, "Partition", "a", "Aa",
          "number", 1, "Int", Nil, None, Seq("noHistory"), Some("number description")).getOrElse(MetaSchema(Nil)).parts.head ==>
          Part(1, "a", None, None, Seq(
            Ns(1, "Aa", "a_Aa", None, None, Seq(
              Attr(1, "number", 1, "Int", None, None, Some(Set("noHistory")), Some("number description"), None, None, None, None, List())))))

        // def file has namespace prepared, here positioned first
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
            |      val number = oneInt.noHistory.doc("number description")
            |    }
            |  }
            |
            |  object b {
            |
            |    trait Bb {
            |      val bb1 = oneInt
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

        // meta schema (saving meta partition for possible ns/attr additions)
        meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name_("Aa")
          .Attrs.name.options.get(moleculeAdminConn) ==> List(("number", Set("noHistory")))

        // live namespaces having defined attributes
        Schema.ns_("Aa").attr.noHistory.get(partitionConn) ==> List(("number", true))

        // Data - we can use the new attribute
        partitionConn.transact(list(list(":db/add", "#db/id[:a -1000001]", ":a_Aa/number", 42.asInstanceOf[Object])).asInstanceOf[util.List[AnyRef]])
        partitionConn.q("[:find ?value :where [_ :a_Aa/number ?value]]") ==> List(List(42))
      }


      test("Enum, card 2, middle pos, options, descr") {
        val ps = new PartitionSetup
        import ps._

        createAttribute(partitionMetaSchema, "Partition", "b", "Bb", "text",
          2, "String", Seq("enum1", "enum2"), None, Seq("fulltext", "noHistory", "uniqueValue"), Some("descr"), 2).getOrElse(MetaSchema(Nil)).parts(1) ==>
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "text", 2, "String", Some(Set("enum1", "enum2")), None, Some(Set("fulltext", "noHistory", "uniqueValue")), Some("descr"), None, None, None, None, List()),
              Attr(3, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List())))))

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
            |      val bb1  = oneInt
            |      val text = manyEnum("enum1", "enum2").fulltext.noHistory.uniqueValue.doc("descr")
            |      val bb2  = oneInt
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

        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.get(moleculeAdminConn).sorted ==> List("bb1", "bb2", "text")

        Schema.ns_("Bb").attr.get(partitionConn).sorted ==> List("bb1", "bb2", "text")

        // Data - we can use the new enum attribute
        partitionConn.transact(list(list(":db/add", "#db/id[:a -1000001]", ":b_Bb/text", ":b_Bb.text/enum1")).asInstanceOf[util.List[AnyRef]])
        partitionConn.q(
          """[:find  (distinct ?b2)
            | :where [?a :b_Bb/text ?b]
            |        [?b :db/ident ?b1]
            |        [(name ?b1) ?b2]]""".stripMargin) ==> List(List(Set("enum1")))
      }


      test("Ref (card 1)") {
        val ps = new PartitionSetup
        import ps._

        createAttribute(partitionMetaSchema, "Partition", "b", "Bc",
          "ref", 1, "ref", Nil, Some("b_Bb")).getOrElse(MetaSchema(Nil)).parts(1) ==>
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "ref", 1, "ref", None, Some("b_Bb"), None, None, None, None, None, None, List())))))

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
            |    }
            |
            |    trait Bc {
            |      val bc1 = oneInt
            |      val ref = one[Bb]
            |    }
            |  }
            |
            |  object c {
            |
            |  }
            |}
            |""".stripMargin

        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bc").Attrs.name.get(moleculeAdminConn).sorted ==> List("bc1", "ref")

        Schema.ns_("Bc").attr.get(partitionConn).sorted ==> List("bc1", "ref")

        // Data - we can use the new reference
        partitionConn.transact(
          list(
            list(":db/add", "#db/id[:b -1000001]", ":b_Bc/bc1", 42.asInstanceOf[Object]),
            list(":db/add", "#db/id[:b -1000001]", ":b_Bc/ref", "#db/id[:b -1000002]"),
            list(":db/add", "#db/id[:b -1000002]", ":b_Bb/bb1", 43.asInstanceOf[Object]),
          ).asInstanceOf[util.List[AnyRef]]
        )
        partitionConn.q(
          """[:find  ?b ?d
            | :where [?a :b_Bc/bc1 ?b]
            |        [?a :b_Bc/ref ?c]
            |        [?c :b_Bb/bb1 ?d]]""".stripMargin) ==> List(List(42, 43))
      }


      test("Ref (card 2) to other partition") {
        val ps = new PartitionSetup
        import ps._

        createAttribute(partitionMetaSchema, "Partition", "a", "Aa",
          "ref", 2, "ref", Nil, Some("b_Bb")) ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List(
                Attr(1, "ref", 2, "ref", None, Some("b_Bb"), None, None, None, None, None, None, List()),
              )))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List())
          )
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
            |      val ref = many[b.Bb]
            |    }
            |  }
            |
            |  object b {
            |
            |    trait Bb {
            |      val bb1 = oneInt
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

        meta_Db.name_("Partition").Partitions.name_("a").Namespaces.name_("Aa").Attrs.name.get(moleculeAdminConn).sorted ==> List("ref")

        Schema.ns_("Aa").attr.get(partitionConn).sorted ==> List("ref")

        // Data - we can use the new reference between two partitions
        partitionConn.transact(
          list(
            list(":db/add", "#db/id[:a -1000001]", ":a_Aa/ref", "#db/id[:b -1000002]"),
            list(":db/add", "#db/id[:b -1000002]", ":b_Bb/bb1", 43.asInstanceOf[Object]),
          ).asInstanceOf[util.List[AnyRef]]
        )
        partitionConn.q(
          """[:find  ?c
            | :where [?a :a_Aa/ref ?b]
            |        [?b :b_Bb/bb1 ?c]]""".stripMargin) ==> List(List(43))
      }
    }


    test("Update") {

      test("Basic validation") {
        val ps = new PartitionSetup
        import ps._

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "", 1, "Int") ==> Left(
          "Empty attribute name."
        )

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "Hobby", 1, "Int") ==> Left(
          "Attribute name should start with lowercase letter and match `[a-z][a-zA-Z0-9]*`."
        )

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "best-friend", 1, "Int") ==> Left(
          "Attribute name should match `[a-z][a-zA-Z0-9]*`."
        )

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "op", 1, "Int") ==> Left(
          """Reserved attribute names:
            |a
            |apply
            |assert
            |contains
            |e
            |insert
            |k
            |not
            |op
            |replace
            |retract
            |save
            |self
            |t
            |tx
            |txInstant
            |update
            |v""".stripMargin
        )

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1", 4, "Int") ==> Left(
          "Cardinality number can only be 1, 2 or 3. Found: `4`"
        )

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb2", 1, "Int") ==> Left(
          "Attribute `bb2` already exists in namespace `Bb` in partition `b` in database `Partition`. Please choose another attribute name."
        )

        updateAttribute(partitionMetaSchema, "qqq", "b", "Bb", "bb1", "x", 1, "Int") ==> Left(
          "Couldn't find database `qqq`."
        )

        updateAttribute(partitionMetaSchema, "Partition", "z", "Bb", "bb1", "x", 1, "Int") ==> Left(
          "Couldn't find partition `z` in client schema."
        )

        updateAttribute(partitionMetaSchema, "Partition", "b", "Bz", "bb1", "x", 1, "Int") ==> Left(
          "Couldn't find namespace `Bz` in partition `b` in client schema."
        )

        val nestedMetaSchema1 = MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(3, "Bz", "b_Bz", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List())
            ))
          )),
          Part(3, "c", None, None, List()),
          Part(3, "z", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List())
            ))
          ))
        ))

        updateAttribute(nestedMetaSchema1, "Partition", "z", "Bb", "bb1", "x", 1, "Int") ==> Left(
          "Couldn't find partition `z` in database `Partition`."
        )

        updateAttribute(nestedMetaSchema1, "Partition", "b", "Bz", "bb1", "x", 1, "Int") ==> Left(
          "Couldn't find namespace `Bz` in partition `b` in database `Partition`."
        )

        // no change
        updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int",
          1, "Int", Nil, None, Nil, Some("Card one Int attribute"), 2) ==> Right(coreMetaSchema)
      }


      test("Attr group") {

        test("Empty header") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb2", "bb2", 1, "Int", Nil, None, Nil, None, 2, Some("")) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, Some(""), None, None, None, List()),
                  Attr(3, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            )
            ))
        }

        test("Header") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb3", "bb3", 1, "Int", Nil, None, Nil, None, 3, Some("Attr group header...")) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb3", 1, "Int", None, None, None, None, Some("Attr group header..."), None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }
      }


      test("Order") {

        test("Unchanged") {
          val ps = new PartitionSetup1
          import ps._

          // Unchanged
          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "bb1", "bb1",
            1, "Int", Nil, None, Nil, None, 0) ==> Right(partition1MetaSchema)

          // Default
          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "bb1", "bb1",
            1, "Int") ==> Right(partition1MetaSchema)

          // Unchanged - `bb1` is already first
          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "bb1", "bb1",
            1, "Int", Nil, None, Nil, None, 1) ==> Right(partition1MetaSchema)
        }


        test("1 -> 2") {
          val ps = new PartitionSetup1
          import ps._


          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb1", "bb1", 1, "Int", Nil, None, Nil, None, 2) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("1 -> 3") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb1", "bb1", 1, "Int", Nil, None, Nil, None, 3) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("1 -> 4") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb1", "bb1", 1, "Int", Nil, None, Nil, None, 4) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("2 -> 1") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb2", "bb2", 1, "Int", Nil, None, Nil, None, 1) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("2 -> 3") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb2", "bb2", 1, "Int", Nil, None, Nil, None, 3) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("2 -> 4") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb2", "bb2", 1, "Int", Nil, None, Nil, None, 4) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("3 -> 1") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb3", "bb3", 1, "Int", Nil, None, Nil, None, 1) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("3 -> 2") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb3", "bb3", 1, "Int", Nil, None, Nil, None, 2) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("3 -> 4") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb3", "bb3", 1, "Int", Nil, None, Nil, None, 4) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("4 -> 1") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb4", "bb4", 1, "Int", Nil, None, Nil, None, 1) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("4 -> 2") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb4", "bb4", 1, "Int", Nil, None, Nil, None, 2) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }


        test("4 -> 3") {
          val ps = new PartitionSetup1
          import ps._

          updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
            "bb4", "bb4", 1, "Int", Nil, None, Nil, None, 3) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )
        }
      }


      test("Cardinality") {


        test("One -> map not allowed") {
          val ps = new PartitionSetup
          import ps._

          // card one to map cardinality not allowed
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int", 3, "Int") ==> Left(
            "Successfully rolled back from error: " +
              "Couldn't change to map cardinality since keys are unknown. Please create a new map attribute and populate with keyed data."
          )

          // Successfully rolled back (attribute `int` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("int").pos.card.tpe.get(moleculeAdminConn) ==> List((2, 1, "Int"))

          // live schema
          Schema.attr("int").card.tpe.get(coreConn) ==> List(("int", "one", "long"))
        }


        test("Many -> map not allowed") {
          val ps = new PartitionSetup
          import ps._

          // card many to map cardinality not allowed
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "ints", "ints", 3, "Int") ==> Left(
            "Successfully rolled back from error: " +
              "Couldn't change to map cardinality since keys are unknown. Please create a new map attribute and populate with keyed data."
          )

          // Successfully rolled back (attribute `ints` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("ints").pos.card.tpe.get(moleculeAdminConn) ==> List((17, 2, "Int"))

          // live schema
          Schema.attr("ints").card.tpe.get(coreConn) ==> List(("ints", "many", "long"))
        }


        test("One -> many -> one") {
          val ps = new PartitionSetup
          import ps._

          val schema1 = updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb1", 2, "Int").getOrElse(MetaSchema(Nil))
          schema1 ==> MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 2, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List())
          ))

          val updatedDefFile =
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
              |      val bb1 = manyInt
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

          read ! partitionDefFilePath ==> updatedDefFile

          // meta
          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").card.get(moleculeAdminConn).sorted ==> List(2)

          // live
          Schema.a_(":b_Bb/bb1").card.get(partitionConn) ==> List("many")


          // Add set of 2 values to attr that is now cardinality-many
          val eid: Long = partitionConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 1.asInstanceOf[AnyRef]),
              Util.list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 2.asInstanceOf[AnyRef])
            ).asInstanceOf[util.List[AnyRef]]
          ).eid
          partitionConn.q(s"[:find (distinct ?v) :where [$eid :b_Bb/bb1 ?v]]") ==> List(List(Set(1, 2)))

          // Can't make change from card many to one if any entity has multiple values for the attribute
          updateAttribute(schema1, "Partition", "b", "Bb", "bb1", "bb1", 1, "Int") ==>
            Left("Successfully rolled back from error: " +
              "Couldn't change attribute to cardinality 1 since some entities have multiple values. Please reduce all values to one value before changing cardinality.")

          // Successfully rolled back (attribute `bb1` still cardinality 2)

          // client
          getMetaSchema("Partition") ==> schema1

          // def file
          read ! partitionDefFilePath ==> updatedDefFile

          // meta
          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").pos.card.tpe.get(moleculeAdminConn) ==> List((1, 2, "Int"))

          // live schema
          Schema.attr("bb1").card.tpe.get(partitionConn) ==> List(("bb1", "many", "long"))


          // Make all entities (1 here) have maximum 1 value for the attribute
          partitionConn.transact(
            Util.list(
              Util.list(s":db/retract", eid.asInstanceOf[AnyRef], ":b_Bb/bb1", 2.asInstanceOf[AnyRef])
            ).asInstanceOf[util.List[AnyRef]]
          )
          partitionConn.q(s"[:find (distinct ?v) :where [$eid :b_Bb/bb1 ?v]]") ==> List(List(Set(1)))

          // Now we can change cardinality from many back to one
          updateAttribute(schema1, "Partition", "b", "Bb", "bb1", "bb1", 1, "Int") ==> Right(
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

          // Def file back to original
          read ! partitionDefFilePath ==> partitionDefFile

          // meta
          meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").card.get(moleculeAdminConn).sorted ==> List(1)

          // live
          Schema.a_(":b_Bb/bb1").card.get(partitionConn) ==> List("one")
        }


        test("Map -> one (String)") {
          val ps = new PartitionSetup
          import ps._

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(3)

          // live - map cardinality is internally just cardinality `many`
          Schema.a_(":Ns/strMap").card.get(coreConn) ==> List("many")

          // Create 2 entities with 2/1 values
          val List(e1, e2) = coreConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key1@hello"),
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key2@world"),
              Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/strMap", "key3@mister")
            ).asInstanceOf[util.List[AnyRef]]
          ).eids

          coreConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
            List(e1, Set("key1@hello", "key2@world")),
            List(e2, Set("key3@mister"))
          )

          // Can't make change from card map to one if any entity has multiple values for the attribute
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "strMap", "strMap",
            1, "String") ==> Left("Successfully rolled back from error: " +
            "Couldn't change attribute to cardinality 1 since some entities have multiple values. Please reduce all values to one value before changing cardinality.")

          // Successfully rolled back (attribute `intMap` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").pos.card.tpe.get(moleculeAdminConn) ==> List((31, 3, "String"))

          // live schema
          Schema.attr("strMap").card.tpe.get(coreConn) ==> List(("strMap", "many", "string"))

          // Data intact
          coreConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
            List(e1, Set("key1@hello", "key2@world")),
            List(e2, Set("key3@mister"))
          )


          // Make all entities (1 here) have maximum 1 value for the attribute
          coreConn.transact(
            Util.list(
              Util.list(s":db/retract", e1.asInstanceOf[AnyRef], ":Ns/strMap", "key2@world")
            ).asInstanceOf[util.List[AnyRef]]
          )

          coreConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
            List(e1, Set("key1@hello")),
            List(e2, Set("key3@mister"))
          )

          // Now we can change cardinality from map to one
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "strMap", "strMap",
            1, "String", Nil, None, Seq("fulltext")
          ) ==> Right(
            MetaSchema(List(
              Part(1, "db.part/user", None, None, List(
                Ns(1, "Ns", "Ns", None, None, List(
                  Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, List()),
                  Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, List()),
                  Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(12, "enum", 1, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(14, "ref1", 1, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(27, "enums", 2, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),

                  Attr(31, "strMap", 1, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, List()),

                  Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Ref1", "Ref1", None, None, List(
                  Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, List()),
                  Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, List()),
                  Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()))),
                Ns(3, "Ref2", "Ref2", None, None, List(
                  Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List()),
                  Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
                  Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, List()),
                  Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List())))))
            ))
          )

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(1)

          // live
          Schema.a_(":Ns/strMap").card.get(coreConn) ==> List("one")

          // Keys are removed from String values
          coreConn.q(s"[:find ?e ?v :where [?e :Ns/strMap ?v]]") ==> List(
            List(e1, "hello"),
            List(e2, "mister")
          )
        }


        test("Map -> one (Int/other)") {
          val ps = new PartitionSetup
          import ps._

          // Now we can change cardinality from map to one
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "intMap", "intMap",
            1, "Int") ==> Left("Successfully rolled back from error: " +
            "Changing map cardinality when not of type String not supported yet (would require creating a new attribute of the target type)."
          )

          // Successfully rolled back (attribute `intMap` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("intMap").pos.card.tpe.get(moleculeAdminConn) ==> List((32, 3, "Int"))

          // live schema
          Schema.attr("intMap").card.tpe.get(coreConn) ==> List(("intMap", "many", "string"))
        }


        test("Map -> many (String)") {
          val ps = new PartitionSetup
          import ps._

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(3)

          // live - map cardinality is internally just cardinality `many`
          Schema.a_(":Ns/strMap").card.get(coreConn) ==> List("many")

          // Create 2 entities with 2/1 values
          val List(e1, e2) = coreConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key1@hello"),
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/strMap", "key2@world"),
              Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/strMap", "key2@mister")
            ).asInstanceOf[util.List[AnyRef]]
          ).eids

          coreConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
            List(e1, Set("key1@hello", "key2@world")),
            List(e2, Set("key2@mister"))
          )

          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "strMap", "strMap",
            2, "String", Nil, None, Seq("fulltext")
          ) ==> Right(
            MetaSchema(List(
              Part(1, "db.part/user", None, None, List(
                Ns(1, "Ns", "Ns", None, None, List(
                  Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, List()),
                  Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, List()),
                  Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(12, "enum", 1, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(14, "ref1", 1, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(27, "enums", 2, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),

                  Attr(31, "strMap", 2, "String", None, None, Some(Set("fulltext")), None, None, None, None, None, List()),

                  Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Ref1", "Ref1", None, None, List(
                  Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, List()),
                  Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, List()),
                  Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()))),
                Ns(3, "Ref2", "Ref2", None, None, List(
                  Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List()),
                  Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
                  Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, List()),
                  Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List())))))
            ))
          )

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("strMap").card.get(moleculeAdminConn).sorted ==> List(2)

          // live
          Schema.a_(":Ns/strMap").card.get(coreConn) ==> List("many")

          // Keys are removed from String values
          coreConn.q(s"[:find ?e (distinct ?v) :where [?e :Ns/strMap ?v]]") ==> List(
            List(e1, Set("hello", "world")),
            List(e2, Set("mister"))
          )
        }


        test("Map -> many (Int)") {
          val ps = new PartitionSetup
          import ps._
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "intMap", "intMap",
            2, "Int") ==> Left("Successfully rolled back from error: " +
            "Changing map cardinality when not of type String not supported yet (would require creating a new attribute of the target type)."
          )

          // Successfully rolled back (attribute `intMap` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("intMap").pos.card.tpe.get(moleculeAdminConn) ==> List((32, 3, "Int"))

          // live schema
          Schema.attr("intMap").card.tpe.get(coreConn) ==> List(("intMap", "many", "string"))
        }
      }


      test("Enums") {

        test("Validation") {
          val ps = new PartitionSetup
          import ps._
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enum", "enum",
            1, "Enum", Nil) ==> Left("Successfully rolled back from error: No enum values passed.")

          // Successfully rolled back (attribute `enum` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").pos.card.tpe.enums.get(moleculeAdminConn) ==> List(
            (12, 1, "String", Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8"))
          )

          // live schema
          Schema.attr("enum").card.tpe.enum.get(coreConn).sortBy(_._4) ==> List(
            ("enum", "one", "ref", "enum0"),
            ("enum", "one", "ref", "enum1"),
            ("enum", "one", "ref", "enum2"),
            ("enum", "one", "ref", "enum3"),
            ("enum", "one", "ref", "enum4"),
            ("enum", "one", "ref", "enum5"),
            ("enum", "one", "ref", "enum6"),
            ("enum", "one", "ref", "enum7"),
            ("enum", "one", "ref", "enum8"),
            ("enum", "one", "ref", "enum9")
          )
        }


        test("Card one") {
          val ps = new PartitionSetup
          import ps._

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").enums.get(moleculeAdminConn).head.toList.sorted ==> List(
            "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
          )

          // live - map cardinality is internally just cardinality `many`
          Schema.a_(":Ns/enum").enum.get(coreConn).sorted ==> List(
            "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
          )

          // Add 2 enum data values
          val List(e1, e2) = coreConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enum", ":Ns.enum/enum1"),
              Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/enum", ":Ns.enum/enum7")
            ).asInstanceOf[util.List[AnyRef]]
          ).eids

          // Live data
          coreConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enum ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==>
            List(List(Set("enum1", "enum7")))

          // Can't remove enum value if it has been asserted ("enum7" is now obsolete)
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enum", "enum",
            1, "Enum", Seq(
              "enum0", "enum1", "enum2", // keep some existing enum values
              "man", "woman" // add new enum values
            )
          ) ==> Left(
            """Successfully rolled back from error: Couldn't remove obsolete enums having live values. Please remove values before removing enums from schema.
              |Conflicting obsolete enum values: enum7""".stripMargin)

          // Successfully rolled back (attribute `enum` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").pos.card.tpe.enums.get(moleculeAdminConn) ==> List(
            (12, 1, "String", Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8"))
          )

          // live schema
          Schema.attr("enum").card.tpe.enum.get(coreConn).sortBy(_._4) ==> List(
            ("enum", "one", "ref", "enum0"),
            ("enum", "one", "ref", "enum1"),
            ("enum", "one", "ref", "enum2"),
            ("enum", "one", "ref", "enum3"),
            ("enum", "one", "ref", "enum4"),
            ("enum", "one", "ref", "enum5"),
            ("enum", "one", "ref", "enum6"),
            ("enum", "one", "ref", "enum7"),
            ("enum", "one", "ref", "enum8"),
            ("enum", "one", "ref", "enum9")
          )


          // Retract obsolete enum data value so that we can remove the enum schema value
          coreConn.transact(
            Util.list(
              Util.list(s":db/retract", e2.asInstanceOf[AnyRef], ":Ns/enum", ":Ns.enum/enum7")
            ).asInstanceOf[util.List[AnyRef]]
          )

          // Now we can also remove the `enum7` schema enum value
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enum", "enum",
            1, "Enum", Seq(
              "enum0", "enum1", "enum2", // keep some existing enum values
              "man", "woman" // add new enum values
            )
          ) ==> Right(
            MetaSchema(List(
              Part(1, "db.part/user", None, None, List(
                Ns(1, "Ns", "Ns", None, None, List(
                  Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, List()),
                  Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, List()),
                  Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, List()),

                  Attr(12, "enum", 1, "String", Some(Set(
                    "enum0", "enum1", "enum2",
                    "man", "woman"
                  )), None, None, None, None, None, None, None, List()),

                  Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(14, "ref1", 1, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(27, "enums", 2, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(31, "strMap", 3, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Ref1", "Ref1", None, None, List(
                  Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, List()),
                  Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, List()),
                  Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()))),
                Ns(3, "Ref2", "Ref2", None, None, List(
                  Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List()),
                  Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
                  Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, List()),
                  Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List())))))
            ))
          )

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enum").enums.get(moleculeAdminConn) ==> List(Set("enum1", "man", "woman", "enum2", "enum0"))

          // live
          Schema.a_(":Ns/enum").enum.get(coreConn) ==> List("woman", "enum2", "man", "enum1", "enum0")

          // New enums can now be asserted
          coreConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enum", ":Ns.enum/woman")
            ).asInstanceOf[util.List[AnyRef]]
          ).eids

          // Value is saved
          coreConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enum ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==>
            List(List(Set("enum1", "woman")))
        }


        test("Card many") {
          val ps = new PartitionSetup
          import ps._

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enums").enums.get(moleculeAdminConn).head.toList.sorted ==> List(
            "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
          )

          // live - map cardinality is internally just cardinality `many`
          Schema.a_(":Ns/enums").enum.get(coreConn).sorted ==> List(
            "enum0", "enum1", "enum2", "enum3", "enum4", "enum5", "enum6", "enum7", "enum8", "enum9"
          )

          // Add 2 enum data values
          val List(e1, e2) = coreConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enums", ":Ns.enums/enum1"),
              Util.list(":db/add", "#db/id[:db.part/user -1000002]", ":Ns/enums", ":Ns.enums/enum7")
            ).asInstanceOf[util.List[AnyRef]]
          ).eids

          // Live data
          coreConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enums ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==>
            List(List(Set("enum1", "enum7")))

          // Can't remove enum value if it has been asserted ("enum7" is now obsolete)
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enums", "enums",
            2, "Enum", Seq(
              "enum0", "enum1", "enum2", // keep some existing enum values
              "man", "woman" // add new enum values
            )
          ) ==> Left(
            """Successfully rolled back from error: Couldn't remove obsolete enums having live values. Please remove values before removing enums from schema.
              |Conflicting obsolete enum values: enum7""".stripMargin)

          // Successfully rolled back (attribute `enum` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enums").pos.card.tpe.enums.get(moleculeAdminConn) ==> List(
            (27, 2, "String", Set("enum1", "enum4", "enum9", "enum3", "enum8", "enum6", "enum2", "enum7", "enum0", "enum5"))
          )

          // live schema
          Schema.attr("enums").card.tpe.enum.get(coreConn).sortBy(_._4) ==> List(
            ("enums", "many", "ref", "enum0"),
            ("enums", "many", "ref", "enum1"),
            ("enums", "many", "ref", "enum2"),
            ("enums", "many", "ref", "enum3"),
            ("enums", "many", "ref", "enum4"),
            ("enums", "many", "ref", "enum5"),
            ("enums", "many", "ref", "enum6"),
            ("enums", "many", "ref", "enum7"),
            ("enums", "many", "ref", "enum8"),
            ("enums", "many", "ref", "enum9"),
          )


          // Retract obsolete enum data value so that we can remove the enum schema value
          coreConn.transact(
            Util.list(
              Util.list(s":db/retract", e2.asInstanceOf[AnyRef], ":Ns/enums", ":Ns.enums/enum7")
            ).asInstanceOf[util.List[AnyRef]]
          )

          // Now we can also remove the `enum7` schema enum value
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "enums", "enums",
            2, "Enum", Seq(
              "enum0", "enum1", "enum2", // keep some existing enum values
              "man", "woman" // add new enum values
            )
          ) ==> Right(
            MetaSchema(List(
              Part(1, "db.part/user", None, None, List(
                Ns(1, "Ns", "Ns", None, None, List(
                  Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, List()),
                  Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, List()),
                  Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(12, "enum", 1, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(14, "ref1", 1, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, List()),

                  Attr(27, "enums", 2, "String", Some(Set(
                    "enum0", "enum1", "enum2",
                    "man", "woman"
                  )), None, None, None, None, None, None, None, List()),

                  Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(31, "strMap", 3, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Ref1", "Ref1", None, None, List(
                  Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, List()),
                  Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, List()),
                  Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()))),
                Ns(3, "Ref2", "Ref2", None, None, List(
                  Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List()),
                  Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
                  Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, List()),
                  Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List())))))
            ))
          )

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("enums").enums.get(moleculeAdminConn) ==> List(Set("enum1", "man", "woman", "enum2", "enum0"))

          // live
          Schema.a_(":Ns/enums").enum.get(coreConn) ==> List("woman", "enum2", "man", "enum1", "enum0")

          // New enums can now be asserted
          coreConn.transact(
            Util.list(
              Util.list(":db/add", "#db/id[:db.part/user -1000001]", ":Ns/enums", ":Ns.enums/woman")
            ).asInstanceOf[util.List[AnyRef]]
          ).eids

          // Value is saved
          coreConn.q(s"[:find (distinct ?enum) :where [?e :Ns/enums ?ref] [?ref :db/ident ?ident] [(name ?ident) ?enum]]") ==> List(
            List(Set("enum1", "woman")))

          Nsx.enums.get(coreConn) ==> List(Set("enum1", "woman"))
        }
      }


      test("Ref ns") {

        test("Validation") {
          val ps = new PartitionSetup
          import ps._
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "ref1", "ref1",
            1, "ref", Nil, Some("xx")) ==> Left(
            "Successfully rolled back from error: Couldn't find ref namespace `xx` in client schema."
          )

          // Successfully rolled back (attribute `ref1` unchanged)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("ref1").pos.card.tpe.get(moleculeAdminConn) ==> List((14, 1, "ref"))

          // live schema
          Schema.attr("ref1").card.tpe.get(coreConn) ==> List(("ref1", "one", "ref"))
        }


        test("Within partition") {
          val ps = new PartitionSetup1
          import ps._

          // Add `refAttr` pointing to `Bc`
          val schema1: MetaSchema = createAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "refAttr",
            1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

          // Let `refAttr` point to Bd instead
          updateAttribute(schema1, "Partition1", "b", "Bb", "refAttr", "refAttr",
            1, "ref", Nil, Some("b_Bd")) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List()))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "refAttr", 1, "ref", None, Some("b_Bd"), None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )

          read ! partition1DefFilePath ==>
            """package db.migration.schema
              |import molecule.schema.definition._
              |
              |@InOut(0, 5)
              |object Partition1Definition {
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
              |      val bb3     = oneInt
              |      val bb4     = oneInt
              |      val refAttr = one[Bd]
              |    }
              |
              |    trait Bc {
              |      val bc1 = oneInt
              |    }
              |
              |    trait Bd {
              |      val bd1 = oneInt
              |    }
              |  }
              |
              |  object c {
              |
              |  }
              |}
              |""".stripMargin

          meta_Db.name_("Partition1").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.get(moleculeAdminConn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

          Schema.ns_("Bb").attr.get(partition1Conn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

          // Data - we can use the new reference
          partition1Conn.transact(
            list(
              list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 42.asInstanceOf[Object]),
              list(":db/add", "#db/id[:b -1000001]", ":b_Bb/refAttr", "#db/id[:b -1000002]"),
              list(":db/add", "#db/id[:b -1000002]", ":b_Bc/bc1", 43.asInstanceOf[Object])
            ).asInstanceOf[util.List[AnyRef]]
          )
          partition1Conn.q(
            """[:find  ?b ?d
              | :where [?a :b_Bb/bb1 ?b]
              |        [?a :b_Bb/refAttr ?c]
              |        [?c :b_Bc/bc1 ?d]]""".stripMargin) ==> List(List(42, 43))
        }


        test("To other partition") {
          val ps = new PartitionSetup1
          import ps._

          // Add `refAttr` pointing to `Bc`
          val schema1: MetaSchema = createAttribute(partition1MetaSchema, "Partition1", "b", "Bb", "refAttr",
            1, "ref", Nil, Some("b_Bc")).getOrElse(MetaSchema(Nil))

          // Add attribute to namespace `Aa`
          val schema2: MetaSchema = createAttribute(schema1, "Partition1", "a", "Aa", "aa1",
            1, "Int").getOrElse(MetaSchema(Nil))

          // Let `refAttr` point to Bd instead
          updateAttribute(schema2, "Partition1", "b", "Bb", "refAttr", "refAttr",
            1, "ref", Nil, Some("a_Aa")) ==> Right(
            MetaSchema(List(
              Part(1, "a", None, None, List(
                Ns(1, "Aa", "a_Aa", None, None, List(
                  Attr(1, "aa1", 1, "Int", None, None, None, None, None, None, None, None, List())
                )))),
              Part(2, "b", None, None, List(
                Ns(1, "Bb", "b_Bb", None, None, List(
                  Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "bb3", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "bb4", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "refAttr", 1, "ref", None, Some("a_Aa"), None, None, None, None, None, None, List())
                )),
                Ns(2, "Bc", "b_Bc", None, None, List(
                  Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
                Ns(3, "Bd", "b_Bd", None, None, List(
                  Attr(1, "bd1", 1, "Int", None, None, None, None, None, None, None, None, List())))
              )),
              Part(3, "c", None, None, List())
            ))
          )

          read ! partition1DefFilePath ==>
            """package db.migration.schema
              |import molecule.schema.definition._
              |
              |@InOut(0, 5)
              |object Partition1Definition {
              |
              |  object a {
              |
              |    trait Aa {
              |      val aa1 = oneInt
              |    }
              |  }
              |
              |  object b {
              |
              |    trait Bb {
              |      val bb1     = oneInt
              |      val bb2     = oneInt
              |      val bb3     = oneInt
              |      val bb4     = oneInt
              |      val refAttr = one[a.Aa]
              |    }
              |
              |    trait Bc {
              |      val bc1 = oneInt
              |    }
              |
              |    trait Bd {
              |      val bd1 = oneInt
              |    }
              |  }
              |
              |  object c {
              |
              |  }
              |}
              |""".stripMargin

          meta_Db.name_("Partition1").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name.get(moleculeAdminConn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

          Schema.ns_("Bb").attr.get(partition1Conn).sorted ==> List("bb1", "bb2", "bb3", "bb4", "refAttr")

          // Data - we can use the new reference
          partition1Conn.transact(
            list(
              list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 42.asInstanceOf[Object]),
              list(":db/add", "#db/id[:b -1000001]", ":b_Bb/refAttr", "#db/id[:b -1000002]"),
              list(":db/add", "#db/id[:b -1000002]", ":a_Aa/aa1", 43.asInstanceOf[Object])
            ).asInstanceOf[util.List[AnyRef]]
          )
          partition1Conn.q(
            """[:find  ?b ?d
              | :where [?a :b_Bb/bb1 ?b]
              |        [?a :b_Bb/refAttr ?c]
              |        [?c :a_Aa/aa1 ?d]]""".stripMargin) ==> List(List(42, 43))
        }


        test("No partitions") {
          val ps = new PartitionSetup
          import ps._
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "ref1", "ref1",
            1, "ref", Nil, Some("Ref2")) ==> Right(
            MetaSchema(List(
              Part(1, "db.part/user", None, None, List(
                Ns(1, "Ns", "Ns", None, None, List(
                  Attr(1, "str", 1, "String", None, None, Some(Set("fulltext")), Some("Card one String attribute"), None, None, None, None, List()),
                  Attr(2, "int", 1, "Int", None, None, None, Some("Card one Int attribute"), None, None, None, None, List()),
                  Attr(3, "long", 1, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(4, "float", 1, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "double", 1, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(6, "bool", 1, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(7, "bigInt", 1, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "bigDec", 1, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(9, "date", 1, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(10, "uuid", 1, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(11, "uri", 1, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(12, "enum", 1, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(13, "parent", 1, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),

                  // Ref namespace changed from `ref1` to `ref2`
                  Attr(14, "ref1", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),

                  Attr(15, "refSub1", 1, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(16, "strs", 2, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(17, "ints", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(18, "longs", 2, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(19, "floats", 2, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(20, "doubles", 2, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(21, "bools", 2, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(22, "bigInts", 2, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(23, "bigDecs", 2, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(24, "dates", 2, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(25, "uuids", 2, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(26, "uris", 2, "URI", None, None, None, None, None, None, None, None, List()),
                  Attr(27, "enums", 2, "String", Some(Set("enum4", "enum6", "enum1", "enum3", "enum5", "enum0", "enum2", "enum7", "enum9", "enum8")), None, None, None, None, None, None, None, List()),
                  Attr(28, "parents", 2, "ref", None, Some("Ns"), None, None, None, None, None, None, List()),
                  Attr(29, "refs1", 2, "ref", None, Some("Ref1"), None, None, None, None, None, None, List()),
                  Attr(30, "refsSub1", 2, "ref", None, Some("Ref1"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(31, "strMap", 3, "String", None, None, Some(Set("fulltext")), None, Some(""), None, None, None, List()),
                  Attr(32, "intMap", 3, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(33, "longMap", 3, "Long", None, None, None, None, None, None, None, None, List()),
                  Attr(34, "floatMap", 3, "Float", None, None, None, None, None, None, None, None, List()),
                  Attr(35, "doubleMap", 3, "Double", None, None, None, None, None, None, None, None, List()),
                  Attr(36, "boolMap", 3, "Boolean", None, None, None, None, None, None, None, None, List()),
                  Attr(37, "bigIntMap", 3, "BigInt", None, None, None, None, None, None, None, None, List()),
                  Attr(38, "bigDecMap", 3, "BigDecimal", None, None, None, None, None, None, None, None, List()),
                  Attr(39, "dateMap", 3, "Date", None, None, None, None, None, None, None, None, List()),
                  Attr(40, "uuidMap", 3, "UUID", None, None, None, None, None, None, None, None, List()),
                  Attr(41, "uriMap", 3, "URI", None, None, None, None, None, None, None, None, List()))),
                Ns(2, "Ref1", "Ref1", None, None, List(
                  Attr(1, "str1", 1, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(2, "int1", 1, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(3, "enum1", 1, "String", Some(Set("enum12", "enum10", "enum11")), None, None, None, None, None, None, None, List()),
                  Attr(4, "ref2", 1, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(5, "refSub2", 1, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()),
                  Attr(6, "strs1", 2, "String", None, None, None, None, Some(""), None, None, None, List()),
                  Attr(7, "ints1", 2, "Int", None, None, None, None, None, None, None, None, List()),
                  Attr(8, "refs2", 2, "ref", None, Some("Ref2"), None, None, None, None, None, None, List()),
                  Attr(9, "refsSub2", 2, "ref", None, Some("Ref2"), Some(Set("isComponent")), None, None, None, None, None, List()))),
                Ns(3, "Ref2", "Ref2", None, None, List(
                  Attr(1, "str2", 1, "String", None, None, Some(Set("uniqueIdentity")), None, None, None, None, None, List()),
                  Attr(2, "int2", 1, "Int", None, None, Some(Set("uniqueValue")), None, None, None, None, None, List()),
                  Attr(3, "enum2", 1, "String", Some(Set("enum22", "enum20", "enum21")), None, None, None, None, None, None, None, List()),
                  Attr(4, "strs2", 2, "String", None, None, None, None, None, None, None, None, List()),
                  Attr(5, "ints2", 2, "Int", None, None, Some(Set("noHistory")), None, None, None, None, None, List())))))
            ))
          )
        }
      }


      test("Options") {

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
          Schema.attr("int").card.tpe.index$.unique$.fulltext$.isComponent$.noHistory$.get(coreConn) ==> List(
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
          Schema.attr("int").card.tpe.index$.unique$.fulltext$.isComponent$.noHistory$.get(coreConn) ==> List(
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
          Schema.attr("int").card.tpe.index$.unique$.fulltext$.isComponent$.noHistory$.get(coreConn) ==> List(
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
          updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ref1", "str1", "str1",
            1, "String", Nil, None, Seq("fulltext")) ==> Left(
            "Successfully rolled back from error: Can't add fulltext option to existing attribute."
          )

          // Successfully rolled back (attribute `str1` unchanged. `fulltext` option discarded)

          // client
          getMetaSchema("CoreTest") ==> coreMetaSchema

          // def file
          read ! coreDefFilePath ==> coreDefFile

          // meta
          meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ref1").Attrs.name_("str1").pos.card.tpe.options$.get(moleculeAdminConn) ==> List((1, 1, "String", Some(Set("indexed"))))

          // live schema
          Schema.attr("str1").card.tpe.fulltext$.get(coreConn) ==> List(("str1", "one", "string", None))


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
          Schema.attr("str").card.tpe.fulltext$.get(coreConn) ==> List(("str", "one", "string", Some(true)))
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
      }


      test("Doc") {

        test("Add txt") {
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
      }


      test("Name") {
        val ps = new PartitionSetup
        import ps._

        // Rename attribute
        val schema1 = updateAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1", "bb7", 1, "Int").getOrElse(MetaSchema(Nil))
        schema1 ==> MetaSchema(List(
          Part(1, "a", None, None, List(
            Ns(1, "Aa", "a_Aa", None, None, List()))),
          Part(2, "b", None, None, List(
            Ns(1, "Bb", "b_Bb", None, None, List(
              Attr(1, "bb7", 1, "Int", None, None, None, None, None, None, None, None, List()),
              Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()))),
            Ns(2, "Bc", "b_Bc", None, None, List(
              Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
          Part(3, "c", None, None, List())
        ))


        // Create new attribute with previous name of renamed attribute.
        // This can be a two-step way of changing a value type of an attribute (history will be gone though)
        createAttribute(schema1, "Partition", "b", "Bb", "bb1", 1, "String") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb7", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(2, "bb2", 1, "Int", None, None, None, None, None, None, None, None, List()),
                Attr(3, "bb1", 1, "String", None, None, None, None, None, None, None, None, List())
              )),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List())
          ))
        )
      }
    } // Update


    test("Delete") {

      test("With data") {
        val ps = new PartitionSetup
        import ps._

        // Assert value for attribute `bb1`
        partitionConn.transact(list(list(":db/add", "#db/id[:b -1000001]", ":b_Bb/bb1", 42.asInstanceOf[Object])).asInstanceOf[util.List[AnyRef]])
        partitionConn.q("[:find ?value :where [_ :b_Bb/bb1 ?value]]") ==> List(List(42))

        // Can't delete partition with attribute `bb1` having asserted value
        deleteAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb1") ==> Left(
          "Can't delete attribute `bb1` asserted with 1 entities. Please retract or transfer attribute value(s) first."
        )

        // Successfully rolled back (attribute `bb1` unchanged)

        // client
        getMetaSchema("Partition") ==> partitionMetaSchema

        // def file
        read ! partitionDefFilePath ==> partitionDefFile

        // meta
        meta_Db.name_("Partition").Partitions.name_("b").Namespaces.name_("Bb").Attrs.name_("bb1").pos.card.tpe.get(moleculeAdminConn) ==> List((1, 1, "Int"))

        // live schema
        Schema.attr("bb1").card.tpe.get(partitionConn) ==> List(("bb1", "one", "long"))

        // Data intact
        partitionConn.q("[:find ?value :where [_ :b_Bb/bb1 ?value]]") ==> List(List(42))
      }


      test("Without data") {
        val ps = new PartitionSetup
        import ps._

        // Deleting attribute without value is ok
        deleteAttribute(partitionMetaSchema, "Partition", "b", "Bb", "bb2") ==> Right(
          MetaSchema(List(
            Part(1, "a", None, None, List(
              Ns(1, "Aa", "a_Aa", None, None, List()))),
            Part(2, "b", None, None, List(
              Ns(1, "Bb", "b_Bb", None, None, List(
                Attr(1, "bb1", 1, "Int", None, None, None, None, None, None, None, None, List()))),
              Ns(2, "Bc", "b_Bc", None, None, List(
                Attr(1, "bc1", 1, "Int", None, None, None, None, None, None, None, None, List()))))),
            Part(3, "c", None, None, List())
          ))
        )
      }
    }


    test("Restore") {
      ResetDbs.resetDbs()
    }
  }
}