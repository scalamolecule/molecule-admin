package moleculeadmin.servertest

import ammonite.ops.{home, write}
import datomic.Peer
import db.admin.dsl.moleculeAdmin._
import db.admin.schema.MoleculeAdminSchema
import db.core.dsl.coreTest._
import db.core.dsl.tree._
import db.core.schema.{CoreTestSchema, CoreTestSchema2, TreeSchema}
import db.integration.MBrainzSchemaLowerToUpper1
import db.migration.dsl.partition.b_Bb
import db.migration.schema.{Partition1Schema, PartitionSchema}
import molecule.api.out10._
import molecule.boilerplate.attributes.EnumValue
import molecule.facade.Conn
import moleculeadmin.server.QueryBackend
import moleculeadmin.servertest.schema.withPartitions.Settings
import moleculeadmin.shared.testdata.ExampleData
import moleculeadmin.server.utils.DefFile
import utest._

/*
  /Users/mg/clazzig/clazzig/clazzig/server/app/db/schema/ClazzigDefinition.scala
  /Users/mg/molecule/molecule-admin/molecule-admin/server/src/main/scala/db/core/schema/CoreTestDefinition.scala
  /Users/mg/molecule/molecule-admin/molecule-admin/server/src/main/scala/db/migration/schema/PartitionDefinition.scala
  /Users/mg/molecule/molecule-admin/molecule-admin/server/src/main/scala/db/migration/schema/Partition1Definition.scala
  /Users/mg/molecule/molecule-admin/molecule-admin/server/src/main/scala/db/core/schema/TreeDefinition.scala
  /Users/mg/molecule/molecule-admin/molecule-admin/server/src/main/scala/db/integration/schema/MBrainzDefinition.scala
*/

object ResetDbs extends TestSuite with ExampleData with Settings {
  val protocol = "free"
  val base     = "datomic:free://localhost:4334"


  val tests = Tests {
    //        test("Reset all") {
    //          resetDbs()
    //        }

    //        test("Reset all and poplulate") {
    //          resetDbs()
    ////                populateCoreTest(Conn("datomic:free://localhost:4334/CoreTest"))
    //          //      populatePartition(Conn("datomic:free://localhost:4334/Partition"))
    //          //      populateTree(Conn("datomic:free://localhost:4334/Tree"))
    //        }

    //    test("Reset CoreTest") {
    //      resetDbs(Seq("CoreTest"))
    //      populateCoreTest(Conn("datomic:free://localhost:4334/CoreTest"))
    //    }

    test("Reset CoreTest") {
      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)

      Ns.int(1).save
      val tx2 = Ns.int(2).save
      val tx3 = Ns.int(3).save
      Ns.int(4).save

      // group edit
      val (e2, t2, e3, t3) = (tx2.eid, tx2.t, tx3.eid, tx3.t)
      val tx5              = Ns(e2).int(12).update
      val tx6              = Ns(e3).int(13).update
      val undoneTs         = Seq(
        tx6.t << 32 | t3,
        tx5.t << 32 | t2,
      )

      val moleculeAdminConn = Conn(base + "/MoleculeAdmin")

      val dbSettingsId = user_User.username_("admin")
        .DbSettings.e.Db.name_("CoreTest").get(moleculeAdminConn)
      val groupEditId  = user_GroupEdit.t1(t2).t2(t3).save(moleculeAdminConn).eid
      user_DbSettings(dbSettingsId)
        .undoneTs(undoneTs)
        .groupEdits(groupEditId).update(moleculeAdminConn)

      //      (new QueryBackend).getLastTxs("CoreTest", 5, Nil)
    }
  }

  def resetDbs(dbs0: Seq[String] = Nil): Unit = {
    val debug = true

    if (dbs0.isEmpty) {
      if (debug) println("MoleculeAdmin")
      recreateDbFrom(MoleculeAdminSchema, "localhost:4334/MoleculeAdmin", "free")
    } else {
      if (debug) println("Connecting...")
      Conn("datomic:free://localhost:4334/MoleculeAdmin")
    }

    val dbs = if (dbs0.isEmpty) List(
      "CoreTest",
      "Partition",
      "Partition1",
      "Tree",
      "mbrainz-1968-1973"
    ) else dbs0


    dbs.foreach {
      case "CoreTest" =>
        if (debug) println("- CoreTest")
        write.over(coreDefFilePath, coreDefFile)
        DefFile("CoreTest", Some(coreDefFilePath.toString)).saveToMetaDb
        recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)

      case "CoreTest2" =>
        if (debug) println("- CoreTest")
        write.over(coreDefFilePath, coreDefFile)
        DefFile("CoreTest", Some(coreDefFilePath.toString)).saveToMetaDb
        recreateDbFrom(CoreTestSchema2, "localhost:4334/CoreTest2", protocol)

      case "Partition" =>
        if (debug) println("- Partition")
        write.over(partitionDefFilePath, partitionDefFile)
        DefFile("Partition", Some(partitionDefFilePath.toString)).saveToMetaDb
        recreateDbFrom(PartitionSchema, "localhost:4334/Partition", protocol)

      case "Partition1" =>
        if (debug) println("- Partition1")
        write.over(partition1DefFilePath, partition1DefFile)
        DefFile("Partition1", Some(partition1DefFilePath.toString)).saveToMetaDb
        recreateDbFrom(Partition1Schema, "localhost:4334/Partition1", protocol)

      case "Tree" =>
        if (debug) println("- Tree")
        val defFilePath = pwd / 'server / 'src / 'main / 'scala / 'db / 'core / 'schema / "TreeDefinition.scala"
        DefFile("Tree", Some(defFilePath.toString)).saveToMetaDb
        recreateDbFrom(TreeSchema, "localhost:4334/Tree", protocol)

      case "mbrainz-1968-1973" =>
        if (debug) println("- mbrainz-1968-1973")
        val defFilePath = pwd / 'server / 'src / 'main / 'scala / 'db / 'integration / 'schema / "MBrainzDefinition.scala"
        DefFile("mbrainz-1968-1973", Some(defFilePath.toString)).saveToMetaDb
        implicit val mbrainzConn = Conn("datomic:free://localhost:4334/mbrainz-1968-1973")
        if (Schema.a(":Artist/name").get.isEmpty) {
          if (debug) println("MBRAINZ EXTRA TRANSACTIONS...")
          // Add uppercase-namespaced attribute names so that we can access the externally
          // transacted lowercase names with uppercase names of the molecule code.
          mbrainzConn.datomicConn.transact(MBrainzSchemaLowerToUpper1.namespaces).get
        }

      case _ =>
    }
  }

  def populatePartition(implicit conn: Conn): Unit = {
    b_Bb.bb1(42).save
  }

  def populateTree(implicit conn: Conn): Unit = {
    val aaa = Aaa.attrA("a").save.eid
    val bbb = Bbb.attrB("b").save.eid
    val ccc = Ccc.attrC("c").save.eid
    val ddd = Ddd.attrD("d").save.eid

    Aaa(aaa).ab(bbb).ac(ccc).ad(ddd).update
    Bbb(bbb).ba(aaa).bc(ccc).bd(ddd).update
    Ccc(ccc).ca(aaa).cb(bbb).cd(ddd).update
    Ddd(ddd).da(aaa).db(bbb).dc(ccc).update
  }

  def populateCoreTest(implicit conn: Conn): Unit = {
    Ns.str insert List("", " ", "   ", "\n", "  \n  ", "Hello World", "  Hello  \n  World  ")
    Ns.long insert 2L
    Ns.float insert 3.3f
    Ns.double insert 4.4
    Ns.bool insert true
    Ns.bigInt insert List(bigInt1, bigInt2, bigInt3, BigInt(21))
    Ns.bigDec insert bigDec1
    Ns.date insert List(
      date1,
      str2date("2019-09-06 12:17:18.721"),
      str2date("2019-09-06 12:17:18.721 +01:00"),
      str2date("2019-09-06 12:17:18.721 +02:00"),
      str2date("2019-09-06 12:17:18.721 +03:00"),
      new java.util.Date()
    )
    Ns.uuid insert uuid1
    Ns.uri insert uri1
    Ns.enum insert enum1


    Ns.int(1).Ref1.int1(11).Ref2.int2(21).save
    Ns.int(2).Ref1.int1(12).Ref2.int2(22).save
    Ns.int(3).Refs1.int1(13).Refs2.int2(23).save
    Ns.int(4).Refs1.int1(14).Refs2.int2(24).save


    val List(r1, r2, r3) = Ref1.int1.insert(21, 22, 23).eids
    val r4               = Ref1.int1(21).save.eid
    Ns.int(41).refs1(r1).save
    Ns.int(42).refs1(r1).save
    Ns.int(43).refs1(r1).save
    Ns.int(44).refs1(r1, r2, r3, r4).save
    Ns.int(47).refs1(r4).save
    Ns.int(46).refs1(r2).save
    Ns.int(48).refs1(r3).save

    Ns.int.Parent.int insert List((1, 11), (2, 22))

    Ns.int(42).dates(date1).save

    Ns.strs insert List(Set("", " ", " \n ", "a", "b"), Set("c", "Hi there, Ben!\nHi Lisa"))
    Ns.ints insert List(Set(1, 2), Set(3, 4, 5, 6, 7))
    Ns.longs insert List(Set(1L, 2L), Set(3L, 4L, 5L))
    Ns.floats insert List(Set(1.1f, 2.2f), Set(3.3f, 4.4f, 5.5f))
    Ns.doubles insert List(Set(1.1, 2.1), Set(3.3, 4.4, 5.5))
    Ns.bools insert List(Set(true, false), Set(true))
    Ns.dates insert List(Set(date1, date2), Set(date3))
    Ns.uuids insert List(Set(uuid1), Set(uuid2, uuid3))
    Ns.uris insert List(Set(uri1, uri2), Set(uri3))
    Ns.enums insert List(Set(enum1, enum2), Set(enum3))
    Ns.bigInts insert List(Set(bigInt1, bigInt2), Set(bigInt3))
    Ns.bigDecs insert List(Set(bigDec1, bigDec2), Set(bigDec3))

    Ns.strMap insert List(
      Map("a" -> "Hi\nthere", "bb" -> "Go,\ngirl! <..> & "),
      Map("c" -> "Hello", "dd" -> "world")
    )
    Ns.intMap insert Map("a" -> 1, "bb" -> 2)
    Ns.longMap insert Map("a" -> 1L, "bb" -> 2L)
    Ns.floatMap insert Map("a" -> 1.1f, "bb" -> 2.2f)
    Ns.doubleMap insert Map("a" -> 1.1, "bb" -> 2.2)
    Ns.boolMap insert Map("a" -> true, "bb" -> false)
    Ns.dateMap insert Map("a" -> date1, "bb" -> date2)
    Ns.uuidMap insert Map("a" -> uuid1)
    Ns.uriMap insert Map("a" -> uri1, "bb" -> uri2)
    Ns.bigIntMap insert Map("a" -> bigInt1, "bb" -> bigInt2)
    Ns.bigDecMap insert Map("a" -> bigDec1, "bb" -> bigDec2)

    Ns.int.str$ insert List(
      (1, None),
      (1, Some("")),
      (1, Some("c")),
      (2, Some("b")),
      (3, Some("b")),
    )
    Ns.long.int$ insert List((2, None), (4, Some(1)))
    Ns.int.long$ insert List((3, None), (2, Some(4)), (1, Some(3)), (2, Some(3)))
    Ns.int.float$ insert List((4, None), (4, Some(2.0f)))
    Ns.int.double$ insert List((5, None), (5, Some(2.0)))
    Ns.int.bool$ insert List((6, None), (6, Some(true)), (6, Some(false)))
    Ns.int.date$ insert List((7, None), (7, Some(date2)), (42, Some(date1)))
    Ns.int.uuid$ insert List((8, None), (8, Some(uuid2)))
    Ns.int.uri$ insert List((9, None), (9, Some(uri2)))
    Ns.int.enum$ insert List((10, None), (10, Some(enum2)))
    Ns.int.bigInt$ insert List((11, None), (11, Some(bigInt2)))
    Ns.int.bigDec$ insert List((12, None), (12, Some(bigDec2)))

    Ns.int.strs$ insert List((1, None), (1, Some(Set("", "b"))))
    Ns.int.ints$ insert List((2, None), (2, Some(Set(1, 2, 3, 4, 5))))
    Ns.int.longs$ insert List((3, None), (3, Some(Set(21L, 22L))))
    Ns.int.floats$ insert List((4, None), (4, Some(Set(1.0f, 2.0f))))
    Ns.int.doubles$ insert List((5, None), (5, Some(Set(1.0, 2.0))))
    Ns.int.bools$ insert List((6, None), (6, Some(Set(true, false))))
    Ns.int.dates$ insert List((7, None), (7, Some(Set(date1, date2, date3, date4))))
    Ns.int.uuids$ insert List((8, None), (8, Some(Set(uuid1, uuid2))))
    Ns.int.uris$ insert List((9, None), (9, Some(Set(uri1, uri2))))
    Ns.int.enums$ insert List((10, None), (10, Some(Set(enum1, enum2))))
    Ns.int.bigInts$ insert List((11, None), (11, Some(Set(bigInt1, bigInt2))))
    Ns.int.bigDecs$ insert List((12, None), (12, Some(Set(bigDec1, bigDec2))))

    Ns.int.strMap$ insert List((1, None), (1, Some(Map("a" -> "A", "bb" -> "B"))))
    Ns.int.intMap$ insert List((2, None), (2, Some(Map("a" -> 1, "bb" -> 2))))
    Ns.int.longMap$ insert List((3, None), (3, Some(Map("a" -> 1L, "bb" -> 2L))))
    Ns.int.floatMap$ insert List((4, None), (4, Some(Map("a" -> 1.0f, "bb" -> 2.0f))))
    Ns.int.doubleMap$ insert List((5, None), (5, Some(Map("a" -> 1.0, "bb" -> 2.0))))
    Ns.int.boolMap$ insert List((6, None), (6, Some(Map("a" -> true, "bb" -> false))))
    Ns.int.dateMap$ insert List((7, None), (42, Some(Map("a" -> date1))), (7, Some(Map("a" -> date1, "bb" -> date2))))
    Ns.int.uuidMap$ insert List((8, None), (8, Some(Map("a" -> uuid1, "bb" -> uuid2))))
    Ns.int.uriMap$ insert List((9, None), (9, Some(Map("a" -> uri1, "bb" -> uri2))))
    Ns.int.bigIntMap$ insert List((11, None), (11, Some(Map("a" -> bigInt1, "bb" -> bigInt2))))
    Ns.int.bigDecMap$ insert List((12, None), (12, Some(Map("a" -> bigDec1, "bb" -> bigDec2))))


    Ns.long(222).Tx(Ns.str("meta info")).save
    Ns.long(333).Tx(Ns.str("meta with ref").Ref1.int1(444)).save
  }
}
