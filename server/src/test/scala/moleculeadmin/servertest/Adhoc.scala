package moleculeadmin.servertest
import ammonite.ops.read
import db.admin.dsl.moleculeAdmin.{meta_Db, user_ColSetting}
import db.core.dsl.coreTest._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.servertest.schema.withPartitions.PartitionSetup
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData, mBrainzSchema}
import moleculeadmin.shared.util.HelpersAdmin
import org.specs2.mutable._
import scala.languageFeature.implicitConversions._
import ammonite.ops._


object Adhoc extends Specification
  with HelpersAdmin
  with ExampleData
  //  with TreeSchema
  with mBrainzSchema {

  val base = "datomic:free://localhost:4334"


  "Adhoc" >> {

    //    implicit val conn = Conn(base + "/MoleculeAdmin")
    //    implicit val conn = Conn(base + "/mbrainz-1968-1973")
    implicit val conn = Conn(base + "/CoreTest")


    //    Ns.int(222).Tx(Ns.str("meta info")).save
    //    Ns.int(333).Tx(Ns.str("meta with ref").Ref1.int1(444)).save

    val all = (1 to 1000000).toList

    val t = new Timer
    println(all.contains(987654))
    t.log(1)





    ok
  }

  "One -> map not allowed" >> {
    val ps = new PartitionSetup
    import ps._

    // card one to map cardinality not allowed
    updateAttribute(coreMetaSchema, "CoreTest", "db.part/user", "Ns", "int", "int", 3, "Int") === Left(
      "Successfully rolled back from error: " +
        "Couldn't change to map cardinality since keys are unknown. Please create a new map attribute and populate with keyed data."
    )

    // Successfully rolled back (attribute `int` unchanged)

    // client
    getMetaSchema("CoreTest") === coreMetaSchema

    // def file
    read ! coreDefFilePath === coreDefFile

    // meta
    meta_Db.name_("CoreTest").Partitions.name_("db.part/user").Namespaces.name_("Ns").Attrs.name_("int").pos.card.tpe.get(moleculeAdminConn) === List((2, 1, "Int"))

    // live schema
    Schema.attr("int").card.tpe.get(coreConn) === List(("int", "one", "long"))
  }
}
