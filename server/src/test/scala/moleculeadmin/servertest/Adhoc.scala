package moleculeadmin.servertest

import db.core.dsl.coreTest._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.server.utils.DateStrLocal
import moleculeadmin.shared.ast.schema.MetaSchema
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData}
import moleculeadmin.shared.util.HelpersAdmin
import utest._
import scala.languageFeature.implicitConversions._


object Adhoc extends TestSuite
  with HelpersAdmin
  with ExampleData
  //  with TreeSchema
  //  with mBrainzSchema
  with CoreSchema
  //with MoleculeAdminSchema
  with DateStrLocal
  with ModelOps
  with ColOps {

  val base = "datomic:free://localhost:4334"


  val tests = Tests {

    test("Adhoc") {

      //      implicit val conn = recreateDbFrom(CoreTestSchema, "localhost:4334/CoreTest", protocol)
      implicit val conn = Conn(base + "/MoleculeAdmin")
      //      implicit val conn = Conn(base + "/mbrainz-1968-1973")
      //      implicit val conn = Conn("datomic:free://localhost:4334/Clazzig")


      // in-memory db
      //      implicit val conn = recreateDbFrom(CoreTestSchema)
      //      implicit val conn = Conn(base + "/CoreTest")


      //      Schema.a.get foreach println

//      meta_Db.name("CoreTest").Partitions.Namespaces.name.get foreach println

      val ns = meta_Db.name_("CoreTest").Partitions.Namespaces.e.name_("Ns").get.head

      meta_Namespace(ns).Attrs.name.options$.get.sortBy(_._1) foreach println

      //      meta_Attribute.a("s2")

    }
  }
}