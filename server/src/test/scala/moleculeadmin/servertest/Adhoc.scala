package moleculeadmin.servertest
import db.admin.dsl.meta.user_ColSetting
import db.core.dsl.coreTest._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.shared.testdata.{CoreSchema, ExampleData, mBrainzSchema}
import moleculeadmin.shared.util.HelpersAdmin
import org.specs2.mutable._
import scala.languageFeature.implicitConversions._


object Adhoc extends Specification
  with HelpersAdmin
  with ExampleData
//  with TreeSchema
  with mBrainzSchema
{

  val base = "datomic:free://localhost:4334"


  "Adhoc" >> {

//          implicit val conn = Conn(base + "/meta")
          implicit val conn = Conn(base + "/CoreTest")
//    implicit val conn = Conn(base + "/mbrainz-1968-1973")

    //      val eid = 716881581322888L
//    val eid = 17592186072911L
//
//    Schema.a.get.sorted foreach println
//
//    println(Schema.a(":Release/country").get)
//    println(Schema.a(":country/US").get)
//    println(Schema.a(":Country/US").get)
//
//
//    println(eid.touchQuoted)

    Ns.e.str.int.debugGet
    Ns.str.int.debugInsert("a", 42)


    ok
  }
}
