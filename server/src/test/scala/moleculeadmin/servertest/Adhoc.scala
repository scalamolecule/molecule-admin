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
  with mBrainzSchema {

  val base = "datomic:free://localhost:4334"


  "Adhoc" >> {

    //    implicit val conn = Conn(base + "/meta")
    //    implicit val conn = Conn(base + "/mbrainz-1968-1973")
    implicit val conn = Conn(base + "/CoreTest")


    //    Ns.int(222).Tx(Ns.str("meta info")).save
    //    Ns.int(333).Tx(Ns.str("meta with ref").Ref1.int1(444)).save

    val all = (1 to 1000000).toList

    val t = new Timer
    println(all.contains(987654))
    t.log(1)

    var searching = true
    var result    = false
    //    while (searching) {
    //
    //    }

    def length = {
      println("auch")
      3
    }
    var i = 0
    while (i < length) {
      i += 1
    }

    //    println()

    println(result)
    t.log(2)

    ok
  }
}
