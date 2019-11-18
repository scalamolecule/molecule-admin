package moleculeadmin.servertest
import molecule.api.out10._
import molecule.ast.model.{Atom, Bond, EntValue, Generic, ReBond, VarValue}
import molecule.facade.Conn
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema, mBrainzSchema}
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

//    //      implicit val conn = Conn(base + "/CoreTest")
//    implicit val conn = Conn(base + "/mbrainz-1968-1973")
//
//    //      val eid = 716881581322888L
//    val eid = 17592186072911L
//
//    Schema.a.get.sorted foreach println
//
//    println(Schema.a(":Release/country").get)
//    println(Schema.a(":country/US").get)
//
//
//    println(eid.touchQuoted)


    Molecule2Model(
      "Release.name.Country.e.name._Release.Labels.name.Country.e.name"
    ) === Right(List(
      Atom("Release", "name", "String", 1, VarValue, None, Seq(), Seq()),
      Bond("Release", "country", "Country", 1, Seq()),
      Generic("Country", "e", "datom", EntValue),
      Atom("Country", "name", "String", 1, VarValue, None, Seq(), Seq()),
      ReBond("Release"),
      Bond("Release", "labels", "Label", 2, Seq()),
      Atom("Label", "name", "String", 1, VarValue, None, Seq(), Seq()),
      Bond("Label", "country", "Country", 1, Seq()),
      Generic("Country", "e", "datom", EntValue),
      Atom("Country", "name", "String", 1, VarValue, None, Seq(), Seq())
    ))

    ok
  }
}
