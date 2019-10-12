package moleculeadmin.sharedtest.ops.transform.conversion

import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin
import moleculeadmin.shared.ops.transform.{Model2Molecule, Molecule2Model}
import moleculeadmin.shared.testdata.SeattleSchema
import utest._
import scala.languageFeature.implicitConversions._


object SeattleMolecules extends TestSuite with BaseApi with SeattleSchema with HelpersAdmin with Model2Molecule {

  val tests = Tests {

    //  test("Extract molecules") {
    //
    //    val path = "/Users/mg/molecule/molecule/examples/src/test/scala/molecule/examples/seattle/SeattleTests.scala"
    //    val src = Source.fromFile(new File(path))
    //    try {
    //      val lines: List[String] = src.getLines().toList
    //
    //      val molecules = lines.flatMap {
    //        case r".*(Community.*)$m\.get.*" => Some(m)
    //        case _                           => None
    //      }.distinct.sorted
    //
    //      println(molecules.mkString("Seq(\n  \"\"\"", "\"\"\",\n  \"\"\"", "\"\"\"\n)"))
    //
    //    } finally {
    //      src.close()
    //    }
    //  }

    val allMolecules = Seq(
      //    """Community(communityId).name""",
      """Community.category""",
      """Community.e.name_""",
      """Community.e.name_("belltown")""",
      """Community.name""",
      """Community.name < "C"""",
      """Community.name("belltown 2").`type`.url.category""",
      """Community.name("belltown 3")""",
      """Community.name("belltown 3").`type`""",
      """Community.name("belltown 3").`type`.url.category""",
      """Community.name.<("C")""",
      """Community.name.Neighborhood.District.region""",
      """Community.name.Neighborhood.District.region_("ne" or "sw")""",
      """Community.name.Neighborhood.District.region_("ne")""",
      """Community.name.`type`("twitter")""",
      """Community.name.category("news" or "arts")""",
      """Community.name.category("shopping")""",
      """Community.name.category_("news" or "arts")""",
      """Community.name.category_("news", "arts")""",
      """Community.name.category_("restaurants" and "shopping")""",
      """Community.name.contains("AAA").url.`type`.orgtype.category.Neighborhood.name.District.name.region""",
      """Community.name.contains("Com")""",
      """Community.name.contains("DDD").url.`type`.orgtype.category.Neighborhood.name.District.name.region""",
      """Community.name.type_("twitter" or "facebook_page")""",
      """Community.name.type_("twitter" or "facebook_page").Neighborhood.District.region_("sw" or "s" or "se")""",
      """Community.name.type_("twitter")""",
      """Community.name.type_("website").category.contains("food")""",
      """Community.name.type_("website").category.contains("food", "shopping")""",
      //    """Community.name.type_(tw)""",
      """Community.name.url""",
      """Community.name_("belltown 2").category""",
      """Community.name_("belltown 2").url""",
      """Community.name_("belltown").category""",
      //    """Community.name_.t"""
    )


    test("test") {

      var errors = 0
      var succes = 0

      val mols = Seq(
        //      """Community.name < "C"""",
        //      """Community.category("news" or "arts")""",
        """Community.name("belltown 3")""",


      )

      allMolecules.foreach { m1 =>
        //    mols.foreach { m1 =>

        println("-------------------")
        Molecule2Model(m1) match {
          case Left(err) =>
            errors += 1
            println(err)

          case Right(elements) =>
            succes += 1
            val m2 = model2molecule(elements)
            if (m1 != m2) {
              println(m1)
              println(m2)
            }
          //          println(Model(elements))
        }


      }
      println("==>==>==>==>==>==>==>==>==>==>")
      println("errors: " + errors)
      println("succes: " + succes)
    }

  }

}

























































