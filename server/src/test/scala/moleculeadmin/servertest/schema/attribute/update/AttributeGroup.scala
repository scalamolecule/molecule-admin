package moleculeadmin.servertest.schema.attribute.update

import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.metaSchema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeGroup extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Empty header") {
      val ps = new Partition1Setup
      import ps._

      updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
        "bb2", "bb2", 1, "Int", Nil, None, Nil, None, 2, Some("")) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, Some(""), None, None, None, List()),
              MetaAttr(3, "bb3", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(4, "bb4", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())
            )),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bd", "b_Bd", None, None, List(
              MetaAttr(1, "bd1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())))
          )),
          MetaPart(3, "c", None, None, List())
        )
        ))
    }

    test("Header") {
      val ps = new Partition1Setup
      import ps._

      updateAttribute(partition1MetaSchema, "Partition1", "b", "Bb",
        "bb3", "bb3", 1, "Int", Nil, None, Nil, None, 3, Some("Attr group header...")) ==> Right(
        MetaSchema(List(
          MetaPart(1, "a", None, None, List(
            MetaNs(1, "Aa", "a_Aa", None, None, List()))),
          MetaPart(2, "b", None, None, List(
            MetaNs(1, "Bb", "b_Bb", None, None, List(
              MetaAttr(1, "bb1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(2, "bb2", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()),
              MetaAttr(3, "bb3", 1, "Int", Nil, None, Nil, None, Some("Attr group header..."), None, None, None, List()),
              MetaAttr(4, "bb4", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())
            )),
            MetaNs(2, "Bc", "b_Bc", None, None, List(
              MetaAttr(1, "bc1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List()))),
            MetaNs(3, "Bd", "b_Bd", None, None, List(
              MetaAttr(1, "bd1", 1, "Int", Nil, None, Nil, None, None, None, None, None, List())))
          )),
          MetaPart(3, "c", None, None, List())
        ))
      )
    }

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition1"))
    }
  }
}