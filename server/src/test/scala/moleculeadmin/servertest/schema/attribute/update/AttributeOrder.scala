package moleculeadmin.servertest.schema.attribute.update

import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeOrder extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

    test("Unchanged") {
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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
      val ps = new Partition1Setup
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

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition1"))
    }
  }
}