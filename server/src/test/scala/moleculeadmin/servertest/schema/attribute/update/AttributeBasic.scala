package moleculeadmin.servertest.schema.attribute.update

import molecule.util.Helpers
import moleculeadmin.servertest._
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AttributeBasic extends TestSuite with TreeSchema with Helpers {

  val tests = Tests {

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

    test("Restore") {
      new ResetDbsCmds {}.resetDbs(Seq("Partition"))
    }
  }
}