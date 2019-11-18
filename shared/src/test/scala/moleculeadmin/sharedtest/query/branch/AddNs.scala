package moleculeadmin.sharedtest.query.branch

import molecule.ast.model._
import molecule.ops.VerifyRawModel
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object AddNs extends TestSuite with TreeSchema with TreeOps {

  val tests = Tests {

    test("dummy > bond") {
      val model = List(
        Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb")(nsMap))).toString ==> Model(List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("attr > bond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("2 attr > bond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("bond > bond") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc"))).toString ==> Model(List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("attr > bond > bond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("attr > bond attr > bond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("bond > bond | later sibling") {
      val model = List(
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ad", "Ddd"))).toString ==> Model(List(
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("bond > bond | prev sibling") {
      val model = List(
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb"))).toString ==> Model(List(
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("attr > bond > bond | later sibling") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ad", "Ddd"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    test("attr > bond > bond | prev sibling") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )).toString
    }


    //  test("42") {
    //    val model = List(
    //      Bond("Aaa", "ab", "Bbb", 1, Seq()),
    //      Atom("Bbb", "attrB", "String", 1, VarValue, None, Seq(), Seq()),
    //      ReBond("Aaa"),
    //      Bond("Aaa", "ac", "Ccc", 1, Seq()),
    //      Atom("Ccc", "attrC", "String", 1, VarValue, None, Seq(), Seq()),
    //    )
    //
    //
    //    Model(ValidatedModelElements(addNs(model, List("" -> "Aaa"), "ab", "Bbb"))).toString ==> Model(List(
    //      Bond("Aaa", "ab", "Bbb", 1, Seq()),
    //      Atom("Bbb", "attrB", "String", 1, VarValue, None, Seq(), Seq()),
    //      ReBond("Aaa"),
    //      Bond("Aaa", "ac", "Ccc", 1, Seq()),
    //      Atom("Ccc", "attrC", "String", 1, VarValue, None, Seq(), Seq()),
    //
    //      ReBond("Aaa"),
    //      Bond("Aaa", "ad", "Ddd", 1, List()),
    //      Atom("Ddd", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
    //    )).toString
    //  }
  }
}