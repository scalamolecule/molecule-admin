package moleculeadmin.sharedtest.ops.query.branch

import molecule.ast.model._
import moleculeadmin.shared.ops.query.builder.TreeOps
import scala.languageFeature.implicitConversions._
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object RemoveBranch extends TestSuite with TreeSchema with TreeOps {

  val tests = Tests {

    test("bond") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        //        Bond("Aaa", "ab", "Bbb", 1, List()),
        //        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )
    }


    test("attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
        //        Bond("Aaa", "ab", "Bbb", 1, List()),
        //        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )
    }


    test("bond bond") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //      Bond("Aaa", "ab", "Bbb", 1, List()),
        //      Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //      Bond("Bbb", "bc", "Ccc", 1, List()),
        //      Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc")) ==> List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        //        Bond("Bbb", "bc", "Ccc", 1, List()),
        //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
    }


    test("bond bond attr") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //      Bond("Aaa", "ab", "Bbb", 1, List()),
        //      Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //      Bond("Bbb", "bc", "Ccc", 1, List()),
        //      Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc")) ==> List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        //        Bond("Bbb", "bc", "Ccc", 1, List()),
        //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
    }


    test("attr bond bond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      Bond("Aaa", "ab", "Bbb", 1, List()),
        //      Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //      Bond("Bbb", "bc", "Ccc", 1, List()),
        //      Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        //        Bond("Bbb", "bc", "Ccc", 1, List()),
        //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
    }


    test("attr bond attr bond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      Bond("Aaa", "ab", "Bbb", 1, List()),
        //      Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        //        Bond("Bbb", "bc", "Ccc", 1, List()),
        //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        //        Bond("Bbb", "bc", "Ccc", 1, List()),
        //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
    }


    test("attr bond dummy rebond, before rebond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //        Bond("Aaa", "ab", "Bbb", 1, List()),
        //        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )
    }


    test("attr bond attr rebond, before rebond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //        Bond("Aaa", "ab", "Bbb", 1, List()),
        //        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )
    }


    test("attr bond dummy rebond, after rebond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ac" -> "Ccc")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        //        ReBond("Aaa"),
        //        Bond("Aaa", "ac", "Ccc", 1, List()),
        //        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )
    }


    test("attr bond attr rebond, after rebond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      removeBranch(model, List("" -> "Aaa", "ac" -> "Ccc")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //        ReBond("Aaa"),
        //        Bond("Aaa", "ac", "Ccc", 1, List()),
        //        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )
    }


    test("Complex example") {

      // See bottom of ModelTree test for indented model (easier to orient with)

      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ab" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      Bond("Aaa", "ab", "Bbb", 1, List()),
        //      Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //      Bond("Bbb", "ba", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      Bond("Aaa", "ad", "Ddd", 1, List()),
        //      Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Aaa"),
        //      ReBond("Bbb"),
        //      Bond("Bbb", "bd", "Ddd", 1, List()),
        //      Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Bbb"),
        //      ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //      Bond("Bbb", "ba", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      Bond("Aaa", "ad", "Ddd", 1, List()),
        //      Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Aaa"),
        //      ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      Bond("Aaa", "ad", "Ddd", 1, List()),
        //      Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ab" -> "Bbb", "bd" -> "Ddd")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        //      Bond("Bbb", "bd", "Ddd", 1, List()),
        //      Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ac" -> "Ccc")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        //      Bond("Aaa", "ac", "Ccc", 1, List()),
        //      Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        //      Bond("Ccc", "ca", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Ccc"),
        //      Bond("Ccc", "cb", "Bbb", 1, List()),
        //      Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //      Bond("Bbb", "ba", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Bbb"),
        //      ReBond("Ccc"),
        //      ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ac" -> "Ccc", "ca" -> "Aaa")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        //      Bond("Ccc", "ca", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        //      Bond("Ccc", "cb", "Bbb", 1, List()),
        //      Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //      Bond("Bbb", "ba", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Bbb"),
        //      ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb", "ba" -> "Aaa")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        //      Bond("Bbb", "ba", "Aaa", 1, List()),
        //      Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        Bond("Ddd", "db", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ad" -> "Ddd")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        //      ReBond("Bbb"),
        //      ReBond("Ccc"),
        //      ReBond("Aaa"),
        //      Bond("Aaa", "ad", "Ddd", 1, List()),
        //      Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      Bond("Ddd", "db", "Bbb", 1, List()),
        //      Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      removeBranch(model, Seq("" -> "Aaa", "ad" -> "Ddd", "db" -> "Bbb")) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        ReBond("Bbb"),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        Bond("Ccc", "ca", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Ccc"),
        Bond("Ccc", "cb", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Ccc"),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        //      Bond("Ddd", "db", "Bbb", 1, List()),
        //      Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )
    }
  }
}