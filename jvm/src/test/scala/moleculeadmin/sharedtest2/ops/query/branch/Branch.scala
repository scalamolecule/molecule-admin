package moleculeadmin.sharedtest2.ops.query.branch

import moleculeadmin.shared.lib.molecule.ast.model._
import moleculeadmin.shared.ops.query
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object Branch extends TestSuite with TreeSchema with TreeOps with query.Base {

  val tests = Tests {

    test("1 level") {
      val atom = List(
        Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      isolateBranch(atom, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        ),
        Nil
      )
    }


    test("1 level") {
      val atom = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(atom, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      val atom2 = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(atom2, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }


    test("1 level") {
      val atom = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )


      isolateBranch(atom, List("" -> "Aaa")) ==> (
        List(),
        List(
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(atom, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Bond("Aaa", "ab", "Bbb", 1, List()),
        ),
        List(
          Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        ),
        Nil
      )
    }


    test("2 levels, dummy") {
      val bond = List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(bond, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(bond, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }


    test("2 levels") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(model, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }


    test("3 levels") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(model, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "bc", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "bc", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "bc", "Ccc", 1, List())
        ),
        List(
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }


    test("3 levels with rebond, dummy last") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      isolateBranch(model, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa")
        ),
        List(
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        )
      )

      isolateBranch(model, List("" -> "Aaa", "ac" -> "Ccc")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List())
        ),
        List(
          Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        ),
        Nil
      )
    }


    test("3 levels with rebond, atom last") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(model, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa")
        ),
        List(
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )
      )

      isolateBranch(model, List("" -> "Aaa", "ac" -> "Ccc")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List())
        ),
        List(
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }


    test("3 levels with rebond starting with bond") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      isolateBranch(model, List("" -> "Aaa")) ==> (
        Nil,
        List(
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
        ),
        List(
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        )
      )

      isolateBranch(model, List("" -> "Aaa", "ac" -> "Ccc")) ==> (
        List(
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List())
        ),
        List(
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }


    test("Complex example") {
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

      isolateBranch(model, List("" -> "Aaa")) ==> (
        Nil,
        model,
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List())
        ),
        List(
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
          ReBond("Aaa")
        ),
        List(
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
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "ba", "Aaa", 1, List())
        ),
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ad", "Ddd", 1, List()),
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          ReBond("Bbb")
        ),
        List(
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
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd")) ==> (
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "ba", "Aaa", 1, List()),
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ad", "Ddd", 1, List()),
        ),
        List(
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
        ),
        List(
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
      )

      isolateBranch(model, List("" -> "Aaa", "ab" -> "Bbb", "bd" -> "Ddd")) ==> (
        List(
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
        ),
        List(
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          ReBond("Bbb"),
        ),
        List(
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
      )

      isolateBranch(model, List("" -> "Aaa", "ac" -> "Ccc")) ==> (
        List(
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
        ),
        List(
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
        ),
        List(
          Bond("Aaa", "ad", "Ddd", 1, List()),
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          Bond("Ddd", "db", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        )
      )

      isolateBranch(model, List("" -> "Aaa", "ac" -> "Ccc", "ca" -> "Aaa")) ==> (
        List(
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
        ),
        List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          ReBond("Ccc"),
        ),
        List(
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
      )

      isolateBranch(model, List("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb")) ==> (
        List(
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
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "ba", "Aaa", 1, List()),
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          ReBond("Bbb"),
          ReBond("Ccc"),
        ),
        List(
          ReBond("Aaa"),
          Bond("Aaa", "ad", "Ddd", 1, List()),
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          Bond("Ddd", "db", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        )
      )


      isolateBranch(model, List("" -> "Aaa", "ad" -> "Ddd")) ==> (
        List(
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
        ),
        List(
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          Bond("Ddd", "db", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )

      isolateBranch(model, List("" -> "Aaa", "ad" -> "Ddd", "db" -> "Bbb")) ==> (
        List(
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
        ),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        ),
        Nil
      )
    }
  }
}
