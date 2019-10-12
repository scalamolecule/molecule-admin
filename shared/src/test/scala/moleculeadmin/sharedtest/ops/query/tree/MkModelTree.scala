package moleculeadmin.sharedtest.ops.query.tree

import moleculeadmin.shared.lib.molecule.ast.model._
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object MkModelTree extends TestSuite with TreeSchema with TreeOps {

  val tests = Tests {

    test("1 level") {
      val model = List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
      )
    }

    test("1 level") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )
    }


    test("2 levels, bond dummy") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
        )
      )
    }

    test("2 levels, bond attr") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        )
      )
    }


    test("2 levels, dummy") {
      val model = List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        )
      )
    }


    test("2 levels") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
        )
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

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "bc", "Ccc", 1, List()),
          List(
            Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
          )
        )
      )
    }


    test("3 levels with rebond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa")
        ),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        List(
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )
      )
    }


    test("Complex 1") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "ba", "Aaa", 1, List()),
          List(
            Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
            Bond("Aaa", "ad", "Ddd", 1, List()),
            List(
              Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
            )
          )
        )
      )
    }

    test("Complex 2") {
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
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
      )

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "ba", "Aaa", 1, List()),
          List(
            Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
            Bond("Aaa", "ad", "Ddd", 1, List()),
            List(
              Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
              ReBond("Aaa")
            ),
            ReBond("Bbb")
          ),
          Bond("Bbb", "bd", "Ddd", 1, List()),
          List(
            Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
          )
        )
      )
    }

    test("Complex 3") {
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

      mkModelTree(model) ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        List(
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          Bond("Bbb", "ba", "Aaa", 1, List()),
          List(
            Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
            Bond("Aaa", "ad", "Ddd", 1, List()),
            List(
              Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
              ReBond("Aaa")
            ),
            ReBond("Bbb"),
          ),
          Bond("Bbb", "bd", "Ddd", 1, List()),
          List(
            Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
            ReBond("Bbb")
          ),
          ReBond("Aaa")
        ),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        List(
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
          Bond("Ccc", "ca", "Aaa", 1, List()),
          List(
            Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
            ReBond("Ccc"),
          ),
          Bond("Ccc", "cb", "Bbb", 1, List()),
          List(
            Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
            Bond("Bbb", "ba", "Aaa", 1, List()),
            List(
              Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
              ReBond("Bbb")
            ),
            ReBond("Ccc")
          ),
          ReBond("Aaa")
        ),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        List(
          Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
          Bond("Ddd", "db", "Bbb", 1, List()),
          List(
            Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
          )
        )
      )
    }
  }
}