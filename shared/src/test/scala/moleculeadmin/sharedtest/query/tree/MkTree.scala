package moleculeadmin.sharedtest.query.tree

import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.ast.tree.Tree
import molecule.ast.model._
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object MkTree extends TestSuite with TreeSchema with TreeOps {

  val tests = Tests {

    test("1 level") {
      val model = List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
      )

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List(),
        List())
    }


    test("1 level") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      mkTree(mkModelTree(model)) ==>
        Tree(List("" -> "Aaa"),
          List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
          List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
          List(
            "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
            "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
            "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
          List(),
          List())
    }


    test("2 levels, bond dummy") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List())))
    }


    test("3 levels, bond dummy") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("bc"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())),
                List(
                  "ca" -> Ns(1, "Aaa", "Aaa", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cb" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List())))))
    }


    test("2 levels, bond attr") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List())))
    }


    test("2 levels, dummy") {
      val model = List(
        Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List())))
    }


    test("2 levels") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List())))
    }

    test("3 levels") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )


      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("bc"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())),
                List(
                  "ca" -> Ns(1, "Aaa", "Aaa", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cb" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List())))))

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

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab", "ac"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List()),
          Tree(List("" -> "Aaa", "ac" -> "Ccc"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())),
            List(
              "ca" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cb" -> Ns(2, "Bbb", "Bbb", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List())))
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

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ba"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ad"),
                List(
                  Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd"),
                    List(
                      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                    List(
                      "da" -> Ns(1, "Aaa", "Aaa", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "db" -> Ns(2, "Bbb", "Bbb", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "dc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List())))))))
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

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ba", "bd"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ad"),
                List(
                  Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd"),
                    List(
                      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                    List(
                      "da" -> Ns(1, "Aaa", "Aaa", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "db" -> Ns(2, "Bbb", "Bbb", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "dc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List()))),
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bd" -> "Ddd"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                List(
                  "da" -> Ns(1, "Aaa", "Aaa", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "db" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "dc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List())))))
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

      mkTree(mkModelTree(model)) ==> Tree(List("" -> "Aaa"),
        List(
          Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
            Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab", "ac", "ad"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ba", "bd"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ad"),
                List(
                  Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd"),
                    List(
                      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                    List(
                      "da" -> Ns(1, "Aaa", "Aaa", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "db" -> Ns(2, "Bbb", "Bbb", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "dc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List()))),
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bd" -> "Ddd"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                List(
                  "da" -> Ns(1, "Aaa", "Aaa", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "db" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "dc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List()))),
          Tree(List("" -> "Aaa", "ac" -> "Ccc"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())),
            List(
              "ca" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cb" -> Ns(2, "Bbb", "Bbb", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ca", "cb"),
            List(
              Tree(List("" -> "Aaa", "ac" -> "Ccc", "ca" -> "Aaa"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List()),
              Tree(List("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
                List(
                  "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ba"),
                List(
                  Tree(List("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb", "ba" -> "Aaa"),
                    List(
                      Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                    List(
                      "ab" -> Ns(2, "Bbb", "Bbb", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "ac" -> Ns(3, "Ccc", "Ccc", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "ad" -> Ns(4, "Ddd", "Ddd", None, None, List(
                        Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List()))))),
          Tree(List("" -> "Aaa", "ad" -> "Ddd"),
            List(
              Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
            List(
              "da" -> Ns(1, "Aaa", "Aaa", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "db" -> Ns(2, "Bbb", "Bbb", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "dc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("db"),
            List(
              Tree(List("" -> "Aaa", "ad" -> "Ddd", "db" -> "Bbb"),
                List(
                  Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  Attr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  Attr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
                List(
                  "ba" -> Ns(1, "Aaa", "Aaa", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bc" -> Ns(3, "Ccc", "Ccc", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bd" -> Ns(4, "Ddd", "Ddd", None, None, List(
                    Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    Attr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    Attr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List())))))
    }
  }
}