package moleculeadmin.sharedtest.query.tree

import moleculeadmin.shared.ast.metaSchema._
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
          List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
          List(
            "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
            "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
            "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("bc"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())),
                List(
                  "ca" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cb" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "Dummy to keep ns open", "", 0, NoValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("bc"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bc" -> "Ccc"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())),
                List(
                  "ca" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cb" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "cd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab", "ac"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List(),
            List()),
          Tree(List("" -> "Aaa", "ac" -> "Ccc"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())),
            List(
              "ca" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cb" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ba"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ad"),
                List(
                  Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd"),
                    List(
                      MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                    List(
                      "da" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "db" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "dc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ba", "bd"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ad"),
                List(
                  Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd"),
                    List(
                      MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                    List(
                      "da" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "db" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "dc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List()))),
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bd" -> "Ddd"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                List(
                  "da" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "db" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "dc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
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
          MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
          MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
          MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
        List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
        List(
          "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
          "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
            MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
            MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
            MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
        List("ab", "ac", "ad"),
        List(
          Tree(List("" -> "Aaa", "ab" -> "Bbb"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
            List(
              "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ba", "bd"),
            List(
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ad"),
                List(
                  Tree(List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa", "ad" -> "Ddd"),
                    List(
                      MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                    List(
                      "da" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "db" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "dc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List()))),
              Tree(List("" -> "Aaa", "ab" -> "Bbb", "bd" -> "Ddd"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
                List(
                  "da" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "db" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "dc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List()))),
          Tree(List("" -> "Aaa", "ac" -> "Ccc"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())),
            List(
              "ca" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cb" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "cd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("ca", "cb"),
            List(
              Tree(List("" -> "Aaa", "ac" -> "Ccc", "ca" -> "Aaa"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                List(
                  "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List()),
              Tree(List("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
                List(
                  "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List("ba"),
                List(
                  Tree(List("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb", "ba" -> "Aaa"),
                    List(
                      MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                      MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                      MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                    List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())),
                    List(
                      "ab" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "ac" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                      "ad" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                        MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                        MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                        MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                    List(),
                    List()))))),
          Tree(List("" -> "Aaa", "ad" -> "Ddd"),
            List(
              MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
              MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
              MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)),
            List(Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())),
            List(
              "da" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "db" -> MetaNs(2, "Bbb", "Bbb", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
              "dc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
            List("db"),
            List(
              Tree(List("" -> "Aaa", "ad" -> "Ddd", "db" -> "Bbb"),
                List(
                  MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                  MetaAttr(1, "attrB", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(2, "ba", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(3, "bc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                  MetaAttr(4, "bd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil)),
                List(Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())),
                List(
                  "ba" -> MetaNs(1, "Aaa", "Aaa", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrA", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ab", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "ac", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "ad", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bc" -> MetaNs(3, "Ccc", "Ccc", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrC", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "ca", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "cb", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "cd", 1, "ref", None, Some("Ddd"), Some(Set("indexed")), None, None, None, None, None, Nil))),
                  "bd" -> MetaNs(4, "Ddd", "Ddd", None, None, List(
                    MetaAttr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil),
                    MetaAttr(1, "attrD", 1, "String", None, None, Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(2, "da", 1, "ref", None, Some("Aaa"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(3, "db", 1, "ref", None, Some("Bbb"), Some(Set("indexed")), None, None, None, None, None, Nil),
                    MetaAttr(4, "dc", 1, "ref", None, Some("Ccc"), Some(Set("indexed")), None, None, None, None, None, Nil)))),
                List(),
                List())))))
    }
  }
}