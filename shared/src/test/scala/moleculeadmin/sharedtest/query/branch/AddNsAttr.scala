package moleculeadmin.sharedtest.query.branch

import molecule.ast.model._
import molecule.ops.exception.VerifyRawModelException
import molecule.ops.VerifyRawModel
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._

object AddNsAttr extends TestSuite with TreeSchema with TreeOps {

  val tests = Tests {

    test("dummy + bond/attr") {
      val model = List(
        Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "attrB"))).toString ==> Model(List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )).toString
    }

    test("attr + bond/e") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "e"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Generic("Bbb", "e", "datom", EntValue)
      )).toString
    }

    test("attr + bond/attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "attrB"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )).toString
    }

    test("attr + bond/refAttr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "bc"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "bc", "ref", 1, VarValue, None, List(), List())
      )).toString
    }


    test("refAttr + bond/e") {
      val model = List(
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "e"))).toString ==> Model(List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Generic("Bbb", "e", "datom", EntValue)
      )).toString
    }

    test("refAttr + bond/attr") {
      val model = List(
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "attrB"))).toString ==> Model(List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Generic("Bbb", "e", "datom", EntValue),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )).toString
    }



    test("e, refAttr + bond, e") {
      val model = List(
        Generic("Aaa", "e", "datom", EntValue),
        Atom("Aaa", "ab", "ref", 1, VarValue, None, Seq(), Seq()))

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "e"))).toString ==> Model(List(
        Generic("Aaa", "e", "datom", EntValue),
        Bond("Aaa", "ab", "Bbb", 1, Seq()),
        Generic("Bbb", "e", "datom", EntValue))).toString
    }

    test("attr, refAttr + bond, attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "attrB"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Generic("Bbb", "e", "datom", EntValue),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("attr, refAttr + bond, attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }

    test("attr, refAttr + bond, attr") {
      val model = List(
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("attr, refAttr + redundant bond, attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List())
      )

      try {
        VerifyRawModel(List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ))
      } catch {
        case e: VerifyRawModelException => e.getMessage ==>
          "Instead of getting the ref id with `ac` please get it via the referenced namespace: `Ac.e ...`"
      }


      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Generic("Ccc", "e", "datom", EntValue),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("attr, refAttr_ + redundant bond, attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac_", "ref", 1, VarValue, None, List(), List())
      )

      try {
        VerifyRawModel(List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Atom("Aaa", "ac_", "ref", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ))
      } catch {
        case e: VerifyRawModelException => e.getMessage ==>
          "Instead of getting the ref id with `ac_` please get it via the referenced namespace: `Ac.e ...`"
      }

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Generic("Ccc", "e", "datom", EntValue),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("attr, refAttr$ + redundant bond, attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac$", "ref", 1, VarValue, None, List(), List())
      )


      try {
        VerifyRawModel(List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Atom("Aaa", "ac$", "ref", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ))
      } catch {
        case e: VerifyRawModelException => e.getMessage ==>
          "Instead of getting the ref id with `ac$` please get it via the referenced namespace: `Ac.e ...`"
      }

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Generic("Ccc", "e", "datom", EntValue),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("refAttr, attr + redundant bond, attr") {
      val model = List(
        Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )


      try {
        VerifyRawModel(List(
          Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List()),
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ))
      } catch {
        case e: VerifyRawModelException => e.getMessage ==>
          "Instead of getting the ref id with `ac` please get it via the referenced namespace: `Ac.e ...`"
      }

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Generic("Ccc", "e", "datom", EntValue),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }

    test("refAttr_, attr + redundant bond, attr") {
      val model = List(
        Atom("Aaa", "ac_", "ref", 1, VarValue, None, List(), List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )


      try {
        VerifyRawModel(List(
          Atom("Aaa", "ac_", "ref", 1, VarValue, None, List(), List()),
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ))
      } catch {
        case e: VerifyRawModelException => e.getMessage ==>
          "Instead of getting the ref id with `ac_` please get it via the referenced namespace: `Ac.e ...`"
      }


      //      (ValidatedModelElements(List(
      //        Atom("Aaa", "ac_", "ref", 1, VarValue, None, List(), List()),
      //        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
      //        Bond("Aaa", "ac", "Ccc", 1, List()),
      //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      //      )) must throwA[VerifyRawModelException])
      //        .message ==> "Got the exception moleculeadmin.shared.lib.molecule.ops.exception.VerifyRawModelException: " +
      //        "Instead of getting the ref id with `ac_` please get it via the referenced namespace: `Ac.e ...`"

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Generic("Ccc", "e", "datom", EntValue),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }

    test("refAttr$, attr + redundant bond, attr") {
      val model = List(
        Atom("Aaa", "ac$", "ref", 1, VarValue, None, List(), List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )


      try {
        VerifyRawModel(List(
          Atom("Aaa", "ac$", "ref", 1, VarValue, None, List(), List()),
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        ))
      } catch {
        case e: VerifyRawModelException => e.getMessage ==>
          "Instead of getting the ref id with `ac$` please get it via the referenced namespace: `Ac.e ...`"
      }

      //      (ValidatedModelElements(List(
      //        Atom("Aaa", "ac$", "ref", 1, VarValue, None, List(), List()),
      //        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
      //        Bond("Aaa", "ac", "Ccc", 1, List()),
      //        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      //      )) must throwA[VerifyRawModelException])
      //        .message ==> "Got the exception moleculeadmin.shared.lib.molecule.ops.exception.VerifyRawModelException: " +
      //        "Instead of getting the ref id with `ac$` please get it via the referenced namespace: `Ac.e ...`"

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ac", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Generic("Ccc", "e", "datom", EntValue),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("bond + bond/attr") {
      val model = List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc", "attrC"))).toString ==> Model(List(
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("attr bond + bond/attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("attr bond attr + bond/attr") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("bondD/attr inserted after empty sibling bondC") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ad", "Ddd", "attrD"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("bondB/attr inserted _after_ empty sibling bondC") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "attrB"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("bondD/attr inserted after sibling bondC") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ad", "Ddd", "attrD"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("bondB/attr inserted _after_ sibling bondC") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, List("" -> "Aaa"), "ab", "Bbb", "attrB"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("insert before rebond") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, Seq("" -> "Aaa", "ab" -> "Bbb"), "bd", "Ddd", "attrD"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),

        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),

        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("Aaa") {
      val model = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),

        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),
        ReBond("Bbb"),
        ReBond("Aaa"),

        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      Model(VerifyRawModel(addNs(model, Seq("" -> "Aaa", "ab" -> "Bbb"), "bc", "Ccc", "attrC"))).toString ==> Model(List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),

        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "bd", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List()),

        ReBond("Bbb"),
        Bond("Bbb", "bc", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List()),

        ReBond("Bbb"),
        ReBond("Aaa"),

        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )).toString
    }


    test("Build complex example") {
      val m1 = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      val m2 = addNs(m1, List("" -> "Aaa"), "ab", "Bbb", "attrB")
      m2 ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      val m3 = addNs(m2, List("" -> "Aaa", "ab" -> "Bbb"), "ba", "Aaa", "attrA")
      m3 ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      val m4 = addNs(m3, List("" -> "Aaa", "ab" -> "Bbb", "ba" -> "Aaa"), "ad", "Ddd", "attrD")
      m4 ==> List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        Bond("Bbb", "ba", "Aaa", 1, List()),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ad", "Ddd", 1, List()),
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
      )

      val m5 = addNs(m4, List("" -> "Aaa", "ab" -> "Bbb"), "bd", "Ddd", "attrD")
      m5 ==> List(
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

      val m6 = addNs(m5, List("" -> "Aaa"), "ac", "Ccc", "attrC")
      m6 ==> List(
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
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      val m7 = addNs(m6, List("" -> "Aaa", "ac" -> "Ccc"), "ca", "Aaa", "attrA")
      m7 ==> List(
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
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      val m8 = addNs(m7, List("" -> "Aaa", "ac" -> "Ccc"), "cb", "Bbb", "attrB")
      m8 ==> List(
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
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )

      val m9 = addNs(m8, List("" -> "Aaa", "ac" -> "Ccc", "cb" -> "Bbb"), "ba", "Aaa", "attrA")
      m9 ==> List(
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
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )

      val m10 = addNs(m9, List("" -> "Aaa"), "ad", "Ddd", "attrD")
      m10 ==> List(
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
        Atom("Ddd", "attrD", "String", 1, VarValue, None, List(), List())
      )

      val m11 = addNs(m10, List("" -> "Aaa", "ad" -> "Ddd"), "db", "Bbb", "attrB")
      m11 ==> List(
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
    }
  }
}