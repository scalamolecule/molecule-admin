package moleculeadmin.sharedtest.ops.query.attr

import moleculeadmin.shared.lib.molecule.ast.model._
import moleculeadmin.shared.ops.query.attr.ModeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object ToggleMode extends TestSuite with TreeSchema with ModeOps {

  val dum  = List(Atom("Ns", "Dummy to keep ns open", "", 1, NoValue))
  val dum1 = List(Atom("Ref1", "Dummy to keep ns open", "", 1, NoValue))

  val e   = List(Generic("Ns", "e", "datom", EntValue))
  val eFn = List(Generic("Ns", "e", "datom", Eq(Seq(42))))

  val int       = List(Atom("Ns", "int", "Int", 1, VarValue))
  val intCo     = List(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("count")), Atom("Ns", "int", "Int", 1, Fn("count", None)))
  val intTx     = List(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("tx")), Generic("Ns", "tx", "datom", NoValue))
  val intCoTx   = List(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("count", "tx")), Atom("Ns", "int", "Int", 1, Fn("count", None)), Generic("Ns", "tx", "datom", NoValue))
  val intEq     = List(Atom("Ns", "int", "Int", 1, Eq(Seq(42))))
  val intEqCo   = List(Atom("Ns", "int", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count")), Atom("Ns", "int", "Int", 1, Fn("count", None)))
  val intEqTx   = List(Atom("Ns", "int", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("tx")), Generic("Ns", "tx", "datom", NoValue))
  val intEqCoTx = List(Atom("Ns", "int", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count", "tx")), Atom("Ns", "int", "Int", 1, Fn("count", None)), Generic("Ns", "tx", "datom", NoValue))

  val int_       = List(Atom("Ns", "int_", "Int", 1, VarValue))
  val intCo_     = List(Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq("count")), Atom("Ns", "int", "Int", 1, Fn("count", None)))
  val intTx_     = List(Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq("tx")), Generic("Ns", "tx", "datom", NoValue))
  val intCoTx_   = List(Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq("count", "tx")), Atom("Ns", "int", "Int", 1, Fn("count", None)), Generic("Ns", "tx", "datom", NoValue))
  val intEq_     = List(Atom("Ns", "int_", "Int", 1, Eq(Seq(42))))
  val intEqCo_   = List(Atom("Ns", "int_", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count")), Atom("Ns", "int", "Int", 1, Fn("count", None)))
  val intEqTx_   = List(Atom("Ns", "int_", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("tx")), Generic("Ns", "tx", "datom", NoValue))
  val intEqCoTx_ = List(Atom("Ns", "int_", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count", "tx")), Atom("Ns", "int", "Int", 1, Fn("count", None)), Generic("Ns", "tx", "datom", NoValue))

  val int$       = List(Atom("Ns", "int$", "Int", 1, VarValue))

  val str = List(Atom("Ns", "str", "String", 1, VarValue))
  val enum = List(Atom("Ns", "enum", "String", 1, EnumVal, Some(":Ns.enum/")))

  val e_   = List(Generic("Ns", "e_", "datom", EntValue))
  val eFn_ = List(Generic("Ns", "e_", "datom", Eq(Seq(42))))

  val intFn_ = List(Atom("Ns", "int_", "Int", 1, Eq(Seq(42))))
  val str_   = List(Atom("Ns", "str_", "String", 1, VarValue))

  val intFn$ = List(Atom("Ns", "int$", "Int", 1, Eq(Seq(42))))
  val str$   = List(Atom("Ns", "str$", "String", 1, VarValue))

  val intNil = List(Atom("Ns", "int_", "Int", 1, Fn("not", None)))
  val strNil = List(Atom("Ns", "str_", "String", 1, Fn("not", None)))


  val ref1 = List(Bond("Ns", "ref1", "Ref1", 1, Seq()))
  val e1   = List(Generic("Ref1", "e", "datom", EntValue))

  val int1  = List(Atom("Ref1", "int1", "Int", 1, VarValue))
  val int1_ = List(Atom("Ref1", "int1_", "Int", 1, VarValue))
  val int1$ = List(Atom("Ref1", "int1$", "Int", 1, VarValue))

  val intEqCoTx1  = List(Atom("Ref1", "int1", "Int", 1, Eq(Seq(42))), Atom("Ref1", "int1", "Int", 1, Fn("count", None)), Generic("Ref1", "tx", "datom", NoValue))
  val intEqCoTx1_ = List(Atom("Ref1", "int1_", "Int", 1, Eq(Seq(42))), Atom("Ref1", "int1", "Int", 1, Fn("count", None)), Generic("Ref1", "tx", "datom", NoValue))

  val intNil1 = List(Atom("Ref1", "int1_", "Int", 1, Fn("not", None)))

  val str1 = List(Atom("Ref1", "str1", "String", 1, VarValue))

  val e1_ = List(Generic("Ref1", "e_", "datom", EntValue))


  def toggleNs(m1: Seq[Element], attr: String, m2: Seq[Element]): Unit = {
    toggleMode(m1, List("" -> "Ns"), attr) ==> m2
  }

  def toggleRef1(m1: Seq[Element], attr: String, m2: Seq[Element]): Unit = {
    toggleMode(m1, List("" -> "Ns", "ref1" -> "Ref1"), attr) ==> m2
  }


  val tests = Tests {

    test("0 attr") {
      toggleNs(dum, "e", e)
      toggleNs(dum, "int", int)
    }


    test("1 attr") {

      test("e") {
        toggleNs(e, "e", e_)
        toggleNs(e, "int", e ++ int)

        toggleNs(e_, "e", dum)
        toggleNs(e_, "int", e_ ++ int)

        toggleNs(eFn, "e", eFn_)
        toggleNs(eFn, "int", eFn ++ int)

        toggleNs(eFn_, "e", dum)
        toggleNs(eFn_, "int", eFn_ ++ int)
      }

      test("mandatory") {
        toggleNs(int, "e", int ++ e)
        toggleNs(int, "int", int_)
        toggleNs(int, "str", int ++ str)
        toggleNs(int, "enum", int ++ enum)

        toggleNs(intCo, "e", intCo ++ e)
        toggleNs(intCo, "int", intCo_)
        toggleNs(intCo, "str", intCo ++ str)

        toggleNs(intTx, "e", intTx ++ e)
        toggleNs(intTx, "int", intTx_)
        toggleNs(intTx, "str", intTx ++ str)

        toggleNs(intCoTx, "e", intCoTx ++ e)
        toggleNs(intCoTx, "int", intCoTx_)
        toggleNs(intCoTx, "str", intCoTx ++ str)

        toggleNs(intEq, "e", intEq ++ e)
        toggleNs(intEq, "int", intEq_)
        toggleNs(intEq, "str", intEq ++ str)

        toggleNs(intEqCo, "e", intEqCo ++ e)
        toggleNs(intEqCo, "int", intEqCo_)
        toggleNs(intEqCo, "str", intEqCo ++ str)

        toggleNs(intEqTx, "e", intEqTx ++ e)
        toggleNs(intEqTx, "int", intEqTx_)
        toggleNs(intEqTx, "str", intEqTx ++ str)

        toggleNs(intEqCoTx, "e", intEqCoTx ++ e)
        toggleNs(intEqCoTx, "int", intEqCoTx_)
        toggleNs(intEqCoTx, "str", intEqCoTx ++ str)
      }

      test("tacit") {
        toggleNs(int_, "e", int_ ++ e)
        toggleNs(int_, "int", int$)
        toggleNs(int_, "str", int_ ++ str)

        toggleNs(intCo_, "e", intCo_ ++ e)
        toggleNs(intCo_, "int", int$)
        toggleNs(intCo_, "str", intCo_ ++ str)

        toggleNs(intTx_, "e", intTx_ ++ e)
        toggleNs(intTx_, "int", int$)
        toggleNs(intTx_, "str", intTx_ ++ str)

        toggleNs(intCoTx_, "e", intCoTx_ ++ e)
        toggleNs(intCoTx_, "int", int$)
        toggleNs(intCoTx_, "str", intCoTx_ ++ str)

        toggleNs(intEq_, "e", intEq_ ++ e)
        toggleNs(intEq_, "int", int$)
        toggleNs(intEq_, "str", intEq_ ++ str)

        toggleNs(intEqCo_, "e", intEqCo_ ++ e)
        toggleNs(intEqCo_, "int", int$)
        toggleNs(intEqCo_, "str", intEqCo_ ++ str)

        toggleNs(intEqTx_, "e", intEqTx_ ++ e)
        toggleNs(intEqTx_, "int", int$)
        toggleNs(intEqTx_, "str", intEqTx_ ++ str)

        toggleNs(intEqCoTx_, "e", intEqCoTx_ ++ e)
        toggleNs(intEqCoTx_, "int", int$)
        toggleNs(intEqCoTx_, "str", intEqCoTx_ ++ str)
      }

      test("optional") {
        toggleNs(int$, "e", int$ ++ e)
        toggleNs(int$, "int", intNil)
        toggleNs(int$, "str", int$ ++ str)
      }

      test("nil") {
        toggleNs(intNil, "e", intNil ++ e)
        toggleNs(intNil, "int", dum)
        toggleNs(intNil, "str", intNil ++ str)
      }
    }


    test("2 attr") {

      test("attr + e") {
        toggleNs(int ++ e, "e", int ++ e_)
        toggleNs(int ++ e, "int", int_ ++ e)
        toggleNs(int ++ e, "str", int ++ e ++ str)

        toggleNs(int_ ++ e, "e", int_ ++ e_)
        toggleNs(int_ ++ e, "int", int$ ++ e)
        toggleNs(int_ ++ e, "str", int_ ++ e ++ str)

        toggleNs(intEqCoTx ++ e, "e", intEqCoTx ++ e_)
        toggleNs(intEqCoTx ++ e, "int", intEqCoTx_ ++ e)
        toggleNs(intEqCoTx ++ e, "str", intEqCoTx ++ e ++ str)
      }

      test("e + attr") {
        toggleNs(e ++ int, "e", e_ ++ int)
        toggleNs(e ++ int, "int", e ++ int_)
        toggleNs(e ++ int, "str", e ++ int ++ str)

        toggleNs(e ++ int_, "e", e_ ++ int_)
        toggleNs(e ++ int_, "int", e ++ int$)
        toggleNs(e ++ int_, "str", e ++ int_ ++ str)

        toggleNs(e ++ intEqCoTx, "e", e_ ++ intEqCoTx)
        toggleNs(e ++ intEqCoTx, "int", e ++ intEqCoTx_)
        toggleNs(e ++ intEqCoTx, "str", e ++ intEqCoTx ++ str)
      }

      test("attr1 + attr2") {
        toggleNs(int ++ str, "e", int ++ str ++ e)
        toggleNs(int ++ str, "int", int_ ++ str)
        toggleNs(int ++ str, "str", int ++ str_)
      }
    }


    test("attr + additive") {

      test("e") {
        toggleNs(int ++ e, "e", int ++ e_)
        toggleNs(int ++ e, "int", int_ ++ e)
        toggleNs(int ++ e, "str", int ++ e ++ str)

        toggleNs(int_ ++ e, "e", int_ ++ e_)
        toggleNs(int_ ++ e, "int", int$ ++ e)
        toggleNs(int_ ++ e, "str", int_ ++ e ++ str)

        toggleNs(intEqCoTx ++ e, "e", intEqCoTx ++ e_)
        toggleNs(intEqCoTx ++ e, "int", intEqCoTx_ ++ e)
        toggleNs(intEqCoTx ++ e, "str", intEqCoTx ++ e ++ str)
      }
    }


    test("Multiple branches") {

      test("attr") {
        toggleNs(ref1 ++ dum1, "e", e ++ ref1 ++ dum1)
        toggleNs(ref1 ++ dum1, "int", int ++ ref1 ++ dum1)
        toggleNs(ref1 ++ dum1, "str", str ++ ref1 ++ dum1)

        toggleNs(e ++ ref1 ++ dum1, "e", e_ ++ ref1 ++ dum1)
        toggleNs(e ++ ref1 ++ dum1, "int", e ++ int ++ ref1 ++ dum1)
        toggleNs(e ++ ref1 ++ dum1, "str", e ++ str ++ ref1 ++ dum1)

        toggleNs(e_ ++ ref1 ++ dum1, "e", ref1 ++ dum1)
        toggleNs(e_ ++ ref1 ++ dum1, "int", e_ ++ int ++ ref1 ++ dum1)
        toggleNs(e_ ++ ref1 ++ dum1, "str", e_ ++ str ++ ref1 ++ dum1)


        toggleNs(int ++ ref1 ++ dum1, "e", int ++ e ++ ref1 ++ dum1)
        toggleNs(int ++ ref1 ++ dum1, "int", int_ ++ ref1 ++ dum1)
        toggleNs(int ++ ref1 ++ dum1, "str", int ++ str ++ ref1 ++ dum1)

        toggleNs(intEqCoTx ++ ref1 ++ dum1, "e", intEqCoTx ++ e ++ ref1 ++ dum1)
        toggleNs(intEqCoTx ++ ref1 ++ dum1, "int", intEqCoTx_ ++ ref1 ++ dum1)
        toggleNs(intEqCoTx ++ ref1 ++ dum1, "str", intEqCoTx ++ str ++ ref1 ++ dum1)


        toggleNs(int_ ++ ref1 ++ dum1, "e", int_ ++ e ++ ref1 ++ dum1)
        toggleNs(int_ ++ ref1 ++ dum1, "int", int$ ++ ref1 ++ dum1)
        toggleNs(int_ ++ ref1 ++ dum1, "str", int_ ++ str ++ ref1 ++ dum1)

        toggleNs(intEqCoTx_ ++ ref1 ++ dum1, "e", intEqCoTx_ ++ e ++ ref1 ++ dum1)
        toggleNs(intEqCoTx_ ++ ref1 ++ dum1, "int", int$ ++ ref1 ++ dum1)
        toggleNs(intEqCoTx_ ++ ref1 ++ dum1, "str", intEqCoTx_ ++ str ++ ref1 ++ dum1)


        toggleNs(int$ ++ ref1 ++ dum1, "e", int$ ++ e ++ ref1 ++ dum1)
        toggleNs(int$ ++ ref1 ++ dum1, "int", intNil ++ ref1 ++ dum1)
        toggleNs(int$ ++ ref1 ++ dum1, "str", int$ ++ str ++ ref1 ++ dum1)


        toggleNs(intNil ++ ref1 ++ dum1, "e", intNil ++ e ++ ref1 ++ dum1)
        toggleNs(intNil ++ ref1 ++ dum1, "int", ref1 ++ dum1)
        toggleNs(intNil ++ ref1 ++ dum1, "str", intNil ++ str ++ ref1 ++ dum1)
      }

      test("attr1") {
        toggleRef1(ref1 ++ dum1, "e", ref1 ++ e1)
        toggleRef1(ref1 ++ dum1, "int1", ref1 ++ int1)
        toggleRef1(ref1 ++ dum1, "str1", ref1 ++ str1)

        toggleRef1(ref1 ++ e1, "e", ref1 ++ e1_)
        toggleRef1(ref1 ++ e1, "int1", ref1 ++ e1 ++ int1)
        toggleRef1(ref1 ++ e1, "str1", ref1 ++ e1 ++ str1)

        toggleRef1(ref1 ++ e1_, "e", ref1 ++ dum1)
        toggleRef1(ref1 ++ e1_, "int1", ref1 ++ e1_ ++ int1)
        toggleRef1(ref1 ++ e1_, "str1", ref1 ++ e1_ ++ str1)


        toggleRef1(ref1 ++ int1, "e", ref1 ++ int1 ++ e1)
        toggleRef1(ref1 ++ int1, "int1", ref1 ++ int1_)
        toggleRef1(ref1 ++ int1, "str1", ref1 ++ int1 ++ str1)

        toggleRef1(ref1 ++ int1_, "e", ref1 ++ int1_ ++ e1)
        toggleRef1(ref1 ++ int1_, "int1", ref1 ++ int1$)
        toggleRef1(ref1 ++ int1_, "str1", ref1 ++ int1_ ++ str1)

        toggleRef1(ref1 ++ int1$, "e", ref1 ++ int1$ ++ e1)
        toggleRef1(ref1 ++ int1$, "int1", ref1 ++ intNil1)
        toggleRef1(ref1 ++ int1$, "str1", ref1 ++ int1$ ++ str1)


        toggleRef1(ref1 ++ intEqCoTx1, "e", ref1 ++ intEqCoTx1 ++ e1)
        toggleRef1(ref1 ++ intEqCoTx1, "int1", ref1 ++ intEqCoTx1_)
        toggleRef1(ref1 ++ intEqCoTx1, "str1", ref1 ++ intEqCoTx1 ++ str1)

        toggleRef1(ref1 ++ intEqCoTx1_, "e", ref1 ++ intEqCoTx1_ ++ e1)
        toggleRef1(ref1 ++ intEqCoTx1_, "int1", ref1 ++ int1$)
        toggleRef1(ref1 ++ intEqCoTx1_, "str1", ref1 ++ intEqCoTx1_ ++ str1)


        toggleRef1(ref1 ++ intNil1, "e", ref1 ++ intNil1 ++ e1)
        toggleRef1(ref1 ++ intNil1, "int1", ref1 ++ dum1)
        toggleRef1(ref1 ++ intNil1, "str1", ref1 ++ intNil1 ++ str1)
      }


      test("attr$ -> dummy") {
        val non = List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        val man = toggleMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB")
        man ==> List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        val tac = toggleMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB")
        tac ==> List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB_", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        val opt = toggleMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB")
        opt ==> List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB$", "String", 1, VarValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        val nil = toggleMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB")
        nil ==> List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "attrB_", "String", 1, Fn("not", None), None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        toggleMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB") ==> non
      }


      test("e -> dummy") {
        val non = List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        val man = toggleMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "e")
        man ==> List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Generic("Bbb", "e", "datom", EntValue),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        val tac = toggleMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "e")
        tac ==> List(
          Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
          Bond("Aaa", "ab", "Bbb", 1, List()),
          Generic("Bbb", "e_", "datom", EntValue),
          ReBond("Aaa"),
          Bond("Aaa", "ac", "Ccc", 1, List()),
          Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
        )

        toggleMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "e") ==> non
      }
    }
  }
}
