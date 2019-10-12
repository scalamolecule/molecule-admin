package moleculeadmin.sharedtest.ops.query.attr

import moleculeadmin.shared.lib.molecule.ast.model._
import moleculeadmin.shared.ops.query.attr.ModeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object SetMode extends TestSuite with TreeSchema with ModeOps {

  val dum  = List(Atom("Ns", "Dummy to keep ns open", "", 1, NoValue))
  val dum1 = List(Atom("Ref1", "Dummy to keep ns open", "", 1, NoValue))

  val e  = List(Generic("Ns", "e", "datom", EntValue))
  val e_ = List(Generic("Ns", "e_", "datom", EntValue))

  val eEq  = List(Generic("Ns", "e", "datom", Eq(Seq(42))))
  val eEq_ = List(Generic("Ns", "e_", "datom", Eq(Seq(42))))

  val int    = List(Atom("Ns", "int", "Int", 1, VarValue))
  val int_   = List(Atom("Ns", "int_", "Int", 1, VarValue))
  val int$   = List(Atom("Ns", "int$", "Int", 1, VarValue))
  val intNil = List(Atom("Ns", "int_", "Int", 1, Fn("not", None)))

  val intEqCoTx  = List(Atom("Ns", "int", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count", "tx")), Atom("Ns", "int", "Int", 1, Fn("count", None)), Generic("Ns", "tx", "datom", NoValue))
  val intEqCoTx_ = List(Atom("Ns", "int_", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count", "tx")), Atom("Ns", "int", "Int", 1, Fn("count", None)), Generic("Ns", "tx", "datom", NoValue))

  val int1EqCoTx  = List(Atom("Ref1", "int1", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count", "tx")), Atom("Ref1", "int1", "Int", 1, Fn("count", None)), Generic("Ref1", "tx", "datom", NoValue))
  val int1EqCoTx_ = List(Atom("Ref1", "int1_", "Int", 1, Eq(Seq(42)), None, Seq(), Seq("count", "tx")), Atom("Ref1", "int1", "Int", 1, Fn("count", None)), Generic("Ref1", "tx", "datom", NoValue))


  val str    = List(Atom("Ns", "str", "String", 1, VarValue))
  val str_   = List(Atom("Ns", "str_", "String", 1, VarValue))
  val str$   = List(Atom("Ns", "str$", "String", 1, VarValue))
  val strNil = List(Atom("Ns", "str_", "String", 1, Fn("not", None)))

  val long = List(Atom("Ns", "long", "Long", 1, VarValue))

  val ref1 = List(Bond("Ns", "ref1", "Ref1", 1, Seq()))
  val e1   = List(Generic("Ref1", "e", "datom", EntValue))
  val e1_  = List(Generic("Ref1", "e_", "datom", EntValue))

  val int1    = List(Atom("Ref1", "int1", "Int", 1, VarValue))
  val int1_   = List(Atom("Ref1", "int1_", "Int", 1, VarValue))
  val int1$   = List(Atom("Ref1", "int1$", "Int", 1, VarValue))
  val int1Nil = List(Atom("Ref1", "int1_", "Int", 1, Fn("not", None)))


  val str1 = List(Atom("Ref1", "str1", "String", 1, VarValue))


  def fullMode(mode: String) = mode match {
    case "man" => "mandatory"
    case "tac" => "tacit"
    case "opt" => "optional"
    case "nil" => "nil"
    case "non" => "none"
  }

  def setEid(m1: Seq[Element], mode: String, m2: Seq[Element]) = {
    setMode(m1, List("" -> "Ns"), "e", fullMode(mode)) ==> m2
  }

  def setEid1(m1: Seq[Element], mode: String, m2: Seq[Element]) = {
    setMode(m1, List("" -> "Ns", "ref1" -> "Ref1"), "e", fullMode(mode)) ==> m2
  }

  def setInt(m1: Seq[Element], mode: String, m2: Seq[Element]) = {
    setMode(m1, List("" -> "Ns"), "int", fullMode(mode)) ==> m2
  }

  def setInt1(m1: Seq[Element], mode: String, m2: Seq[Element]) = {
    setMode(m1, List("" -> "Ns", "ref1" -> "Ref1"), "int1", fullMode(mode)) ==> m2
  }


  val tests = Tests {

    test("1 branch") {
      test("dummy") {
        test("[e]") {
          setEid(dum, "man", e)
          setEid(dum, "tac", e_) // single tacit/optional/nil not allowed
          setEid(dum, "non", dum)
        }

        test("[int]") {
          setInt(dum, "man", int)
          setInt(dum, "tac", int_) // single tacit/optional/nil not allowed
          setInt(dum, "opt", int$)
          setInt(dum, "nil", intNil)
          setInt(dum, "non", dum)
        }
      }

      test("[attr]") {
        test("e") {
          setEid(e, "man", e)
          setEid(e, "tac", e_)
          setEid(e, "non", dum)

          setEid(eEq, "man", eEq)
          setEid(eEq, "tac", eEq_)
          setEid(eEq, "non", dum)
        }

        test("e_") {
          setEid(e_, "man", e)
          setEid(e_, "tac", e_)
          setEid(e_, "non", dum)

          setEid(eEq_, "man", eEq)
          setEid(eEq_, "tac", eEq_)
          setEid(eEq_, "non", dum)
        }

        test("int") {
          setInt(int, "man", int)
          setInt(int, "tac", int_)
          setInt(int, "opt", int$)
          setInt(int, "nil", intNil)
          setInt(int, "non", dum)

          setInt(intEqCoTx, "man", intEqCoTx)
          setInt(intEqCoTx, "tac", intEqCoTx_)
          setInt(intEqCoTx, "opt", int$)
          setInt(intEqCoTx, "nil", intNil)
          setInt(intEqCoTx, "non", dum)
        }

        test("int_") {
          setInt(int_, "man", int)
          setInt(int_, "tac", int_)
          setInt(int_, "opt", int$)
          setInt(int_, "nil", intNil)
          setInt(int_, "non", dum)

          setInt(intEqCoTx_, "man", intEqCoTx)
          setInt(intEqCoTx_, "tac", intEqCoTx_)
          setInt(intEqCoTx_, "opt", int$)
          setInt(intEqCoTx_, "nil", intNil)
          setInt(intEqCoTx_, "non", dum)
        }

        test("int$") {
          setInt(int$, "man", int)
          setInt(int$, "tac", int_)
          setInt(int$, "opt", int$)
          setInt(int$, "nil", intNil)
          setInt(int$, "non", dum)
        }

        test("intNil") {
          setInt(intNil, "man", int)
          setInt(intNil, "tac", int_)
          setInt(intNil, "opt", int$)
          setInt(intNil, "nil", intNil)
          setInt(intNil, "non", dum)
        }
      }


      test("attr1 + [attr2]") {

        test("e") {
          test("[int]") {
            setInt(e, "man", e ++ int)
            setInt(e, "tac", e ++ int_)
            setInt(e, "opt", e ++ int$)
            setInt(e, "nil", e ++ intNil)
            setInt(e, "non", e)
          }
        }

        test("e_") {
          test("[int]") {
            setInt(e_, "man", e_ ++ int)
            setInt(e_, "tac", e_ ++ int_)
            setInt(e_, "opt", e_ ++ int$)
            setInt(e_, "nil", e_ ++ intNil)
            setInt(e_, "non", e_)
          }
        }

        test("str") {
          test("[e]") {
            setEid(str, "man", str ++ e)
            setEid(str, "tac", str ++ e_)
            setEid(str, "non", str)
          }

          test("[int]") {
            setInt(str, "man", str ++ int)
            setInt(str, "tac", str ++ int_)
            setInt(str, "opt", str ++ int$)
            setInt(str, "nil", str ++ intNil)
            setInt(str, "non", str)
          }
        }

        test("str_") {
          test("[e]") {
            setEid(str_, "man", str_ ++ e)
            setEid(str_, "tac", str_ ++ e_)
            setEid(str_, "non", str_)
          }

          test("[int]") {
            setInt(str_, "man", str_ ++ int)
            setInt(str_, "tac", str_ ++ int_)
            setInt(str_, "opt", str_ ++ int$)
            setInt(str_, "nil", str_ ++ intNil)
            setInt(str_, "non", str_)
          }
        }

        test("str$") {
          test("[e]") {
            setEid(str$, "man", str$ ++ e)
            setEid(str$, "tac", str$ ++ e_)
            setEid(str$, "non", str$)
          }

          test("[int]") {
            setInt(str$, "man", str$ ++ int)
            setInt(str$, "tac", str$ ++ int_)
            setInt(str$, "opt", str$ ++ int$)
            setInt(str$, "nil", str$ ++ intNil)
            setInt(str$, "non", str$)
          }
        }

        test("strNil") {
          test("e") {
            setEid(strNil, "man", strNil ++ e)
            setEid(strNil, "tac", strNil ++ e_)
            setEid(strNil, "non", strNil)
          }

          test("[int]") {
            setInt(strNil, "man", strNil ++ int)
            setInt(strNil, "tac", strNil ++ int_)
            setInt(strNil, "opt", strNil ++ int$)
            setInt(strNil, "nil", strNil ++ intNil)
            setInt(strNil, "non", strNil)
          }
        }
      }


      test("[attr1].attr2") {
        setInt(int ++ str, "man", int ++ str)
        setInt(int ++ str, "tac", int_ ++ str)
        setInt(int ++ str, "opt", int$ ++ str)
        setInt(int ++ str, "nil", intNil ++ str)
        setInt(int ++ str, "non", str)

        setInt(intEqCoTx ++ str, "man", intEqCoTx ++ str)
        setInt(intEqCoTx ++ str, "tac", intEqCoTx_ ++ str)
        setInt(intEqCoTx ++ str, "opt", int$ ++ str)
        setInt(intEqCoTx ++ str, "nil", intNil ++ str)
        setInt(intEqCoTx ++ str, "non", str)

        setInt(int_ ++ str, "man", int ++ str)
        setInt(int_ ++ str, "tac", int_ ++ str)
        setInt(int_ ++ str, "opt", int$ ++ str)
        setInt(int_ ++ str, "nil", intNil ++ str)
        setInt(int_ ++ str, "non", str)

        setInt(intEqCoTx_ ++ str, "man", intEqCoTx ++ str)
        setInt(intEqCoTx_ ++ str, "tac", intEqCoTx_ ++ str)
        setInt(intEqCoTx_ ++ str, "opt", int$ ++ str)
        setInt(intEqCoTx_ ++ str, "nil", intNil ++ str)
        setInt(intEqCoTx_ ++ str, "non", str)

        setInt(int$ ++ str, "man", int ++ str)
        setInt(int$ ++ str, "tac", int_ ++ str)
        setInt(int$ ++ str, "opt", int$ ++ str)
        setInt(int$ ++ str, "nil", intNil ++ str)
        setInt(int$ ++ str, "non", str)

        setInt(intNil ++ str, "man", int ++ str)
        setInt(intNil ++ str, "tac", int_ ++ str)
        setInt(intNil ++ str, "opt", int$ ++ str)
        setInt(intNil ++ str, "nil", intNil ++ str)
        setInt(intNil ++ str, "non", str)
      }

      test("attr1.[attr2]") {
        setInt(str ++ int, "man", str ++ int)
        setInt(str ++ int, "tac", str ++ int_)
        setInt(str ++ int, "opt", str ++ int$)
        setInt(str ++ int, "nil", str ++ intNil)
        setInt(str ++ int, "non", str)

        setInt(str ++ intEqCoTx, "man", str ++ intEqCoTx)
        setInt(str ++ intEqCoTx, "tac", str ++ intEqCoTx_)
        setInt(str ++ intEqCoTx, "opt", str ++ int$)
        setInt(str ++ intEqCoTx, "nil", str ++ intNil)
        setInt(str ++ intEqCoTx, "non", str)

        setInt(str ++ int_, "man", str ++ int)
        setInt(str ++ int_, "tac", str ++ int_)
        setInt(str ++ int_, "opt", str ++ int$)
        setInt(str ++ int_, "nil", str ++ intNil)
        setInt(str ++ int_, "non", str)

        setInt(str ++ intEqCoTx_, "man", str ++ intEqCoTx)
        setInt(str ++ intEqCoTx_, "tac", str ++ intEqCoTx_)
        setInt(str ++ intEqCoTx_, "opt", str ++ int$)
        setInt(str ++ intEqCoTx_, "nil", str ++ intNil)
        setInt(str ++ intEqCoTx_, "non", str)

        setInt(str_ ++ int, "man", str_ ++ int)
        setInt(str_ ++ int, "tac", str_ ++ int_)
        setInt(str_ ++ int, "opt", str_ ++ int$)
        setInt(str_ ++ int, "nil", str_ ++ intNil)
        setInt(str_ ++ int, "non", str_)

        setInt(str_ ++ intEqCoTx, "man", str_ ++ intEqCoTx)
        setInt(str_ ++ intEqCoTx, "tac", str_ ++ intEqCoTx_)
        setInt(str_ ++ intEqCoTx, "opt", str_ ++ int$)
        setInt(str_ ++ intEqCoTx, "nil", str_ ++ intNil)
        setInt(str_ ++ intEqCoTx, "non", str_)
      }

      test("attr1.[attr2].attr3") {
        setInt(str ++ int ++ long, "man", str ++ int ++ long)
        setInt(str ++ int ++ long, "tac", str ++ int_ ++ long)
        setInt(str ++ int ++ long, "opt", str ++ int$ ++ long)
        setInt(str ++ int ++ long, "nil", str ++ intNil ++ long)
        setInt(str ++ int ++ long, "non", str ++ long)

        setInt(str ++ intEqCoTx ++ long, "man", str ++ intEqCoTx ++ long)
        setInt(str ++ intEqCoTx ++ long, "tac", str ++ intEqCoTx_ ++ long)
        setInt(str ++ intEqCoTx ++ long, "opt", str ++ int$ ++ long)
        setInt(str ++ intEqCoTx ++ long, "nil", str ++ intNil ++ long)
        setInt(str ++ intEqCoTx ++ long, "non", str ++ long)
      }
    }


    test("2 branches") {

      test("+[attr].bond.dummy") {
        test("e") {
          setEid(ref1 ++ dum1, "man", e ++ ref1 ++ dum1)
          setEid(ref1 ++ dum1, "tac", e_ ++ ref1 ++ dum1)
          setEid(ref1 ++ dum1, "non", ref1 ++ dum1)
        }

        test("int") {
          setInt(ref1 ++ dum1, "man", int ++ ref1 ++ dum1)
          setInt(ref1 ++ dum1, "tac", int_ ++ ref1 ++ dum1)
          setInt(ref1 ++ dum1, "opt", int$ ++ ref1 ++ dum1)
          setInt(ref1 ++ dum1, "nil", intNil ++ ref1 ++ dum1)
          setInt(ref1 ++ dum1, "non", ref1 ++ dum1)
        }
      }

      test("[attr].bond.dummy") {
        test("e") {
          setEid(int ++ ref1 ++ dum1, "man", int ++ e ++ ref1 ++ dum1)
          setEid(int ++ ref1 ++ dum1, "tac", int ++ e_ ++ ref1 ++ dum1)
          setEid(int ++ ref1 ++ dum1, "non", int ++ ref1 ++ dum1)

          setEid(intEqCoTx ++ ref1 ++ dum1, "man", intEqCoTx ++ e ++ ref1 ++ dum1)
          setEid(intEqCoTx ++ ref1 ++ dum1, "tac", intEqCoTx ++ e_ ++ ref1 ++ dum1)
          setEid(intEqCoTx ++ ref1 ++ dum1, "non", intEqCoTx ++ ref1 ++ dum1)
        }

        test("int") {
          setInt(int ++ ref1 ++ dum1, "man", int ++ ref1 ++ dum1)
          setInt(int ++ ref1 ++ dum1, "tac", int_ ++ ref1 ++ dum1)
          setInt(int ++ ref1 ++ dum1, "opt", int$ ++ ref1 ++ dum1)
          setInt(int ++ ref1 ++ dum1, "nil", intNil ++ ref1 ++ dum1)
          setInt(int ++ ref1 ++ dum1, "non", ref1 ++ dum1)

          setInt(intEqCoTx ++ ref1 ++ dum1, "man", intEqCoTx ++ ref1 ++ dum1)
          setInt(intEqCoTx ++ ref1 ++ dum1, "tac", intEqCoTx_ ++ ref1 ++ dum1)
          setInt(intEqCoTx ++ ref1 ++ dum1, "opt", int$ ++ ref1 ++ dum1)
          setInt(intEqCoTx ++ ref1 ++ dum1, "nil", intNil ++ ref1 ++ dum1)
          setInt(intEqCoTx ++ ref1 ++ dum1, "non", ref1 ++ dum1)
        }
      }


      test("bond.[attr]") {

        test("dummy") {
          setInt1(ref1 ++ dum1, "man", ref1 ++ int1)
          setInt1(ref1 ++ dum1, "tac", ref1 ++ int1_)
          setInt1(ref1 ++ dum1, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ dum1, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ dum1, "non", ref1 ++ dum1)
        }

        test("e") {
          setEid1(ref1 ++ e1, "man", ref1 ++ e1)
          setEid1(ref1 ++ e1, "tac", ref1 ++ e1_)
          setEid1(ref1 ++ e1, "non", ref1 ++ dum1)
        }

        test("e_") {
          setEid1(ref1 ++ e1_, "man", ref1 ++ e1)
          setEid1(ref1 ++ e1_, "tac", ref1 ++ e1_)
          setEid1(ref1 ++ e1_, "non", ref1 ++ dum1)
        }

        test("int") {
          setInt1(ref1 ++ int1, "man", ref1 ++ int1)
          setInt1(ref1 ++ int1, "tac", ref1 ++ int1_)
          setInt1(ref1 ++ int1, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ int1, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ int1, "non", ref1 ++ dum1)

          setInt1(ref1 ++ int1EqCoTx, "man", ref1 ++ int1EqCoTx)
          setInt1(ref1 ++ int1EqCoTx, "tac", ref1 ++ int1EqCoTx_)
          setInt1(ref1 ++ int1EqCoTx, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ int1EqCoTx, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ int1EqCoTx, "non", ref1 ++ dum1)
        }

        test("int_") {
          setInt1(ref1 ++ int1_, "man", ref1 ++ int1)
          setInt1(ref1 ++ int1_, "tac", ref1 ++ int1_)
          setInt1(ref1 ++ int1_, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ int1_, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ int1_, "non", ref1 ++ dum1)

          setInt1(ref1 ++ int1EqCoTx_, "man", ref1 ++ int1EqCoTx)
          setInt1(ref1 ++ int1EqCoTx_, "tac", ref1 ++ int1EqCoTx_)
          setInt1(ref1 ++ int1EqCoTx_, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ int1EqCoTx_, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ int1EqCoTx_, "non", ref1 ++ dum1)
        }

        test("attr$") {
          setInt1(ref1 ++ int1$, "man", ref1 ++ int1)
          setInt1(ref1 ++ int1$, "tac", ref1 ++ int1_)
          setInt1(ref1 ++ int1$, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ int1$, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ int1$, "non", ref1 ++ dum1)
        }

        test("attrNil") {
          setInt1(ref1 ++ int1Nil, "man", ref1 ++ int1)
          setInt1(ref1 ++ int1Nil, "tac", ref1 ++ int1_)
          setInt1(ref1 ++ int1Nil, "opt", ref1 ++ int1$)
          setInt1(ref1 ++ int1Nil, "nil", ref1 ++ int1Nil)
          setInt1(ref1 ++ int1Nil, "non", ref1 ++ dum1)
        }
      }

      test("bond.[attr1].attr2") {
        test("e") {
          setEid1(ref1 ++ e1 ++ str1, "man", ref1 ++ e1 ++ str1)
          setEid1(ref1 ++ e1 ++ str1, "tac", ref1 ++ e1_ ++ str1)
          setEid1(ref1 ++ e1 ++ str1, "non", ref1 ++ str1)
        }

        test("e_") {
          setEid1(ref1 ++ e1_ ++ str1, "man", ref1 ++ e1 ++ str1)
          setEid1(ref1 ++ e1_ ++ str1, "tac", ref1 ++ e1_ ++ str1)
          setEid1(ref1 ++ e1_ ++ str1, "non", ref1 ++ str1)
        }

        test("int") {
          setInt1(ref1 ++ int1 ++ str1, "man", ref1 ++ int1 ++ str1)
          setInt1(ref1 ++ int1 ++ str1, "tac", ref1 ++ int1_ ++ str1)
          setInt1(ref1 ++ int1 ++ str1, "opt", ref1 ++ int1$ ++ str1)
          setInt1(ref1 ++ int1 ++ str1, "nil", ref1 ++ int1Nil ++ str1)
          setInt1(ref1 ++ int1 ++ str1, "non", ref1 ++ str1)

          setInt1(ref1 ++ int1EqCoTx ++ str1, "man", ref1 ++ int1EqCoTx ++ str1)
          setInt1(ref1 ++ int1EqCoTx ++ str1, "tac", ref1 ++ int1EqCoTx_ ++ str1)
          setInt1(ref1 ++ int1EqCoTx ++ str1, "opt", ref1 ++ int1$ ++ str1)
          setInt1(ref1 ++ int1EqCoTx ++ str1, "nil", ref1 ++ int1Nil ++ str1)
          setInt1(ref1 ++ int1EqCoTx ++ str1, "non", ref1 ++ str1)
        }

        test("int_") {
          setInt1(ref1 ++ int1_ ++ str1, "man", ref1 ++ int1 ++ str1)
          setInt1(ref1 ++ int1_ ++ str1, "tac", ref1 ++ int1_ ++ str1)
          setInt1(ref1 ++ int1_ ++ str1, "opt", ref1 ++ int1$ ++ str1)
          setInt1(ref1 ++ int1_ ++ str1, "nil", ref1 ++ int1Nil ++ str1)
          setInt1(ref1 ++ int1_ ++ str1, "non", ref1 ++ str1)

          setInt1(ref1 ++ int1EqCoTx_ ++ str1, "man", ref1 ++ int1EqCoTx ++ str1)
          setInt1(ref1 ++ int1EqCoTx_ ++ str1, "tac", ref1 ++ int1EqCoTx_ ++ str1)
          setInt1(ref1 ++ int1EqCoTx_ ++ str1, "opt", ref1 ++ int1$ ++ str1)
          setInt1(ref1 ++ int1EqCoTx_ ++ str1, "nil", ref1 ++ int1Nil ++ str1)
          setInt1(ref1 ++ int1EqCoTx_ ++ str1, "non", ref1 ++ str1)
        }

        test("attr$") {
          setInt1(ref1 ++ int1$ ++ str1, "man", ref1 ++ int1 ++ str1)
          setInt1(ref1 ++ int1$ ++ str1, "tac", ref1 ++ int1_ ++ str1)
          setInt1(ref1 ++ int1$ ++ str1, "opt", ref1 ++ int1$ ++ str1)
          setInt1(ref1 ++ int1$ ++ str1, "nil", ref1 ++ int1Nil ++ str1)
          setInt1(ref1 ++ int1$ ++ str1, "non", ref1 ++ str1)
        }

        test("attrNil") {
          setInt1(ref1 ++ int1Nil ++ str1, "man", ref1 ++ int1 ++ str1)
          setInt1(ref1 ++ int1Nil ++ str1, "tac", ref1 ++ int1_ ++ str1)
          setInt1(ref1 ++ int1Nil ++ str1, "opt", ref1 ++ int1$ ++ str1)
          setInt1(ref1 ++ int1Nil ++ str1, "nil", ref1 ++ int1Nil ++ str1)
          setInt1(ref1 ++ int1Nil ++ str1, "non", ref1 ++ str1)
        }
      }

      test("bond.attr1.[attr2]") {
        test("e") {
          setEid1(ref1 ++ str1 ++ e1, "man", ref1 ++ str1 ++ e1)
          setEid1(ref1 ++ str1 ++ e1, "tac", ref1 ++ str1 ++ e1_)
          setEid1(ref1 ++ str1 ++ e1, "non", ref1 ++ str1)
        }

        test("e_") {
          setEid1(ref1 ++ str1 ++ e1_, "man", ref1 ++ str1 ++ e1)
          setEid1(ref1 ++ str1 ++ e1_, "tac", ref1 ++ str1 ++ e1_)
          setEid1(ref1 ++ str1 ++ e1_, "non", ref1 ++ str1)
        }

        test("int") {
          setInt1(ref1 ++ str1 ++ int1, "man", ref1 ++ str1 ++ int1)
          setInt1(ref1 ++ str1 ++ int1, "tac", ref1 ++ str1 ++ int1_)
          setInt1(ref1 ++ str1 ++ int1, "opt", ref1 ++ str1 ++ int1$)
          setInt1(ref1 ++ str1 ++ int1, "nil", ref1 ++ str1 ++ int1Nil)
          setInt1(ref1 ++ str1 ++ int1, "non", ref1 ++ str1)

          setInt1(ref1 ++ str1 ++ int1EqCoTx, "man", ref1 ++ str1 ++ int1EqCoTx)
          setInt1(ref1 ++ str1 ++ int1EqCoTx, "tac", ref1 ++ str1 ++ int1EqCoTx_)
          setInt1(ref1 ++ str1 ++ int1EqCoTx, "opt", ref1 ++ str1 ++ int1$)
          setInt1(ref1 ++ str1 ++ int1EqCoTx, "nil", ref1 ++ str1 ++ int1Nil)
          setInt1(ref1 ++ str1 ++ int1EqCoTx, "non", ref1 ++ str1)
        }

        test("int_") {
          setInt1(ref1 ++ str1 ++ int1_, "man", ref1 ++ str1 ++ int1)
          setInt1(ref1 ++ str1 ++ int1_, "tac", ref1 ++ str1 ++ int1_)
          setInt1(ref1 ++ str1 ++ int1_, "opt", ref1 ++ str1 ++ int1$)
          setInt1(ref1 ++ str1 ++ int1_, "nil", ref1 ++ str1 ++ int1Nil)
          setInt1(ref1 ++ str1 ++ int1_, "non", ref1 ++ str1)

          setInt1(ref1 ++ str1 ++ int1EqCoTx_, "man", ref1 ++ str1 ++ int1EqCoTx)
          setInt1(ref1 ++ str1 ++ int1EqCoTx_, "tac", ref1 ++ str1 ++ int1EqCoTx_)
          setInt1(ref1 ++ str1 ++ int1EqCoTx_, "opt", ref1 ++ str1 ++ int1$)
          setInt1(ref1 ++ str1 ++ int1EqCoTx_, "nil", ref1 ++ str1 ++ int1Nil)
          setInt1(ref1 ++ str1 ++ int1EqCoTx_, "non", ref1 ++ str1)
        }

        test("attr$") {
          setInt1(ref1 ++ str1 ++ int1$, "man", ref1 ++ str1 ++ int1)
          setInt1(ref1 ++ str1 ++ int1$, "tac", ref1 ++ str1 ++ int1_)
          setInt1(ref1 ++ str1 ++ int1$, "opt", ref1 ++ str1 ++ int1$)
          setInt1(ref1 ++ str1 ++ int1$, "nil", ref1 ++ str1 ++ int1Nil)
          setInt1(ref1 ++ str1 ++ int1$, "non", ref1 ++ str1)
        }

        test("attrNil") {
          setInt1(ref1 ++ str1 ++ int1Nil, "man", ref1 ++ str1 ++ int1)
          setInt1(ref1 ++ str1 ++ int1Nil, "tac", ref1 ++ str1 ++ int1_)
          setInt1(ref1 ++ str1 ++ int1Nil, "opt", ref1 ++ str1 ++ int1$)
          setInt1(ref1 ++ str1 ++ int1Nil, "nil", ref1 ++ str1 ++ int1Nil)
          setInt1(ref1 ++ str1 ++ int1Nil, "non", ref1 ++ str1)
        }
      }
    }


    test("dummy + e") {
      val non = List(Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()))
      val man = List(Generic("Aaa", "e", "datom", EntValue))
      val tac = List(Generic("Aaa", "e_", "datom", EntValue))

      setMode(non, List("" -> "Aaa"), "e", "none") ==> non
      setMode(non, List("" -> "Aaa"), "e", "mandatory") ==> man
      setMode(non, List("" -> "Aaa"), "e", "tacit") ==> tac

      setMode(man, List("" -> "Aaa"), "e", "none") ==> non
      setMode(man, List("" -> "Aaa"), "e", "mandatory") ==> man
      setMode(man, List("" -> "Aaa"), "e", "tacit") ==> tac
    }


    test("nil + attr") {
      val non = List(
        Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List())
      )
      val man = List(
        Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List()),
        Atom("Aaa", "ab", "ref", 1, VarValue, None, List(), List())
      )
      val tac = List(
        Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List()),
        Atom("Aaa", "ab_", "ref", 1, VarValue, None, List(), List())
      )
      val opt = List(
        Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List()),
        Atom("Aaa", "ab$", "ref", 1, VarValue, None, List(), List())
      )
      val nil = List(
        Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List()),
        Atom("Aaa", "ab_", "ref", 1, Fn("not", None), None, List(), List())
      )

      setMode(non, List("" -> "Aaa"), "ab", "mandatory") ==> man
      setMode(non, List("" -> "Aaa"), "ab", "tacit") ==> tac
      setMode(non, List("" -> "Aaa"), "ab", "optional") ==> opt
      setMode(non, List("" -> "Aaa"), "ab", "nil") ==> nil
      setMode(non, List("" -> "Aaa"), "ab", "none") ==> non

      setMode(man, List("" -> "Aaa"), "ab", "mandatory") ==> man
      setMode(man, List("" -> "Aaa"), "ab", "tacit") ==> tac
      setMode(man, List("" -> "Aaa"), "ab", "nil") ==> nil
      setMode(man, List("" -> "Aaa"), "ab", "optional") ==> opt
      setMode(man, List("" -> "Aaa"), "ab", "none") ==> non

      setMode(tac, List("" -> "Aaa"), "ab", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa"), "ab", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa"), "ab", "optional") ==> opt
      setMode(tac, List("" -> "Aaa"), "ab", "nil") ==> nil
      setMode(tac, List("" -> "Aaa"), "ab", "none") ==> non

      setMode(opt, List("" -> "Aaa"), "ab", "mandatory") ==> man
      setMode(opt, List("" -> "Aaa"), "ab", "tacit") ==> tac
      setMode(opt, List("" -> "Aaa"), "ab", "optional") ==> opt
      setMode(opt, List("" -> "Aaa"), "ab", "nil") ==> nil
      setMode(opt, List("" -> "Aaa"), "ab", "none") ==> non

      setMode(nil, List("" -> "Aaa"), "ab", "mandatory") ==> man
      setMode(nil, List("" -> "Aaa"), "ab", "tacit") ==> tac
      setMode(nil, List("" -> "Aaa"), "ab", "optional") ==> opt
      setMode(nil, List("" -> "Aaa"), "ab", "nil") ==> nil
      setMode(nil, List("" -> "Aaa"), "ab", "none") ==> non
    }


    test("dummy + attr") {
      val non = List(Atom("Aaa", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()))
      val man = List(Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()))
      val tac = List(Atom("Aaa", "attrA_", "String", 1, VarValue, None, List(), List()))
      val opt = List(Atom("Aaa", "attrA$", "String", 1, VarValue, None, List(), List()))
      val nil = List(Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List()))

      setMode(non, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(non, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(non, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(non, List("" -> "Aaa"), "attrA", "nil") ==> nil
      setMode(non, List("" -> "Aaa"), "attrA", "none") ==> non

      setMode(man, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(man, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(man, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(man, List("" -> "Aaa"), "attrA", "nil") ==> nil
      setMode(man, List("" -> "Aaa"), "attrA", "none") ==> non

      setMode(tac, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(tac, List("" -> "Aaa"), "attrA", "nil") ==> nil
      setMode(tac, List("" -> "Aaa"), "attrA", "none") ==> non

      setMode(opt, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(opt, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(opt, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(opt, List("" -> "Aaa"), "attrA", "nil") ==> nil
      setMode(opt, List("" -> "Aaa"), "attrA", "none") ==> non

      setMode(nil, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(nil, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(nil, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(nil, List("" -> "Aaa"), "attrA", "nil") ==> nil
      setMode(nil, List("" -> "Aaa"), "attrA", "none") ==> non
    }


    test("e + attr") {
      val non = List(
        Generic("Aaa", "e", "datom", EntValue))
      val man = List(
        Generic("Aaa", "e", "datom", EntValue),
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )
      val tac = List(
        Generic("Aaa", "e", "datom", EntValue),
        Atom("Aaa", "attrA_", "String", 1, VarValue, None, List(), List())
      )
      val opt = List(
        Generic("Aaa", "e", "datom", EntValue),
        Atom("Aaa", "attrA$", "String", 1, VarValue, None, List(), List())
      )
      val nil = List(
        Generic("Aaa", "e", "datom", EntValue),
        Atom("Aaa", "attrA_", "String", 1, Fn("not", None), None, List(), List())
      )

      setMode(non, List("" -> "Aaa"), "attrA", "none") ==> non
      setMode(non, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(non, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(non, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(non, List("" -> "Aaa"), "attrA", "nil") ==> nil

      setMode(man, List("" -> "Aaa"), "attrA", "none") ==> non
      setMode(man, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(man, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(man, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(man, List("" -> "Aaa"), "attrA", "nil") ==> nil

      setMode(tac, List("" -> "Aaa"), "attrA", "none") ==> non
      setMode(tac, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(tac, List("" -> "Aaa"), "attrA", "nil") ==> nil

      setMode(opt, List("" -> "Aaa"), "attrA", "none") ==> non
      setMode(opt, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(opt, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(opt, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(opt, List("" -> "Aaa"), "attrA", "nil") ==> nil

      setMode(nil, List("" -> "Aaa"), "attrA", "none") ==> non
      setMode(nil, List("" -> "Aaa"), "attrA", "mandatory") ==> man
      setMode(nil, List("" -> "Aaa"), "attrA", "tacit") ==> tac
      setMode(nil, List("" -> "Aaa"), "attrA", "optional") ==> opt
      setMode(nil, List("" -> "Aaa"), "attrA", "nil") ==> nil
    }


    test("attr + e") {
      val non = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )
      val man = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Generic("Aaa", "e", "datom", EntValue),
      )
      val tac = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Generic("Aaa", "e_", "datom", EntValue),
      )

      setMode(non, List("" -> "Aaa"), "e", "mandatory") ==> man
      setMode(non, List("" -> "Aaa"), "e", "tacit") ==> tac
      setMode(non, List("" -> "Aaa"), "e", "none") ==> non

      setMode(man, List("" -> "Aaa"), "e", "mandatory") ==> man
      setMode(man, List("" -> "Aaa"), "e", "tacit") ==> tac
      setMode(man, List("" -> "Aaa"), "e", "none") ==> non

      setMode(tac, List("" -> "Aaa"), "e", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa"), "e", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa"), "e", "none") ==> non
    }


    test("attr + attr") {
      val non = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List())
      )
      val man = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac", "ref", 1, VarValue, None, List(), List())
      )
      val tac = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac_", "ref", 1, VarValue, None, List(), List())
      )
      val opt = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac$", "ref", 1, VarValue, None, List(), List())
      )
      val nil = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Atom("Aaa", "ac_", "ref", 1, Fn("not", None), None, List(), List())
      )

      setMode(non, List("" -> "Aaa"), "ac", "none") ==> non
      setMode(non, List("" -> "Aaa"), "ac", "mandatory") ==> man
      setMode(non, List("" -> "Aaa"), "ac", "tacit") ==> tac
      setMode(non, List("" -> "Aaa"), "ac", "optional") ==> opt
      setMode(non, List("" -> "Aaa"), "ac", "nil") ==> nil

      setMode(man, List("" -> "Aaa"), "ac", "none") ==> non
      setMode(man, List("" -> "Aaa"), "ac", "mandatory") ==> man
      setMode(man, List("" -> "Aaa"), "ac", "tacit") ==> tac
      setMode(man, List("" -> "Aaa"), "ac", "optional") ==> opt
      setMode(man, List("" -> "Aaa"), "ac", "nil") ==> nil

      setMode(tac, List("" -> "Aaa"), "ac", "none") ==> non
      setMode(tac, List("" -> "Aaa"), "ac", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa"), "ac", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa"), "ac", "optional") ==> opt
      setMode(tac, List("" -> "Aaa"), "ac", "nil") ==> nil

      setMode(opt, List("" -> "Aaa"), "ac", "none") ==> non
      setMode(opt, List("" -> "Aaa"), "ac", "mandatory") ==> man
      setMode(opt, List("" -> "Aaa"), "ac", "tacit") ==> tac
      setMode(opt, List("" -> "Aaa"), "ac", "optional") ==> opt
      setMode(opt, List("" -> "Aaa"), "ac", "nil") ==> nil

      setMode(nil, List("" -> "Aaa"), "ac", "none") ==> non
      setMode(nil, List("" -> "Aaa"), "ac", "mandatory") ==> man
      setMode(nil, List("" -> "Aaa"), "ac", "tacit") ==> tac
      setMode(nil, List("" -> "Aaa"), "ac", "optional") ==> opt
      setMode(nil, List("" -> "Aaa"), "ac", "nil") ==> nil
    }


    test("attr + enum") {

      val non = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List())
      )
      val man = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List()),
        Atom("Ns", "enum", "String", 1, EnumVal, Some(":Ns.enum/"), List(), List())
      )
      val tac = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List()),
        Atom("Ns", "enum_", "String", 1, EnumVal, Some(":Ns.enum/"), List(), List())
      )
      val opt = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List()),
        Atom("Ns", "enum$", "String", 1, EnumVal, Some(":Ns.enum/"), List(), List())
      )
      val nil = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List()),
        Atom("Ns", "enum_", "String", 1, Fn("not", None), Some(":Ns.enum/"), List(), List())
      )

      setMode(non, List("" -> "Ns"), "enum", "none") ==> non
      setMode(non, List("" -> "Ns"), "enum", "mandatory") ==> man
      setMode(non, List("" -> "Ns"), "enum", "tacit") ==> tac
      setMode(non, List("" -> "Ns"), "enum", "optional") ==> opt
      setMode(non, List("" -> "Ns"), "enum", "nil") ==> nil

      setMode(man, List("" -> "Ns"), "enum", "none") ==> non
      setMode(man, List("" -> "Ns"), "enum", "mandatory") ==> man
      setMode(man, List("" -> "Ns"), "enum", "tacit") ==> tac
      setMode(man, List("" -> "Ns"), "enum", "optional") ==> opt
      setMode(man, List("" -> "Ns"), "enum", "nil") ==> nil

      setMode(tac, List("" -> "Ns"), "enum", "none") ==> non
      setMode(tac, List("" -> "Ns"), "enum", "mandatory") ==> man
      setMode(tac, List("" -> "Ns"), "enum", "tacit") ==> tac
      setMode(tac, List("" -> "Ns"), "enum", "optional") ==> opt
      setMode(tac, List("" -> "Ns"), "enum", "nil") ==> nil

      setMode(opt, List("" -> "Ns"), "enum", "none") ==> non
      setMode(opt, List("" -> "Ns"), "enum", "mandatory") ==> man
      setMode(opt, List("" -> "Ns"), "enum", "tacit") ==> tac
      setMode(opt, List("" -> "Ns"), "enum", "optional") ==> opt
      setMode(opt, List("" -> "Ns"), "enum", "nil") ==> nil

      setMode(nil, List("" -> "Ns"), "enum", "none") ==> non
      setMode(nil, List("" -> "Ns"), "enum", "mandatory") ==> man
      setMode(nil, List("" -> "Ns"), "enum", "tacit") ==> tac
      setMode(nil, List("" -> "Ns"), "enum", "optional") ==> opt
      setMode(nil, List("" -> "Ns"), "enum", "nil") ==> nil
    }


    test("Multiple branches") {
      val non = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List())
      )
      val man = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List())
      )
      val tac = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB_", "String", 1, VarValue, None, List(), List())
      )
      val opt = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB$", "String", 1, VarValue, None, List(), List())
      )
      val nil = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB_", "String", 1, Fn("not", None), None, List(), List())
      )

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
    }


    test("Multiple branches: attr$ -> dummy") {
      val non = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())

      )
      val man = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
      val tac = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB_", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
      val opt = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB$", "String", 1, VarValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
      val nil = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "attrB_", "String", 1, Fn("not", None), None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non

      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non

      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non

      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(opt, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non

      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "mandatory") ==> man
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "tacit") ==> tac
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "optional") ==> opt
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "nil") ==> nil
      setMode(nil, List("" -> "Aaa", "ab" -> "Bbb"), "attrB", "none") ==> non
    }


    test("Multiple branches: e -> dummy") {
      val non = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Atom("Bbb", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())

      )
      val man = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Generic("Bbb", "e", "datom", EntValue),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )
      val tac = List(
        Atom("Aaa", "attrA", "String", 1, VarValue, None, List(), List()),
        Bond("Aaa", "ab", "Bbb", 1, List()),
        Generic("Bbb", "e_", "datom", EntValue),
        ReBond("Aaa"),
        Bond("Aaa", "ac", "Ccc", 1, List()),
        Atom("Ccc", "attrC", "String", 1, VarValue, None, List(), List())
      )

      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "e", "mandatory") ==> man
      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "e", "tacit") ==> tac
      setMode(non, List("" -> "Aaa", "ab" -> "Bbb"), "e", "none") ==> non

      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "e", "mandatory") ==> man
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "e", "tacit") ==> tac
      setMode(man, List("" -> "Aaa", "ab" -> "Bbb"), "e", "none") ==> non

      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "e", "mandatory") ==> man
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "e", "tacit") ==> tac
      setMode(tac, List("" -> "Aaa", "ab" -> "Bbb"), "e", "none") ==> non
    }


    test("Multiple branches: e -> dummy") {
      val m1 = List(
        Generic("ind_Person", "e", "datom", EntValue),
        Atom("ind_Person", "name", "String", 1, VarValue, None, Seq(), Seq()),
        Bond("ind_Person", "nationality", "loc_Country", 1, Seq()),
        Generic("loc_Country", "e", "datom", EntValue))
    }
  }
}