package moleculeadmin.sharedtest

import molecule.ast.model.{Atom, Bond, Element, EntValue, EnumVal, Eq, Fn, Generic, Model, NoValue, ReBond, VarValue}
import molecule.ops.VerifyRawModel
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema, mBrainzSchema}
import moleculeadmin.shared.util.HelpersAdmin
import moleculeadmin.sharedtest.query.attr.SetMode.setMode
import moleculeadmin.sharedtest.query.attr.ToggleMode.{e, e_, int, int$, intEqCoTx, intEqCoTx_, int_, toggleMode, toggleNs}
import moleculeadmin.sharedtest.query.branch.AddNsAttr.addNs
import moleculeadmin.sharedtest.query.branch.Branch.isolateBranch
import utest._
import scala.languageFeature.implicitConversions._
import scala.util.Random


object Adhoc extends TestSuite
  with TreeSchema with TreeOps {


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

    test("Adhoc") {

      toggleNs(e ++ int, "e", e_ ++ int)
      toggleNs(e ++ int, "int", e ++ int_ )
      toggleNs(e ++ int, "str", e ++ int ++ str)

      toggleNs(e ++ int_, "e", e_ ++ int_ )
      toggleNs(e ++ int_, "int", e ++ int$)
      toggleNs(e ++ int_, "str", e ++ int_ ++ str)

      toggleNs(e ++ intEqCoTx, "e", e_ ++ intEqCoTx)
      toggleNs(e ++ intEqCoTx, "int", e ++ intEqCoTx_)
      toggleNs(e ++ intEqCoTx, "str", e ++ intEqCoTx ++ str)
    }
  }
}