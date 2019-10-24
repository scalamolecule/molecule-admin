package moleculeadmin.sharedtest.ops.query.branch

import molecule.ast.model._
import moleculeadmin.shared.ops.query.attr.AttrOps
import moleculeadmin.shared.testdata.TreeSchema
import moleculeadmin.sharedtest.ops.query.attr.UpsertAttr.upsertAttr
import utest._
import scala.languageFeature.implicitConversions._


object UpsertBranch extends TestSuite with TreeSchema with AttrOps {

  // Applied value
  val eq333 = Eq(Seq(333))
  val eq444 = Eq(Seq(444))
  val eqNil = Eq(Nil)
  val fnMin = Fn("min", None)
  val fnCou = Fn("count", None)
  val fnCoD = Fn("count-distinct", None)
  val fnSum = Fn("sum", None)
  val fnAvg = Fn("avg", None)
  val fnMed = Fn("median", None)
  val fnVar = Fn("variance", None)
  val fnStD = Fn("stddev", None)
  val fnT__ = Fn("t", None)
  val fnTx_ = Fn("tx", None)
  val fnTxI = Fn("txInstant", None)


  val dum = List(Atom("Ns", "Dummy to keep ns open", "", 1, NoValue))

  val e      = List(Generic("Ns", "e", "datom", EntValue))
  val eEq333 = List(Generic("Ns", "e", "datom", eq333))
  val eEq444 = List(Generic("Ns", "e", "datom", eq444))
  val eCount = List(Generic("Ns", "e", "datom", fnCou))

  val e_      = List(Generic("Ns", "e_", "datom", EntValue))
  val eEq333_ = List(Generic("Ns", "e_", "datom", eq333))
  val eCount_ = List(Generic("Ns", "e_", "datom", fnCou))

  val int      = List(Atom("Ns", "int", "Int", 1, VarValue))
  val intEq333 = List(Atom("Ns", "int", "Int", 1, eq333))
  val intEq444 = List(Atom("Ns", "int", "Int", 1, eq444))
  val intMin__ = List(Atom("Ns", "int", "Int", 1, fnMin))
  val intCount = List(Atom("Ns", "int", "Int", 1, fnCou))
  val intCounD = List(Atom("Ns", "int", "Int", 1, fnCoD))
  val intSum__ = List(Atom("Ns", "int", "Int", 1, fnSum))
  val intAvg__ = List(Atom("Ns", "int", "Int", 1, fnAvg))
  val intMed__ = List(Atom("Ns", "int", "Int", 1, fnMed))
  val intVar__ = List(Atom("Ns", "int", "Int", 1, fnVar))
  val intStd__ = List(Atom("Ns", "int", "Int", 1, fnStD))
  val intT____ = List(Generic("Ns", "t", "datom", NoValue))
  val intTx___ = List(Generic("Ns", "tx", "datom", NoValue))
  val intTxIns = List(Generic("Ns", "txInstant", "datom", NoValue))

  val int_      = List(Atom("Ns", "int_", "Int", 1, VarValue))
  val int_Eq333 = List(Atom("Ns", "int_", "Int", 1, eq333))
  val int_Eq444 = List(Atom("Ns", "int_", "Int", 1, eq444))
  val int_Min__ = List(Atom("Ns", "int_", "Int", 1, fnMin))

  val str  = List(Atom("Ns", "str", "String", 1, VarValue))
  val str_ = List(Atom("Ns", "str_", "String", 1, VarValue))
  val long = List(Atom("Ns", "long", "Long", 1, VarValue))


  val ref1    = List(Bond("Ns", "ref1", "Ref1", 1, Seq()))
  val dum1    = List(Atom("Ref1", "Dummy to keep ns open", "", 1, NoValue))
  val e1      = List(Generic("Ref1", "e", "datom", EntValue))
  val e1Eq333 = List(Generic("Ref1", "e", "datom", eq333))
  val e1Eq444 = List(Generic("Ref1", "e", "datom", eq444))
  val e1Count = List(Generic("Ref1", "e", "datom", fnCou))

  val int1      = List(Atom("Ref1", "int1", "Int", 1, VarValue))
  val int1Eq333 = List(Atom("Ref1", "int1", "Int", 1, eq333))
  val int1Count = List(Atom("Ref1", "int1", "Int", 1, fnCou))

  val e1_        = List(Generic("Ref1", "e_", "datom", EntValue))
  val e1Eq333_   = List(Generic("Ref1", "e_", "datom", eq333))
  val e1Count_   = List(Generic("Ref1", "e_", "datom", fnCou))
  val int1_      = List(Atom("Ref1", "int1_", "Int", 1, VarValue))
  val int1_Eq333 = List(Atom("Ref1", "int1_", "Int", 1, eq333))
  val int1_Count = List(Atom("Ref1", "int1_", "Int", 1, fnCou))

  val str1  = List(Atom("Ref1", "str1", "String", 1, VarValue))
  val str1_ = List(Atom("Ref1", "str1_", "String", 1, VarValue))


  def int(additives: String*) = List(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), additives))
  def int_(additives: String*) = List(Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), additives))

  def int0Count(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("count")))
  def int0CounD(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("count-distinct")))
  def int0Sum__(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("sum")))
  def int0Avg__(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("avg")))
  def int0Med__(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("median")))
  def int0Var__(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("variance")))
  def int0Std__(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("stddev")))
  def int0T____(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("t")))
  def int0Tx___(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("tx")))
  def int0TxIns(v: Value = VarValue) = List(Atom("Ns", "int", "Int", 1, v, None, Seq(), Seq("txInstant")))

  def int0_Count(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("count")))
  def int0_CounD(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("count-distinct")))
  def int0_Sum__(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("sum")))
  def int0_Avg__(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("avg")))
  def int0_Med__(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("median")))
  def int0_Var__(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("variance")))
  def int0_Std__(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("stddev")))
  def int0_T____(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("t")))
  def int0_Tx___(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("tx")))
  def int0_TxIns(v: Value = VarValue) = List(Atom("Ns", "int_", "Int", 1, v, None, Seq(), Seq("txInstant")))

  def int0$Count(v: Value = VarValue) = List(Atom("Ns", "int$", "Int", 1, v, None, Seq(), Seq("count")))

  def int1Count(v: Value = VarValue) = List(Atom("Ref1", "int1", "Int", 1, v, None, Seq(), Seq("count")))
  def int1_Count(v: Value = VarValue) = List(Atom("Ref1", "int1_", "Int", 1, v, None, Seq(), Seq("count")))


  def upsertEid(base: Seq[Element], v: Value, result: Seq[Element]): Unit = {
    upsertBranch(base, List("" -> "Ns"), "e", "datom", 1, None, v) ==> result
  }
  def upsertEid1(base: Seq[Element], v: Value, result: Seq[Element]): Unit = {
    upsertBranch(base, List("" -> "Ns", "ref1" -> "Ref1"), "e", "datom", 1, None, v) ==> result
  }

  def upsertInt(base: Seq[Element], v: Value, result: Seq[Element]): Unit = {
    upsertBranch(base, List("" -> "Ns"), "int", "Int", 1, None, v) ==> result
  }
  def upsertInt1(base: Seq[Element], v: Value, result: Seq[Element]): Unit = {
    upsertBranch(base, List("" -> "Ns", "ref1" -> "Ref1"), "int1", "Int", 1, None, v) ==> result
  }


  val tests = Tests {

    test("e") {

      // previous-current-subsequent attributes from outset
      test("0-0-0") {
        upsertEid(dum, eqNil, e)
        upsertEid(dum, eq333, eEq333)
        upsertEid(dum, fnCou, eCount)
      }

      test("0-1-0") {
        upsertEid(e, eqNil, e)
        upsertEid(e, eq333, eEq333)
        upsertEid(e, fnCou, eCount)

        upsertEid(eCount, eqNil, e)
        upsertEid(eCount, eq333, eEq333)
        upsertEid(eCount, fnCou, e)

        upsertEid(eEq333, eqNil, e)
        upsertEid(eEq333, eq333, eEq333)
        upsertEid(eEq333, eq444, eEq444)
        upsertEid(eEq333, fnCou, eCount)


        upsertEid(e_, eqNil, e_)
        upsertEid(e_, eq333, eEq333_)
        upsertEid(e_, fnCou, eCount_)
      }

      test("1-0-0") {
        upsertEid(int, eqNil, int ++ e)
        upsertEid(int, eq333, int ++ eEq333)
        upsertEid(int, fnCou, int ++ eCount)
      }

      test("1-1-0") {
        upsertEid(int ++ e, eqNil, int ++ e)
        upsertEid(int ++ e, eq333, int ++ eEq333)
        upsertEid(int ++ e, fnCou, int ++ eCount)

        upsertEid(int ++ eCount, eqNil, int ++ e)
        upsertEid(int ++ eCount, eq333, int ++ eEq333)
        upsertEid(int ++ eCount, fnCou, int ++ e)

        upsertEid(int ++ eEq333, eqNil, int ++ e)
        upsertEid(int ++ eEq333, eq333, int ++ eEq333)
        upsertEid(int ++ eEq333, eq444, int ++ eEq444)
        upsertEid(int ++ eEq333, fnCou, int ++ eCount)
      }

      test("0-1-1") {
        upsertEid(e ++ int, eqNil, e ++ int)
        upsertEid(e ++ int, eq333, eEq333 ++ int)
        upsertEid(e ++ int, fnCou, eCount ++ int)

        upsertEid(eCount ++ int, eqNil, e ++ int)
        upsertEid(eCount ++ int, eq333, eEq333 ++ int)
        upsertEid(eCount ++ int, fnCou, e ++ int)

        upsertEid(eEq333 ++ int, eqNil, e ++ int)
        upsertEid(eEq333 ++ int, eq333, eEq333 ++ int)
        upsertEid(eEq333 ++ int, eq444, eEq444 ++ int)
        upsertEid(eEq333 ++ int, fnCou, eCount ++ int)
      }

      test("1-1-1") {
        upsertEid(int ++ e ++ str, eqNil, int ++ e ++ str)
        upsertEid(int ++ e ++ str, eq333, int ++ eEq333 ++ str)
        upsertEid(int ++ e ++ str, fnCou, int ++ eCount ++ str)

        upsertEid(int ++ eCount ++ str, eqNil, int ++ e ++ str)
        upsertEid(int ++ eCount ++ str, eq333, int ++ eEq333 ++ str)
        upsertEid(int ++ eCount ++ str, fnCou, int ++ e ++ str)

        upsertEid(int ++ eEq333 ++ str, eqNil, int ++ e ++ str)
        upsertEid(int ++ eEq333 ++ str, eq333, int ++ eEq333 ++ str)
        upsertEid(int ++ eEq333 ++ str, eq444, int ++ eEq444 ++ str)
        upsertEid(int ++ eEq333 ++ str, fnCou, int ++ eCount ++ str)
      }
    }


    test("attr") {

      test("0-0-0") {
        upsertInt(dum, eqNil, int)
        upsertInt(dum, eq333, intEq333)
        upsertInt(dum, fnCou, int0_Count() ++ intCount)
      }

      test("0-1-0") {
        upsertInt(int, eqNil, int)
        upsertInt(int, eq333, intEq333)
        upsertInt(int, fnMin, intMin__)
        upsertInt(int, fnCou, int0Count() ++ intCount)
        upsertInt(int, fnCoD, int0CounD() ++ intCounD)
        upsertInt(int, fnSum, int0Sum__() ++ intSum__)
        upsertInt(int, fnAvg, int0Avg__() ++ intAvg__)
        upsertInt(int, fnMed, int0Med__() ++ intMed__)
        upsertInt(int, fnVar, int0Var__() ++ intVar__)
        upsertInt(int, fnStD, int0Std__() ++ intStd__)
        upsertInt(int, fnT__, int0T____() ++ intT____)
        upsertInt(int, fnTx_, int0Tx___() ++ intTx___)
        upsertInt(int, fnTxI, int0TxIns() ++ intTxIns)

        // Applying fn shifts attr from tacit to mandatory
        upsertInt(int_, eqNil, int_)
        upsertInt(int_, eq333, int_Eq333)
        upsertInt(int_, fnMin, int_Min__)
        upsertInt(int_, fnCou, int0_Count() ++ intCount)
        upsertInt(int_, fnCoD, int0_CounD() ++ intCounD)
        upsertInt(int_, fnSum, int0_Sum__() ++ intSum__)
        upsertInt(int_, fnAvg, int0_Avg__() ++ intAvg__)
        upsertInt(int_, fnMed, int0_Med__() ++ intMed__)
        upsertInt(int_, fnVar, int0_Var__() ++ intVar__)
        upsertInt(int_, fnStD, int0_Std__() ++ intStd__)
        upsertInt(int_, fnT__, int0_T____() ++ intT____)
        upsertInt(int_, fnTx_, int0_Tx___() ++ intTx___)
        upsertInt(int_, fnTxI, int0_TxIns() ++ intTxIns)

        upsertInt(intEq333, eqNil, int) // toggle back to mandatory
        upsertInt(intEq333, eq333, intEq333) // unchanged
        upsertInt(intEq333, eq444, intEq444) // change to new value
        upsertInt(intEq333, fnMin, intMin__) // replace fn
        upsertInt(intEq333, fnCou, int0Count(eq333) ++ intCount)
        upsertInt(intEq333, fnCoD, int0CounD(eq333) ++ intCounD)
        upsertInt(intEq333, fnSum, int0Sum__(eq333) ++ intSum__)
        upsertInt(intEq333, fnAvg, int0Avg__(eq333) ++ intAvg__)
        upsertInt(intEq333, fnMed, int0Med__(eq333) ++ intMed__)
        upsertInt(intEq333, fnVar, int0Var__(eq333) ++ intVar__)
        upsertInt(intEq333, fnStD, int0Std__(eq333) ++ intStd__)
        upsertInt(intEq333, fnT__, int0T____(eq333) ++ intT____)
        upsertInt(intEq333, fnTx_, int0Tx___(eq333) ++ intTx___)
        upsertInt(intEq333, fnTxI, int0TxIns(eq333) ++ intTxIns)

        upsertInt(int_Eq333, eqNil, int_) // toggle back to tacit
        upsertInt(int_Eq333, eq333, int_Eq333) // unchanged
        upsertInt(int_Eq333, eq444, int_Eq444) // changed to new value
        upsertInt(int_Eq333, fnMin, int_Min__) // replace fn
        upsertInt(int_Eq333, fnCou, int0_Count(eq333) ++ intCount)
        upsertInt(int_Eq333, fnCoD, int0_CounD(eq333) ++ intCounD)
        upsertInt(int_Eq333, fnSum, int0_Sum__(eq333) ++ intSum__)
        upsertInt(int_Eq333, fnAvg, int0_Avg__(eq333) ++ intAvg__)
        upsertInt(int_Eq333, fnMed, int0_Med__(eq333) ++ intMed__)
        upsertInt(int_Eq333, fnVar, int0_Var__(eq333) ++ intVar__)
        upsertInt(int_Eq333, fnStD, int0_Std__(eq333) ++ intStd__)
        upsertInt(int_Eq333, fnT__, int0_T____(eq333) ++ intT____)
        upsertInt(int_Eq333, fnTx_, int0_Tx___(eq333) ++ intTx___)
        upsertInt(int_Eq333, fnTxI, int0_TxIns(eq333) ++ intTxIns)

        upsertInt(intMin__, eqNil, intMin__) // unchanged
        upsertInt(intMin__, eq333, intEq333) // replace fn
        upsertInt(intMin__, fnMin, int) // toggle back to mandatory
        upsertInt(intMin__, fnCou, int0Count(fnMin) ++ intCount)
        upsertInt(intMin__, fnCoD, int0CounD(fnMin) ++ intCounD)
        upsertInt(intMin__, fnSum, int0Sum__(fnMin) ++ intSum__)
        upsertInt(intMin__, fnAvg, int0Avg__(fnMin) ++ intAvg__)
        upsertInt(intMin__, fnMed, int0Med__(fnMin) ++ intMed__)
        upsertInt(intMin__, fnVar, int0Var__(fnMin) ++ intVar__)
        upsertInt(intMin__, fnStD, int0Std__(fnMin) ++ intStd__)
        upsertInt(intMin__, fnT__, int0T____(fnMin) ++ intT____)
        upsertInt(intMin__, fnTx_, int0Tx___(fnMin) ++ intTx___)
        upsertInt(intMin__, fnTxI, int0TxIns(fnMin) ++ intTxIns)

        upsertInt(int0Count() ++ intCount, eqNil, int0Count() ++ intCount)
        upsertInt(int0Count() ++ intCount, eq333, int0Count(eq333) ++ intCount)
        upsertInt(int0Count() ++ intCount, fnMin, int0Count(fnMin) ++ intCount)
        upsertInt(int0Count() ++ intCount, fnCou, int) // toggle back to mandatory
        upsertInt(int0Count() ++ intCount, fnCoD, int("count", "count-distinct") ++ intCount ++ intCounD)
        upsertInt(int0Count() ++ intCount, fnSum, int("count", "sum") ++ intCount ++ intSum__)
        upsertInt(int0Count() ++ intCount, fnAvg, int("count", "avg") ++ intCount ++ intAvg__)
        upsertInt(int0Count() ++ intCount, fnMed, int("count", "median") ++ intCount ++ intMed__)
        upsertInt(int0Count() ++ intCount, fnVar, int("count", "variance") ++ intCount ++ intVar__)
        upsertInt(int0Count() ++ intCount, fnStD, int("count", "stddev") ++ intCount ++ intStd__)
        upsertInt(int0Count() ++ intCount, fnT__, int("count", "t") ++ intCount ++ intT____)
        upsertInt(int0Count() ++ intCount, fnTx_, int("count", "tx") ++ intCount ++ intTx___)
        upsertInt(int0Count() ++ intCount, fnTxI, int("count", "txInstant") ++ intCount ++ intTxIns)

        // Order is fixed
        upsertInt(int0CounD() ++ intCounD, fnCou, int("count", "count-distinct") ++ intCount ++ intCounD)

        upsertInt(int0_Count() ++ intCount, eqNil, int0_Count() ++ intCount)
        upsertInt(int0_Count() ++ intCount, eq333, int0_Count(eq333) ++ intCount)
        upsertInt(int0_Count() ++ intCount, fnMin, int0_Count(fnMin) ++ intCount)
        upsertInt(int0_Count() ++ intCount, fnCou, int_) // toggle back to tacit
        upsertInt(int0_Count() ++ intCount, fnCoD, int_("count", "count-distinct") ++ intCount ++ intCounD)
        upsertInt(int0_Count() ++ intCount, fnSum, int_("count", "sum") ++ intCount ++ intSum__)
        upsertInt(int0_Count() ++ intCount, fnAvg, int_("count", "avg") ++ intCount ++ intAvg__)
        upsertInt(int0_Count() ++ intCount, fnMed, int_("count", "median") ++ intCount ++ intMed__)
        upsertInt(int0_Count() ++ intCount, fnVar, int_("count", "variance") ++ intCount ++ intVar__)
        upsertInt(int0_Count() ++ intCount, fnStD, int_("count", "stddev") ++ intCount ++ intStd__)
        upsertInt(int0_Count() ++ intCount, fnT__, int_("count", "t") ++ intCount ++ intT____)
        upsertInt(int0_Count() ++ intCount, fnTx_, int_("count", "tx") ++ intCount ++ intTx___)
        upsertInt(int0_Count() ++ intCount, fnTxI, int_("count", "txInstant") ++ intCount ++ intTxIns)
      }

      test("1-0-0") {
        upsertInt(str, eqNil, str ++ int)
        upsertInt(str, eq333, str ++ intEq333)
        upsertInt(str, fnMin, str ++ intMin__)
        upsertInt(str, fnCou, str ++ int0_Count() ++ intCount)
      }

      test("1-1-0") {
        upsertInt(str ++ int, eqNil, str ++ int)
        upsertInt(str ++ int, eq333, str ++ intEq333)
        upsertInt(str ++ int, fnMin, str ++ intMin__)
        upsertInt(str ++ int, fnCou, str ++ int0Count() ++ intCount)
      }

      test("1-1-1") {
        upsertInt(str ++ int ++ long, eqNil, str ++ int ++ long)
        upsertInt(str ++ int ++ long, eq333, str ++ intEq333 ++ long)
        upsertInt(str ++ int ++ long, fnMin, str ++ intMin__ ++ long)
        upsertInt(str ++ int ++ long, fnCou, str ++ int0Count() ++ intCount ++ long)
      }
    }


    test("[attr].bond") {

      // prev-cur-sub in branch
      test("0-0-0.bond") {
        upsertEid(ref1 ++ dum1, eqNil, e ++ ref1 ++ dum1)
        upsertEid(ref1 ++ dum1, eq333, eEq333 ++ ref1 ++ dum1)
        upsertEid(ref1 ++ dum1, fnCou, eCount ++ ref1 ++ dum1)

        upsertInt(ref1 ++ dum1, eqNil, int ++ ref1 ++ dum1)
        upsertInt(ref1 ++ dum1, eq333, intEq333 ++ ref1 ++ dum1)
        upsertInt(ref1 ++ dum1, fnCou, int0_Count() ++ intCount ++ ref1 ++ dum1)
      }

      test("0-1-0.bond") {
        upsertEid(e ++ ref1 ++ dum1, eqNil, e ++ ref1 ++ dum1)
        upsertEid(e ++ ref1 ++ dum1, eq333, eEq333 ++ ref1 ++ dum1)
        upsertEid(e ++ ref1 ++ dum1, fnCou, eCount ++ ref1 ++ dum1)

        upsertEid(e_ ++ ref1 ++ dum1, eqNil, e_ ++ ref1 ++ dum1)
        upsertEid(e_ ++ ref1 ++ dum1, eq333, eEq333_ ++ ref1 ++ dum1)
        upsertEid(e_ ++ ref1 ++ dum1, fnCou, eCount_ ++ ref1 ++ dum1)

        upsertInt(int ++ ref1 ++ dum1, eqNil, int ++ ref1 ++ dum1)
        upsertInt(int ++ ref1 ++ dum1, eq333, intEq333 ++ ref1 ++ dum1)
        upsertInt(int ++ ref1 ++ dum1, fnCou, int0Count() ++ intCount ++ ref1 ++ dum1)

        upsertInt(int_ ++ ref1 ++ dum1, eqNil, int_ ++ ref1 ++ dum1)
        upsertInt(int_ ++ ref1 ++ dum1, eq333, int_Eq333 ++ ref1 ++ dum1)
        upsertInt(int_ ++ ref1 ++ dum1, fnCou, int0_Count() ++ intCount ++ ref1 ++ dum1)

        upsertInt(intEq333 ++ ref1 ++ dum1, eqNil, int ++ ref1 ++ dum1)
        upsertInt(intEq333 ++ ref1 ++ dum1, eq333, intEq333 ++ ref1 ++ dum1)
        upsertInt(intEq333 ++ ref1 ++ dum1, fnCou, int0Count(eq333) ++ intCount ++ ref1 ++ dum1)

        upsertInt(int_Eq333 ++ ref1 ++ dum1, eqNil, int_ ++ ref1 ++ dum1)
        upsertInt(int_Eq333 ++ ref1 ++ dum1, eq333, int_Eq333 ++ ref1 ++ dum1)
        upsertInt(int_Eq333 ++ ref1 ++ dum1, fnCou, int0_Count(eq333) ++ intCount ++ ref1 ++ dum1)

        upsertInt(int0Count() ++ intCount ++ ref1 ++ dum1, eqNil, int0Count() ++ intCount ++ ref1 ++ dum1)
        upsertInt(int0Count() ++ intCount ++ ref1 ++ dum1, eq333, int0Count(eq333) ++ intCount ++ ref1 ++ dum1)
        upsertInt(int0Count() ++ intCount ++ ref1 ++ dum1, fnCou, int ++ ref1 ++ dum1)

        upsertInt(int0_Count() ++ intCount ++ ref1 ++ dum1, eqNil, int0_Count() ++ intCount ++ ref1 ++ dum1)
        upsertInt(int0_Count() ++ intCount ++ ref1 ++ dum1, eq333, int0_Count(eq333) ++ intCount ++ ref1 ++ dum1)
        upsertInt(int0_Count() ++ intCount ++ ref1 ++ dum1, fnCou, int_ ++ ref1 ++ dum1)
      }

      test("1-0-0.bond") {
        upsertInt(str ++ ref1 ++ dum1, eqNil, str ++ int ++ ref1 ++ dum1)
        upsertInt(str ++ ref1 ++ dum1, eq333, str ++ intEq333 ++ ref1 ++ dum1)

        // Presuming that we only want to see the count
        upsertInt(str ++ ref1 ++ dum1, fnCou, str ++ int0_Count() ++ intCount ++ ref1 ++ dum1)

        upsertInt(str_ ++ ref1 ++ dum1, eqNil, str_ ++ int ++ ref1 ++ dum1)
        upsertInt(str_ ++ ref1 ++ dum1, eq333, str_ ++ intEq333 ++ ref1 ++ dum1)
        upsertInt(str_ ++ ref1 ++ dum1, fnCou, str_ ++ int0_Count() ++ intCount ++ ref1 ++ dum1)
      }
    }


    test("bond.[attr]") {

      // prev-cur-sub in branch
      test("bond.0-0-0") {
        upsertEid1(ref1 ++ dum1, eqNil, ref1 ++ e1)
        upsertEid1(ref1 ++ dum1, eq333, ref1 ++ e1Eq333)
        upsertEid1(ref1 ++ dum1, fnCou, ref1 ++ e1Count)

        upsertInt1(ref1 ++ dum1, eqNil, ref1 ++ int1)
        upsertInt1(ref1 ++ dum1, eq333, ref1 ++ int1Eq333)
        upsertInt1(ref1 ++ dum1, fnCou, ref1 ++ int1_Count() ++ int1Count)
      }

      test("bond.0-1-0") {
        upsertEid1(ref1 ++ e1, eqNil, ref1 ++ e1)
        upsertEid1(ref1 ++ e1, eq333, ref1 ++ e1Eq333)
        upsertEid1(ref1 ++ e1, fnCou, ref1 ++ e1Count)

        upsertEid1(ref1 ++ e1_, eqNil, ref1 ++ e1_)
        upsertEid1(ref1 ++ e1_, eq333, ref1 ++ e1Eq333_)
        upsertEid1(ref1 ++ e1_, fnCou, ref1 ++ e1Count_)

        upsertInt1(ref1 ++ int1, eqNil, ref1 ++ int1)
        upsertInt1(ref1 ++ int1, eq333, ref1 ++ int1Eq333)
        upsertInt1(ref1 ++ int1, fnCou, ref1 ++ int1Count() ++ int1Count)

        upsertInt1(ref1 ++ int1_, eqNil, ref1 ++ int1_)
        upsertInt1(ref1 ++ int1_, eq333, ref1 ++ int1_Eq333)
        upsertInt1(ref1 ++ int1_, fnCou, ref1 ++ int1_Count() ++ int1Count)

        upsertInt1(ref1 ++ int1Eq333, eqNil, ref1 ++ int1)
        upsertInt1(ref1 ++ int1Eq333, eq333, ref1 ++ int1Eq333)
        upsertInt1(ref1 ++ int1Eq333, fnCou, ref1 ++ int1Count(eq333) ++ int1Count)

        upsertInt1(ref1 ++ int1_Eq333, eqNil, ref1 ++ int1_)
        upsertInt1(ref1 ++ int1_Eq333, eq333, ref1 ++ int1_Eq333)
        upsertInt1(ref1 ++ int1_Eq333, fnCou, ref1 ++ int1_Count(eq333) ++ int1Count)

        upsertInt1(ref1 ++ int1Count() ++ int1Count, eqNil, ref1 ++ int1Count() ++ int1Count)
        upsertInt1(ref1 ++ int1Count() ++ int1Count, eq333, ref1 ++ int1Count(eq333) ++ int1Count)
        upsertInt1(ref1 ++ int1Count() ++ int1Count, fnCou, ref1 ++ int1)

        upsertInt1(ref1 ++ int1_Count() ++ int1Count, eqNil, ref1 ++ int1_Count() ++ int1Count)
        upsertInt1(ref1 ++ int1_Count() ++ int1Count, eq333, ref1 ++ int1_Count(eq333) ++ int1Count)
        upsertInt1(ref1 ++ int1_Count() ++ int1Count, fnCou, ref1 ++ int1_)
      }

      test("bond.1-0-0") {
        upsertInt1(ref1 ++ str1, eqNil, ref1 ++ str1 ++ int1)
        upsertInt1(ref1 ++ str1, eq333, ref1 ++ str1 ++ int1Eq333)

        // Presuming that we only want to see the count
        upsertInt1(ref1 ++ str1, fnCou, ref1 ++ str1 ++ int1_Count() ++ int1Count)

        upsertInt1(ref1 ++ str1_, eqNil, ref1 ++ str1_ ++ int1)
        upsertInt1(ref1 ++ str1_, eq333, ref1 ++ str1_ ++ int1Eq333)
        upsertInt1(ref1 ++ str1_, fnCou, ref1 ++ str1_ ++ int1_Count() ++ int1Count)
      }


      test("int") {
        upsertEid(ref1, eqNil, e ++ ref1)
        upsertEid(ref1, eq333, eEq333 ++ ref1)
        upsertEid(ref1, fnCou, eCount ++ ref1)

        upsertEid(ref1, eqNil, e ++ ref1)
        upsertEid(ref1, eq333, eEq333 ++ ref1)
        upsertEid(ref1, fnCou, eCount ++ ref1)

        upsertEid(ref1 ++ int1, eqNil, e ++ ref1 ++ int1)
        upsertEid(ref1 ++ int1, eq333, eEq333 ++ ref1 ++ int1)
        upsertEid(ref1 ++ int1, fnCou, eCount ++ ref1 ++ int1)
      }


      test("ref1") {
        upsertInt(ref1, eqNil, int ++ ref1)
        upsertInt(ref1, eq333, intEq333 ++ ref1)
        upsertInt(ref1, fnCou, int0_Count() ++ intCount ++ ref1)

        upsertInt(ref1, eqNil, int ++ ref1)
        upsertInt(ref1, eq333, intEq333 ++ ref1)
        upsertInt(ref1, fnCou, int0_Count() ++ intCount ++ ref1)

        upsertInt(ref1 ++ int1, eqNil, int ++ ref1 ++ int1)
        upsertInt(ref1 ++ int1, eq333, intEq333 ++ ref1 ++ int1)
        upsertInt(ref1 ++ int1, fnCou, int0_Count() ++ intCount ++ ref1 ++ int1)
      }


      test("edit + t") {
        val m1 = List(
          Generic("Ns", "e", "datom", EntValue),
          Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
          Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit"))
        )

        val m2 = List(
          Generic("Ns", "e", "datom", EntValue),
          Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig", "t")),
          Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")),
          Generic("Ns", "t", "datom", NoValue)
        )

        upsertBranch(m1, List("" -> "Ns"), "int", "Int", 1, None, fnT__) ==> m2
        upsertBranch(m2, List("" -> "Ns"), "int", "Int", 1, None, fnT__) ==> m1
      }
    }
  }
}