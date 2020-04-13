package moleculeadmin.sharedtest.transform

import molecule.ast.model._
import molecule.util.Helpers
import moleculeadmin.shared.ops.transform.Model2Molecule
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object Model2MoleculeTest extends TestSuite with TreeSchema with Model2Molecule with Helpers {


  val tests = Tests {

    test("dummy") {
      model2molecule(List(Atom("Ns", "Dummy to keep ns open", "", 1, NoValue, None, List(), List()))) ==> "Ns"
    }

    test("attr") {
      model2molecule(List(Atom("Ns", "str", "String", 1, VarValue, None, List(), List()))) ==> "Ns.str"
    }

    test("attr_") {
      model2molecule(List(Atom("Ns", "str_", "String", 1, VarValue, None, List(), List()))) ==> "Ns.str_"
    }

    test("attr$") {
      model2molecule(List(Atom("Ns", "str$", "String", 1, VarValue, None, List(), List()))) ==> "Ns.str$"
    }

    test("e") {
      model2molecule(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "str", "String", 1, VarValue, None, List(), List()))) ==> "Ns.e.str"
    }

    test("fulltext") {
      model2molecule(List(Atom("Ns", "str", "String", 1, Fulltext(Seq("b")), None, List(), List()))) ==> """Ns.str.contains("b")"""
    }

    test("aggregates") {
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("min", None), None, List(), List()))) ==> "Ns.int(min)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("max", None), None, List(), List()))) ==> "Ns.int(max)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("min", Some(1)), None, List(), List()))) ==> "Ns.int(min)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("max", Some(1)), None, List(), List()))) ==> "Ns.int(max)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("min", Some(2)), None, List(), List()))) ==> "Ns.int(min(2))"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("max", Some(2)), None, List(), List()))) ==> "Ns.int(max(2))"

      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("rand", None), None, List(), List()))) ==> "Ns.str(rand)"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("sample", None), None, List(), List()))) ==> "Ns.str(sample)"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("rand", Some(1)), None, List(), List()))) ==> "Ns.str(rand)"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("sample", Some(1)), None, List(), List()))) ==> "Ns.str(sample)"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("rand", Some(2)), None, List(), List()))) ==> "Ns.str(rand(2))"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("sample", Some(2)), None, List(), List()))) ==> "Ns.str(sample(2))"

      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("count"), None, List(), List()))) ==> "Ns.str(count)"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("count-distinct"), None, List(), List()))) ==> "Ns.str(countDistinct)"
      model2molecule(List(Atom("Ns", "str", "String", 1, Fn("distinct"), None, List(), List()))) ==> "Ns.str(distinct)"

      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("sum"), None, List(), List()))) ==> "Ns.int(sum)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("avg"), None, List(), List()))) ==> "Ns.int(avg)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("median"), None, List(), List()))) ==> "Ns.int(median)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("variance"), None, List(), List()))) ==> "Ns.int(variance)"
      model2molecule(List(Atom("Ns", "int", "Long", 1, Fn("stddev"), None, List(), List()))) ==> "Ns.int(stddev)"
    }

    test("additives"){

      model2molecule(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq("tx")),
        Generic("Ns", "tx", "datom", NoValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()))) ==> "Ns.e.str.tx.int"
    }


    test("ref") {
      model2molecule(List(
        Bond("ind_Person", "nationality", "loc_Country", 1, Seq()),
        Atom("loc_Country", "name", "String", 1, VarValue, None, Seq(), Seq()))) ==> "ind_Person.Nationality.name"
    }

    test("ref.e(eid)") {
      model2molecule(List(
        Generic("Ns", "e", "datom", EntValue),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Generic("Ref1", "e_", "datom", Eq(Seq(42L))),
        Atom("Ref1", "int1", "Int", 1, VarValue, None, Seq(), Seq())
      )) ==> "Ns.e.Ref1.e_(42L).int1"
    }

    test("Backref with partitions") {
      model2molecule(List(
        Atom("a_Aa", "aa1", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("a_Aa", "abb", "b_Bb", 1, Seq()),
        Atom("b_Bb", "bb1", "Int", 1, VarValue, None, Seq(), Seq()),
        ReBond("a_Aa"),
        Atom("a_Aa", "acc", "ref", 1, VarValue, None, Seq(), Seq())
      )) ==> "a_Aa.aa1.Abb.bb1._a_Aa.acc"
    }
  }
}