package moleculeadmin.sharedtest.query.model

import java.lang.IllegalArgumentException
import molecule.api.core.recreateDbFrom
import molecule.ast.model._
import moleculeadmin.shared.ops.query.ModelOps
import utest._

object ModelOpsTest extends TestSuite with ModelOps {


  val tests = Tests {

    test("toggleEdit") {
      val m0 = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "parent", "Ns", 1, Seq()),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()))

      val m1 = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")),
        Bond("Ns", "parent", "Ns", 1, Seq()),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()))

      val m2 = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "parent", "Ns", 1, Seq()),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")))

      val m3 = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")),
        Bond("Ns", "parent", "Ns", 1, Seq()),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")))


      // toggle on
      toggleEdit(m0, 0, "Ns", "int") ==> m1
      toggleEdit(m0, 1, "Ns", "int") ==> m2
      toggleEdit(m1, 2, "Ns", "int") ==> m3
      toggleEdit(m2, 0, "Ns", "int") ==> m3

      // toggle off
      toggleEdit(m1, 1, "Ns", "int") ==> m0
      toggleEdit(m2, 2, "Ns", "int") ==> m0
      toggleEdit(m3, 1, "Ns", "int") ==> m2
      toggleEdit(m3, 3, "Ns", "int") ==> m1

      try {
        // Can't toggle on attr once editing and marked as `orig`
        toggleEdit(m1, 0, "Ns", "int")
      } catch {
        case e: IllegalArgumentException =>
          e.getMessage ==> "Unexpected col index 0 for toggling edit"
      }

      val m4 = List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")))

      val m5 = List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()))

      toggleEdit(m4, 2, "Ns", "int") ==> m5
    }


    test("Invalid queries 1") {
      val none = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "Dummy to keep ns open", "", 1, NoValue, None, Seq(), Seq()))

      emptyNamespaces(none).nonEmpty ==> true // catches invalid state
      hasIncompleteBranches(none) ==> false // should be true

      val mandatory = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1", "Int", 1, VarValue, None, Seq(), Seq()))

      emptyNamespaces(mandatory).nonEmpty ==> false
      hasIncompleteBranches(mandatory) ==> false

      val tacit = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1_", "Int", 1, VarValue, None, Seq(), Seq()))

      emptyNamespaces(tacit).nonEmpty ==> false
      hasIncompleteBranches(tacit) ==> false

      val optional = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1$", "Int", 1, VarValue, None, Seq(), Seq()))

      emptyNamespaces(optional).nonEmpty ==> false
      hasIncompleteBranches(optional) ==> false

      val nil = List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1_", "Int", 1, Fn("not", None), None, Seq(), Seq()))

      emptyNamespaces(nil).nonEmpty ==> false
      hasIncompleteBranches(nil) ==> false
    }


    test("Invalid queries 2") {
      val none = List(
        Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "Dummy to keep ns open", "", 1, NoValue, None, Seq(), Seq()))

      emptyNamespaces(none).nonEmpty ==> true // catches invalid state
      hasIncompleteBranches(none) ==> false // should be true

      val mandatory = List(
        Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1", "Int", 1, VarValue, None, Seq(), Seq()))

      emptyNamespaces(mandatory).nonEmpty ==> false
      hasIncompleteBranches(mandatory) ==> false

      val tacit = List(
        Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1_", "Int", 1, VarValue, None, Seq(), Seq()))

      emptyNamespaces(tacit).nonEmpty ==> false
      hasIncompleteBranches(tacit) ==> true

      val optional = List(
        Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1$", "Int", 1, VarValue, None, Seq(), Seq()))

      emptyNamespaces(optional).nonEmpty ==> false
      hasIncompleteBranches(optional) ==> true

      val nil = List(
        Atom("Ns", "int_", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1_", "Int", 1, Fn("not", None), None, Seq(), Seq()))

      emptyNamespaces(nil).nonEmpty ==> false
      hasIncompleteBranches(nil) ==> true
    }
  }
}