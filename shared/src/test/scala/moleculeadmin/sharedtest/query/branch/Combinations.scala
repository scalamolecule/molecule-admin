package moleculeadmin.sharedtest.query.branch

import molecule.ast.model._
import moleculeadmin.shared.ops.query.builder.TreeOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._
import scala.languageFeature.implicitConversions._


object Combinations extends TestSuite with TreeSchema with TreeOps {

  val tests = Tests {

    val combinations = Seq(
      Seq(0),
      Seq(1),
      Seq(2),
      Seq(0, 1),
      Seq(0, 2),
      Seq(1, 2),
      Seq(0, 1, 2)
    )
    val abcd = Seq("a", "b", "c", "d")
    val indexes = Seq(0, 1, 2, 3)
    val nss = Map(
      0 -> "Aaa",
      1 -> "Bbb",
      2 -> "Ccc",
      3 -> "Ddd",
    )
    val nss_ = Map(
      "Aaa" -> 0,
      "Bbb" -> 1,
      "Ccc" -> 2,
      "Ddd" -> 3,
    )
    val attrs = Map(
      0 -> "attrA",
      1 -> "attrB",
      2 -> "attrC",
      3 -> "attrD",
    )

    val ll1 = for {
      a <- Seq(0, 1, 2)
      x <- combinations
    } yield Seq(Seq(a, x))

    //  ll1 foreach println
    //  println("--------------")

    val ll2 = for {
      (a, b) <- Seq((0, 1), (0, 2), (1, 2))
      x <- combinations
      y <- combinations
    } yield Seq(Seq(a, x), Seq(b, y))

    //  ll2 foreach println
    //  println("--------------")

    val ll3 = for {
      x <- combinations
      y <- combinations
      z <- combinations
    } yield Seq(Seq(0, x), Seq(1, y), Seq(2, z))

    //  ll3.foreach(l => println("    " + l + ","))
    //  println("--------------")


    val l1 = List(
      List(List(0, List(0))),
      List(List(0, List(1))),
      List(List(0, List(2))),
      List(List(0, List(0, 1))),
      List(List(0, List(0, 2))),
      List(List(0, List(1, 2))),
      List(List(0, List(0, 1, 2))),

      List(List(1, List(0))),
      List(List(1, List(1))),
      List(List(1, List(2))),
      List(List(1, List(0, 1))),
      List(List(1, List(0, 2))),
      List(List(1, List(1, 2))),
      List(List(1, List(0, 1, 2))),

      List(List(2, List(0))),
      List(List(2, List(1))),
      List(List(2, List(2))),
      List(List(2, List(0, 1))),
      List(List(2, List(0, 2))),
      List(List(2, List(1, 2))),
      List(List(2, List(0, 1, 2)))
    )

    val l2 = List(
      List(List(0, List(0)), List(1, List(0))),
      List(List(0, List(0)), List(1, List(1))),
      List(List(0, List(0)), List(1, List(2))),
      List(List(0, List(0)), List(1, List(0, 1))),
      List(List(0, List(0)), List(1, List(0, 2))),
      List(List(0, List(0)), List(1, List(1, 2))),
      List(List(0, List(0)), List(1, List(0, 1, 2))),
      List(List(0, List(1)), List(1, List(0))),
      List(List(0, List(1)), List(1, List(1))),
      List(List(0, List(1)), List(1, List(2))),
      List(List(0, List(1)), List(1, List(0, 1))),
      List(List(0, List(1)), List(1, List(0, 2))),
      List(List(0, List(1)), List(1, List(1, 2))),
      List(List(0, List(1)), List(1, List(0, 1, 2))),
      List(List(0, List(2)), List(1, List(0))),
      List(List(0, List(2)), List(1, List(1))),
      List(List(0, List(2)), List(1, List(2))),
      List(List(0, List(2)), List(1, List(0, 1))),
      List(List(0, List(2)), List(1, List(0, 2))),
      List(List(0, List(2)), List(1, List(1, 2))),
      List(List(0, List(2)), List(1, List(0, 1, 2))),
      List(List(0, List(0, 1)), List(1, List(0))),
      List(List(0, List(0, 1)), List(1, List(1))),
      List(List(0, List(0, 1)), List(1, List(2))),
      List(List(0, List(0, 1)), List(1, List(0, 1))),
      List(List(0, List(0, 1)), List(1, List(0, 2))),
      List(List(0, List(0, 1)), List(1, List(1, 2))),
      List(List(0, List(0, 1)), List(1, List(0, 1, 2))),
      List(List(0, List(0, 2)), List(1, List(0))),
      List(List(0, List(0, 2)), List(1, List(1))),
      List(List(0, List(0, 2)), List(1, List(2))),
      List(List(0, List(0, 2)), List(1, List(0, 1))),
      List(List(0, List(0, 2)), List(1, List(0, 2))),
      List(List(0, List(0, 2)), List(1, List(1, 2))),
      List(List(0, List(0, 2)), List(1, List(0, 1, 2))),
      List(List(0, List(1, 2)), List(1, List(0))),
      List(List(0, List(1, 2)), List(1, List(1))),
      List(List(0, List(1, 2)), List(1, List(2))),
      List(List(0, List(1, 2)), List(1, List(0, 1))),
      List(List(0, List(1, 2)), List(1, List(0, 2))),
      List(List(0, List(1, 2)), List(1, List(1, 2))),
      List(List(0, List(1, 2)), List(1, List(0, 1, 2))),
      List(List(0, List(0, 1, 2)), List(1, List(0))),
      List(List(0, List(0, 1, 2)), List(1, List(1))),
      List(List(0, List(0, 1, 2)), List(1, List(2))),
      List(List(0, List(0, 1, 2)), List(1, List(0, 1))),
      List(List(0, List(0, 1, 2)), List(1, List(0, 2))),
      List(List(0, List(0, 1, 2)), List(1, List(1, 2))),
      List(List(0, List(0, 1, 2)), List(1, List(0, 1, 2)))
    )


    def buildElements(basePath: Seq[Int], combs: Seq[Any], model0: Seq[Element], depth0: Int = 0): (Int, Seq[Int], Seq[Element]) = {
      val last = combs.size - 1
      combs.zipWithIndex.foldLeft(depth0, basePath, model0) {

        case ((0, path, model), (comb: Seq[_], `last`)) =>
          buildElements(path, comb, model, model.size)

        case ((d, path, model), (comb: Seq[_], _)) =>
          val (d1, path1, sub) = buildElements(path, comb, model, d)
          val (newPath, rebonds) = if (basePath == path && path != path1) {
            (basePath.intersect(path1), path1.init.reverse.map(i => ReBond(nss(i))))
          } else
            (path1, Nil)
          val newDepth = d1 - rebonds.size
          (newDepth, newPath, sub ++ rebonds)

        case ((d, path, model), (opt: Int, _)) =>
          val baseIndex = basePath.last
          val refIndex = indexes.filterNot(_ == baseIndex)(opt)
          val (ns, refNs, attr, x, y) = (nss(baseIndex), nss(refIndex), attrs(refIndex), abcd(baseIndex), abcd(refIndex))
          val rebonds = if (basePath != path) (basePath.last +: path.diff(basePath).init).reverse.map(i => ReBond(nss(i))) else Nil
          val bond = Bond(ns, x + y, refNs, 1)
          val atom = Atom(refNs, attr, "String", 1, VarValue)
          val newDepth = d + 1 - rebonds.size
          val newPath = if (rebonds.nonEmpty) path else path :+ refIndex
          (newDepth, newPath, model ++ (rebonds :+ bond :+ atom))
      }
    }

    test("go") {
      //  ll3.zipWithIndex.foreach { case (l, i) =>
      ll3.zipWithIndex.slice(134, 135).foreach { case (l, i) =>
        println(s"==> " + (i + 1) + " ==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>==>\n" + l + "\n")

        //    val elements = buildElements(Seq(0), l, Seq(Atom("Aaa", "attrA", "String", 1, VarValue)))
        val elements = buildElements(Seq(0), l, Seq(Atom("Aaa", "attrA", "String", 1, VarValue)))._3

        //    val before = List(
        //      Atom("Loc_Municipality", "attrA", "String", 1, VarValue, None, List(), List())
        //    )
        //
        //      val after = List(
        //        Atom("Loc_Municipality", "attrA", "String", 1, VarValue, None, List(), List()),
        //        Bond("loc_Municipality", "county", "loc_County", 1, List()),
        //        Atom("Loc_County", "name", "String", 1, VarValue, None, List(), List())
        //      )
        //      Data_views.addNsAttr(before, Seq("" -> "loc_Municipality"), "county", "loc_County", "name").code ==> after.code
        //      Data_views.removeNs(after, Seq("" -> "loc_Municipality", "county" -> "loc_County")).code ==> before.code

        elements foreach println
        println("")
        println(modelTreeFormatted(mkModelTree(elements)))
        println("")
        println(mkTree(elements).toString)

      }
    }
  }
}