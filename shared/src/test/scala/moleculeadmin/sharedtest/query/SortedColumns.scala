package moleculeadmin.sharedtest.query

import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.testdata.TreeSchema
import utest._


object SortedColumns extends TestSuite with TreeSchema with ColOps {

  val tests = Tests {

    def col(i: Int, sortI: Int, pos: Int): Col = {
      val sort = sortI match {
        case 0 => ""
        case 1 => "asc"
        case 2 => "desc"
      }
      Col(i,0,  "Ns", "Ns", "str", "String", "string", 1, false, Nil, "", "", sort, pos)
    }


    def cols1(s1: Int, p1: Int = 0): Seq[Col] = Seq(col(0, s1, p1))

    def cols2(s1: Int, s2: Int,
              p1: Int = 0,
              p2: Int = 0,
             ): Seq[Col] = {
      Seq(
        col(0, s1, p1),
        col(1, s2, p2),
      )
    }

    def cols3(s1: Int, s2: Int, s3: Int,
              p1: Int = 0,
              p2: Int = 0,
              p3: Int = 0
             ): Seq[Col] = {
      Seq(
        col(0, s1, p1),
        col(1, s2, p2),
        col(2, s3, p3),
      )
    }


    test("Toggle") {
      getSortedColumns(cols1(0), 0, false) ==> cols1(1)
      getSortedColumns(cols1(1), 0, false) ==> cols1(2)
      getSortedColumns(cols1(2), 0, false) ==> cols1(0)

      // No difference with shift
      getSortedColumns(cols1(0), 0, true) ==> cols1(1)
      getSortedColumns(cols1(1), 0, true) ==> cols1(2)
      getSortedColumns(cols1(2), 0, true) ==> cols1(0)
    }


    test("Replace 1") {
      getSortedColumns(cols2(0, 1), 0, false) ==> cols2(1, 0)
      getSortedColumns(cols2(0, 2), 0, false) ==> cols2(1, 0)
    }


    test("Replace 2") {
      getSortedColumns(cols3(0, 1, 1), 0, false) ==> cols3(1, 0, 0)
      getSortedColumns(cols3(1, 0, 1), 1, false) ==> cols3(0, 1, 0)
      getSortedColumns(cols3(1, 1, 0), 2, false) ==> cols3(0, 0, 1)
    }


    test("Add new sort to 1 prev") {
      // Notice position of sort columns
      getSortedColumns(
        cols2(0, 1, 0, 0), 0, true) ==>
        cols2(1, 1, 2, 1)
      getSortedColumns(
        cols2(1, 0, 0, 0), 1, true) ==>
        cols2(1, 1, 1, 2)

      getSortedColumns(
        cols3(0, 0, 1, 0, 0, 0), 0, true) ==>
        cols3(1, 0, 1, 2, 0, 1)
      getSortedColumns(
        cols3(0, 0, 1, 0, 0, 0), 1, true) ==>
        cols3(0, 1, 1, 0, 2, 1)
      getSortedColumns(
        cols3(0, 1, 0, 0, 0, 0), 0, true) ==>
        cols3(1, 1, 0, 2, 1, 0)
      getSortedColumns(
        cols3(0, 1, 0, 0, 0, 0), 2, true) ==>
        cols3(0, 1, 1, 0, 1, 2)
      getSortedColumns(
        cols3(1, 0, 0, 0, 0, 0), 1, true) ==>
        cols3(1, 1, 0, 1, 2, 0)
      getSortedColumns(
        cols3(1, 0, 0, 0, 0, 0), 2, true) ==>
        cols3(1, 0, 1, 1, 0, 2)
    }


    test("Add new sort to 2 prev") {
      getSortedColumns(
        cols3(0, 1, 1, 0, 1, 2), 0, true) ==>
        cols3(1, 1, 1, 3, 1, 2)
      getSortedColumns(
        cols3(0, 1, 1, 0, 2, 1), 0, true) ==>
        cols3(1, 1, 1, 3, 2, 1)

      getSortedColumns(
        cols3(1, 0, 1, 1, 0, 2), 1, true) ==>
        cols3(1, 1, 1, 1, 3, 2)
      getSortedColumns(
        cols3(1, 0, 1, 2, 0, 1), 1, true) ==>
        cols3(1, 1, 1, 2, 3, 1)

      getSortedColumns(
        cols3(1, 1, 0, 2, 1, 0), 2, true) ==>
        cols3(1, 1, 1, 2, 1, 3)
      getSortedColumns(
        cols3(1, 1, 0, 1, 2, 0), 2, true) ==>
        cols3(1, 1, 1, 1, 2, 3)
    }


    test("Toggle 1 of 2 prev") {
      getSortedColumns(
        cols2(1, 1, 1, 2), 0, true) ==>
        cols2(2, 1, 1, 2) // pos unchanged
      getSortedColumns(
        cols2(2, 1, 1, 2), 0, true) ==>
        cols2(0, 1, 0, 0) // 1 sort left, no positioning

      getSortedColumns(
        cols2(1, 1, 2, 1), 1, true) ==>
        cols2(1, 2, 2, 1) // pos unchanged

      getSortedColumns(
        cols2(1, 2, 2, 1), 1, true) ==>
        cols2(1, 0, 0, 0) // 1 sort left, no positioning
    }


    test("Toggle 1 of 3 prev") {
      getSortedColumns(
        cols3(1, 1, 1, 1, 2, 3), 0, true) ==>
        cols3(2, 1, 1, 1, 2, 3) // positions unchanged
      getSortedColumns(
        cols3(2, 1, 1, 1, 2, 3), 0, true) ==>
        cols3(0, 1, 1, 0, 1, 2) // 2 positions left
      getSortedColumns(
        cols3(1, 1, 1, 1, 3, 2), 0, true) ==>
        cols3(2, 1, 1, 1, 3, 2) // positions unchanged
      getSortedColumns(
        cols3(2, 1, 1, 1, 3, 2), 0, true) ==>
        cols3(0, 1, 1, 0, 2, 1) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 2, 1, 3), 0, true) ==>
        cols3(2, 1, 1, 2, 1, 3) // positions unchanged
      getSortedColumns(
        cols3(2, 1, 1, 2, 1, 3), 0, true) ==>
        cols3(0, 1, 1, 0, 1, 2) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 2, 3, 1), 0, true) ==>
        cols3(2, 1, 1, 2, 3, 1) // positions unchanged
      getSortedColumns(
        cols3(2, 1, 1, 2, 3, 1), 0, true) ==>
        cols3(0, 1, 1, 0, 2, 1) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 3, 1, 2), 0, true) ==>
        cols3(2, 1, 1, 3, 1, 2) // positions unchanged
      getSortedColumns(
        cols3(2, 1, 1, 3, 1, 2), 0, true) ==>
        cols3(0, 1, 1, 0, 1, 2) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 3, 2, 1), 0, true) ==>
        cols3(2, 1, 1, 3, 2, 1) // positions unchanged
      getSortedColumns(
        cols3(2, 1, 1, 3, 2, 1), 0, true) ==>
        cols3(0, 1, 1, 0, 2, 1) /* 2 positions left*/

      getSortedColumns(
        cols3(1, 1, 1, 1, 2, 3), 1, true) ==>
        cols3(1, 2, 1, 1, 2, 3) // positions unchanged
      getSortedColumns(
        cols3(1, 2, 1, 1, 2, 3), 1, true) ==>
        cols3(1, 0, 1, 1, 0, 2) // 2 positions left
      getSortedColumns(
        cols3(1, 1, 1, 1, 3, 2), 1, true) ==>
        cols3(1, 2, 1, 1, 3, 2) // positions unchanged
      getSortedColumns(
        cols3(1, 2, 1, 1, 3, 2), 1, true) ==>
        cols3(1, 0, 1, 1, 0, 2) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 2, 1, 3), 1, true) ==>
        cols3(1, 2, 1, 2, 1, 3) // positions unchanged
      getSortedColumns(
        cols3(1, 2, 1, 2, 1, 3), 1, true) ==>
        cols3(1, 0, 1, 1, 0, 2) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 2, 3, 1), 1, true) ==>
        cols3(1, 2, 1, 2, 3, 1) // positions unchanged
      getSortedColumns(
        cols3(1, 2, 1, 2, 3, 1), 1, true) ==>
        cols3(1, 0, 1, 2, 0, 1) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 3, 1, 2), 1, true) ==>
        cols3(1, 2, 1, 3, 1, 2) // positions unchanged
      getSortedColumns(
        cols3(1, 2, 1, 3, 1, 2), 1, true) ==>
        cols3(1, 0, 1, 2, 0, 1) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 3, 2, 1), 1, true) ==>
        cols3(1, 2, 1, 3, 2, 1) // positions unchanged
      getSortedColumns(
        cols3(1, 2, 1, 3, 2, 1), 1, true) ==>
        cols3(1, 0, 1, 2, 0, 1) /* 2 positions left*/

      getSortedColumns(
        cols3(1, 1, 1, 1, 2, 3), 2, true) ==>
        cols3(1, 1, 2, 1, 2, 3) // positions unchanged
      getSortedColumns(
        cols3(1, 1, 2, 1, 2, 3), 2, true) ==>
        cols3(1, 1, 0, 1, 2, 0) // 2 positions left
      getSortedColumns(
        cols3(1, 1, 1, 1, 3, 2), 2, true) ==>
        cols3(1, 1, 2, 1, 3, 2) // positions unchanged
      getSortedColumns(
        cols3(1, 1, 2, 1, 3, 2), 2, true) ==>
        cols3(1, 1, 0, 1, 2, 0) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 2, 1, 3), 2, true) ==>
        cols3(1, 1, 2, 2, 1, 3) // positions unchanged
      getSortedColumns(
        cols3(1, 1, 2, 2, 1, 3), 2, true) ==>
        cols3(1, 1, 0, 2, 1, 0) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 2, 3, 1), 2, true) ==>
        cols3(1, 1, 2, 2, 3, 1) // positions unchanged
      getSortedColumns(
        cols3(1, 1, 2, 2, 3, 1), 2, true) ==>
        cols3(1, 1, 0, 1, 2, 0) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 3, 1, 2), 2, true) ==>
        cols3(1, 1, 2, 3, 1, 2) // positions unchanged
      getSortedColumns(
        cols3(1, 1, 2, 3, 1, 2), 2, true) ==>
        cols3(1, 1, 0, 2, 1, 0) /* 2 positions left*/
      getSortedColumns(
        cols3(1, 1, 1, 3, 2, 1), 2, true) ==>
        cols3(1, 1, 2, 3, 2, 1) // positions unchanged
      getSortedColumns(
        cols3(1, 1, 2, 3, 2, 1), 2, true) ==>
        cols3(1, 1, 0, 2, 1, 0) /* 2 positions left*/
    }
  }
}