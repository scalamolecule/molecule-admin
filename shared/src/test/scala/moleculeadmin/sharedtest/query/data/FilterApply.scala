package moleculeadmin.sharedtest.query.data

import moleculeadmin.shared.ast.query.{Col, Filter, QueryResult}
import moleculeadmin.shared.ops.query.data.{FilterFactory, FilterIndex}
import utest._
import scala.language.existentials


object FilterApply extends TestSuite with FilterFactory with FilterIndex {

  val tests = Tests {

    test("filter 3 columns") {

      val col0 = Array[Option[Double]](Some(0), Some(1), Some(1), Some(1))
      val col1 = Array[Option[Double]](Some(1), Some(1), Some(2), Some(2))
      val col2 = Array[Option[Double]](Some(1), Some(1), Some(2), Some(3))

      val qr = QueryResult(
        Nil,
        List(col0, col1, col2), // 3 number arrayIndexes
        Nil, Nil, Nil, Nil,
        Map(0 -> 0, 1 -> 1, 2 -> 2),
        4, 4,
        777
      )

      def mkFilter(
        colIndex: Int,
        attrType: String,
        colType: String,
        filterExpr: String
      ): Filter[_] = createFilter(
        Col(colIndex, 0, "", "", "", attrType, colType, 1, false, Nil, "", "", "", 0), filterExpr
      ).get

      val f0 = 0 -> mkFilter(0, "Int", "double", "1")
      val f1 = 1 -> mkFilter(1, "Int", "double", "2")
      val f2 = 2 -> mkFilter(2, "Int", "double", "3")


      getFilterIndex(qr, Map(f0)).toList ==> List(1, 2, 3)
      getFilterIndex(qr, Map(f1)).toList ==> List(2, 3)
      getFilterIndex(qr, Map(f2)).toList ==> List(3)

      getFilterIndex(qr, Map(f0, f1)).toList ==> List(2, 3)
      getFilterIndex(qr, Map(f0, f2)).toList ==> List(3)
      getFilterIndex(qr, Map(f1, f2)).toList ==> List(3)

      getFilterIndex(qr, Map(f0, f1, f2)).toList ==> List(3)

      // Predicate order doesn't matter
      getFilterIndex(qr, Map(f2, f1, f0)).toList ==> List(3)
    }


    test("filter + sort") {

      val col = Array[Option[Double]](Some(2), Some(1), Some(4), Some(3))

      // sort indexes to order `col`
      val sortAsc  = Array(1, 0, 3, 2) // --> 1, 2, 3, 4
      val sortDesc = Array(2, 3, 0, 1) // --> 4, 3, 2, 1

      val qr = QueryResult(
        Nil, List(col), Nil, Nil, Nil, Nil,
        Map(0 -> 0),
        4, 4, 777)

      val filters = Map(0 -> Filter(0, "double", false, ">1", (opt: Option[Double]) => opt.fold(false)(_ > 1)))


      // filter only
      val filteredIndex = getFilterIndex(qr, filters).toList
      filteredIndex ==> List(0, 2, 3)
      filteredIndex.flatMap(col(_)) ==> List(2, 4, 3)

      // filter + sortAsc index
      val filteredAndAscIndex: Seq[Int] = getFilterIndex(qr, filters, sortAsc).toList
      filteredAndAscIndex ==> List(0, 3, 2)
      filteredAndAscIndex.flatMap(col(_)) ==> List(2, 3, 4)

      // filter + sortDesc index
      val filteredAndDescIndex = getFilterIndex(qr, filters, sortDesc).toList
      filteredAndDescIndex ==> List(2, 3, 0)
      filteredAndDescIndex.flatMap(col(_)) ==> List(4, 3, 2)
    }


    test("filter + multi-sort") {

      val col1 = Array[Option[Double]](Some(2), Some(1), Some(2), Some(1))
      val col2 = Array[Option[Double]](Some(3), Some(3), Some(4), Some(4))

      // sort indexes to order `col`
      val sort1Asc  = Array(1, 3, 0, 2)
      val sort1Desc = Array(0, 2, 1, 3)

      val sort2Asc  = Array(0, 1, 2, 3)
      val sort2Desc = Array(2, 3, 0, 1)

      val sortAscAsc   = Array(1, 3, 0, 2)
      val sortAscDesc  = Array(3, 1, 2, 0)
      val sortDescAsc  = Array(0, 2, 1, 3)
      val sortDescDesc = Array(2, 0, 3, 1)

      val qr = QueryResult(
        Nil, List(col1, col2), Nil, Nil, Nil, Nil,
        Map(0 -> 0, 1 -> 1),
        4, 4, 777
      )

      val f1 = 0 -> Filter(0, "double", false, "1", (opt: Option[Double]) => opt.fold(false)(_ == 1))


      val noSort = getFilterIndex(qr, Map(f1)).toList
      noSort ==> List(1, 3)
      noSort.flatMap(col1(_)) ==> List(1, 1)
      noSort.flatMap(col2(_)) ==> List(3, 4)

      val asc1 = getFilterIndex(qr, Map(f1), sort1Asc).toList
      asc1 ==> List(1, 3)
      asc1.flatMap(col1(_)) ==> List(1, 1)
      asc1.flatMap(col2(_)) ==> List(3, 4)

      val desc1 = getFilterIndex(qr, Map(f1), sort1Desc).toList
      desc1 ==> List(1, 3)
      desc1.flatMap(col1(_)) ==> List(1, 1)
      desc1.flatMap(col2(_)) ==> List(3, 4)

      val asc2 = getFilterIndex(qr, Map(f1), sort2Asc).toList
      asc2 ==> List(1, 3)
      asc2.flatMap(col1(_)) ==> List(1, 1)
      asc2.flatMap(col2(_)) ==> List(3, 4)

      val desc2 = getFilterIndex(qr, Map(f1), sort2Desc).toList
      desc2 ==> List(3, 1)
      desc2.flatMap(col1(_)) ==> List(1, 1)
      desc2.flatMap(col2(_)) ==> List(4, 3)

      val ascAsc = getFilterIndex(qr, Map(f1), sortAscAsc).toList
      ascAsc ==> List(1, 3)
      ascAsc.flatMap(col1(_)) ==> List(1, 1)
      ascAsc.flatMap(col2(_)) ==> List(3, 4)

      val ascDesc = getFilterIndex(qr, Map(f1), sortAscDesc).toList
      ascDesc ==> List(3, 1)
      ascDesc.flatMap(col1(_)) ==> List(1, 1)
      ascDesc.flatMap(col2(_)) ==> List(4, 3)

      val descAsc = getFilterIndex(qr, Map(f1), sortDescAsc).toList
      descAsc ==> List(1, 3)
      descAsc.flatMap(col1(_)) ==> List(1, 1)
      descAsc.flatMap(col2(_)) ==> List(3, 4)

      val descDesc = getFilterIndex(qr, Map(f1), sortDescDesc).toList
      descDesc ==> List(3, 1)
      descDesc.flatMap(col1(_)) ==> List(1, 1)
      descDesc.flatMap(col2(_)) ==> List(4, 3)

      //      // filter + sortDesc index
      //      val filteredAndDescIndex = processedIndex(qr, Map(f1), sortDesc).toList
      //      filteredAndDescIndex ==> List(2, 3, 0)
      //      filteredAndDescIndex.flatMap(col(_)) ==> List(4, 3, 2)


      val f2 = 0 -> Filter(0, "double", false, "2", (opt: Option[Double]) => opt.fold(false)(_ == 2))
    }
  }
}
