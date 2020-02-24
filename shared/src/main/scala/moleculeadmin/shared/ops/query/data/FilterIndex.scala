package moleculeadmin.shared.ops.query.data

import moleculeadmin.shared.ast.query.{Filter, QueryResult}
import moleculeadmin.shared.util.PredicateMerger._


trait FilterIndex {

  def getFilterIndex(
    qr: QueryResult,
    filters: Map[Int, Filter[_]],
    sortIndex: Array[Int] = Array.empty[Int]
  ): Array[Int] = {

    // Card one lambdas

    // None doesn't satisfy any predicate.
    // (Use nil in query buildup to get non-values)
    def lambdaOne[T](
      values: Array[Option[T]],
      predicate: Option[T] => Boolean
    ): Int => Boolean = {
      if (sortIndex.isEmpty) {
        i: Int => predicate(values(i))
      } else {
        i: Int => predicate(values(sortIndex(i)))
      }
    }

    // Card many lambdas

    // At least one value in a Set should satisfy the predicate
    def lambdaMany[T](
      values: Array[Option[List[T]]],
      predicate: Option[T] => Boolean
    ): Int => Boolean = {
      if (sortIndex.isEmpty) {
        i: Int =>
          values(i) match {
            case Some(vs) => vs.exists(v => predicate(Some(v)))
            case None     => predicate(None)
          }
      } else {
        i: Int =>
          values(sortIndex(i)) match {
            case Some(vs) => vs.exists(v => predicate(Some(v)))
            case None     => predicate(None)
          }
      }
    }

    def double(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.num(arrayIndex)
      val predicate  = f.pred.asInstanceOf[Option[Double] => Boolean]
      lambdaOne(values, predicate)
    }
    def string(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.str(arrayIndex)
      val predicate  = f.pred.asInstanceOf[Option[String] => Boolean]
      lambdaOne(values, predicate)
    }

    def listDouble(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.listNum(arrayIndex)
      val predicate  = f.pred.asInstanceOf[Option[Double] => Boolean]
      lambdaMany(values, predicate)
    }
    def listString(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.listStr(arrayIndex)
      val predicate  = f.pred.asInstanceOf[Option[String] => Boolean]
      lambdaMany(values, predicate)
    }

    var i                                = 0
    var posIndex                         = 0
    val lastRow                          = qr.rowCount
    val positives                        = new Array[Int](lastRow)
    val predicates: List[Int => Boolean] = filters.values.toList.map { f =>
      if (f.isAggr)
        double(f)
      else
        f.colType match {
          case "double"     => double(f)
          case "string"     => string(f)
          case "listDouble" => listDouble(f)
          case "listString" => listString(f)
          case _            => _: Int => true
        }
    }

    if (filters.size == 1) {
      val p = predicates.head
      if (sortIndex.isEmpty)
        while (i < lastRow) {
          if (p(i)) {
            positives(posIndex) = i
            posIndex += 1
          }
          i += 1
        }
      else
        while (i < lastRow) {
          if (p(i)) {
            positives(posIndex) = sortIndex(i)
            posIndex += 1
          }
          i += 1
        }
    } else {
      val pp  = predicates
      // Apply filters from all columns with AND logic
      val ppp = pp.tail.foldLeft(pp.head)(_ and _)
      if (sortIndex.isEmpty)
        while (i < lastRow) {
          if (ppp(i)) {
            positives(posIndex) = i
            posIndex += 1
          }
          i += 1
        }
      else
        while (i < lastRow) {
          if (ppp(i)) {
            positives(posIndex) = sortIndex(i)
            posIndex += 1
          }
          i += 1
        }
    }

    // Truncate filter index
    val filteredIndex = new Array[Int](posIndex)
    System.arraycopy(positives, 0, filteredIndex, 0, posIndex)
    filteredIndex
  }
}
