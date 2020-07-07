package moleculeadmin.shared.ops.query.data

import moleculeadmin.shared.ast.query.{Filter, QueryResult}
import moleculeadmin.shared.util.PredicateMerger._
import scala.language.existentials


trait FilterIndex {

  def getFilterIndex(
    qr: QueryResult,
    filters: Map[Int, Filter[_]],
    curStars: Set[Long],
    curFlags: Set[Long],
    curChecks: Set[Long],
    sortIndex: Array[Int] = Array.empty[Int]
  ): Array[Int] = {

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

    def lambdaMap(
      values: Array[Option[Map[String, String]]],
      predicate: Option[String] => Boolean
    ): Int => Boolean = {
      if (sortIndex.isEmpty) {
        i: Int =>
          values(i) match {
            case Some(pairs) => pairs.exists {
              case (k, v) => predicate(Some(s"$k -> $v"))
            }
            case None        => predicate(None)
          }
      } else {
        i: Int =>
          values(sortIndex(i)) match {
            case Some(pairs) => pairs.exists {
              case (k, v) => predicate(Some(s"$k -> $v"))
            }
            case None        => predicate(None)
          }
      }
    }

    def double(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.num(arrayIndex)
      val predicate  = f.markerPred((curStars, curFlags, curChecks))
      lambdaOne(values, predicate.asInstanceOf[Option[Double] => Boolean])
    }
    def string(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.str(arrayIndex)
      val predicate  = f.markerPred((curStars, curFlags, curChecks))
      lambdaOne(values, predicate.asInstanceOf[Option[String] => Boolean])
    }

    def listDouble(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.listNum(arrayIndex)
      val predicate  = f.markerPred((curStars, curFlags, curChecks))
      lambdaMany(values, predicate.asInstanceOf[Option[Double] => Boolean])
    }
    def listString(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.listStr(arrayIndex)
      val predicate  = f.markerPred((curStars, curFlags, curChecks))
      lambdaMany(values, predicate.asInstanceOf[Option[String] => Boolean])
    }

    def mapDouble(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      // Convert Double values to String values
      val values     = qr.mapNum(arrayIndex).map { optMap =>
        optMap.map { valueMap =>
          valueMap.map { case (k, v) => (k, v.toString) }
        }
      }
      val predicate  = f.markerPred((curStars, curFlags, curChecks))
      lambdaMap(values, predicate.asInstanceOf[Option[String] => Boolean])
    }
    def mapString(f: Filter[_]): Int => Boolean = {
      val arrayIndex = qr.arrayIndexes(f.colIndex)
      val values     = qr.mapStr(arrayIndex)
      val predicate  = f.markerPred((curStars, curFlags, curChecks))
      lambdaMap(values, predicate.asInstanceOf[Option[String] => Boolean])
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
          case "mapDouble"  => mapDouble(f)
          case "mapString"  => mapString(f)
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
