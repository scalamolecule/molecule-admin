package moleculeadmin.shared.ops.query.data

import moleculeadmin.shared.ast.query.{Col, QueryResult}


trait SortIndex {

  def getSortIndex(
    qr: QueryResult,
    sortCols: Seq[Col],
    rowCount: Int
  ): Array[Int] = {
    sortCols.size match {
      case 1 => sortArray1(qr, sortCols)
      case 2 => sortArray2(qr, sortCols, rowCount)
      case 3 => sortArray3(qr, sortCols, rowCount)
      case 4 => sortArray4(qr, sortCols, rowCount)
      case 5 => sortArray5(qr, sortCols, rowCount)
      case n => throw new IllegalArgumentException(s"Can only sort by 1-5 columns. Found $n")
    }
  }

  private def ord[T](ordering: Ordering[T], sort: String): Ordering[T] =
    if (sort == "asc") ordering else ordering.reverse

  private def str(qr: QueryResult, i: Int): Array[String] =
    qr.str(i).map(_.getOrElse(""))

  private def dou(qr: QueryResult, i: Int): Array[Double] =
    qr.num(i).map(_.getOrElse(scala.Double.MinValue)) // empty values last


  private def tpe(attrType: String, dataType: String, aggrType: String): String = {
    lazy val number = Seq("Int", "Long", "Float", "Double").contains(attrType)
    aggrType match {
      case "aggrInt"                    => "double"
      case "aggrDouble"                 => "double"
      case "aggrSingle" if number       => "double"
      case "aggrSingle"                 => "string"
      case "aggrSingleSample" if number => "double"
      case "aggrSingleSample"           => "string"
      case _                            => dataType
    }
  }

  private def sortArray1(qr: QueryResult, sortCols: Seq[Col]): Array[Int] = {
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, _, _, _) = sortCols.head

    val i1 = qr.arrayIndexes(colIndex1)
    tpe(t1, dt1, at1) match {
      case "string" => indexArray1[String](str(qr, i1), s1)
      case "double" => indexArray1[Double](dou(qr, i1), s1)
      case a        => println(s"ILLEGAL column type name: $a"); null
    }
  }

  private def indexArray1[T](
    array1: Array[T],
    sort1: String
  )(implicit ord1: Ordering[T]): Array[Int] =
    array1.zipWithIndex.sortBy { case (v, i) => v }(ord(ord1, sort1)).map(_._2)


  private def sortArray2(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val sortColsSorted                                                    = sortCols.sortBy(_.sortPos)
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, _, _, _) = sortColsSorted.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, _, _, _) = sortColsSorted(1)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)

    (tpe(t1, dt1, at1), tpe(t2, dt2, at2)) match {
      case ("string", "string") => indexArray2[String, String](numRows, str(qr, i1), str(qr, i2), s1, s2)
      case ("string", "double") => indexArray2[String, Double](numRows, str(qr, i1), dou(qr, i2), s1, s2)
      case ("double", "string") => indexArray2[Double, String](numRows, dou(qr, i1), str(qr, i2), s1, s2)
      case ("double", "double") => indexArray2[Double, Double](numRows, dou(qr, i1), dou(qr, i2), s1, s2)
      case (a, b)               => println(s"ILLEGAL column type names: ($a, $b)"); null
    }
  }

  private def indexArray2[T1, T2](
    rowCount: Int,
    array1: Array[T1],
    array2: Array[T2],
    sort1: String,
    sort2: String
  )(implicit
    ord1: Ordering[T1],
    ord2: Ordering[T2]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i))
      i += 1
    }
    val ordering = Ordering.Tuple2(ord(ord1, sort1), ord(ord2, sort2))
    indexArray.sortBy { case (_, v1, v2) => (v1, v2) }(ordering).map(_._1)
  }


  private def sortArray3(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val sortColsSorted                                                    = sortCols.sortBy(_.sortPos)
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, _, _, _) = sortColsSorted.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, _, _, _) = sortColsSorted(1)
    val Col(colIndex3, _, _, _, _, t3, dt3, _, _, _, at3, _, s3, _, _, _) = sortColsSorted(2)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)
    val i3 = qr.arrayIndexes(colIndex3)

    (tpe(t1, dt1, at1), tpe(t2, dt2, at2), tpe(t3, dt3, at3)) match {
      case ("string", "string", "string") => indexArray3[String, String, String](numRows, str(qr, i1), str(qr, i2), str(qr, i3), s1, s2, s3)
      case ("string", "string", "double") => indexArray3[String, String, Double](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), s1, s2, s3)
      case ("string", "double", "string") => indexArray3[String, Double, String](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), s1, s2, s3)
      case ("string", "double", "double") => indexArray3[String, Double, Double](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), s1, s2, s3)
      case ("double", "string", "string") => indexArray3[Double, String, String](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), s1, s2, s3)
      case ("double", "string", "double") => indexArray3[Double, String, Double](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), s1, s2, s3)
      case ("double", "double", "string") => indexArray3[Double, Double, String](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), s1, s2, s3)
      case ("double", "double", "double") => indexArray3[Double, Double, Double](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), s1, s2, s3)
      case (a, b, c)                      => println(s"ILLEGAL column types: ($a, $b, $c)"); null
    }
  }

  private def indexArray3[T1, T2, T3](
    rowCount: Int,
    array1: Array[T1],
    array2: Array[T2],
    array3: Array[T3],
    sort1: String,
    sort2: String,
    sort3: String
  )(implicit
    ord1: Ordering[T1],
    ord2: Ordering[T2],
    ord3: Ordering[T3]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2, T3)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i), array3(i))
      i += 1
    }
    val ordering = Ordering.Tuple3(
      ord(ord1, sort1),
      ord(ord2, sort2),
      ord(ord3, sort3))
    indexArray.sortBy {
      case (_, v1, v2, v3) => (v1, v2, v3)
    }(ordering).map(_._1)
  }


  private def sortArray4(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val sortColsSorted                                                    = sortCols.sortBy(_.sortPos)
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, _, _, _) = sortColsSorted.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, _, _, _) = sortColsSorted(1)
    val Col(colIndex3, _, _, _, _, t3, dt3, _, _, _, at3, _, s3, _, _, _) = sortColsSorted(2)
    val Col(colIndex4, _, _, _, _, t4, dt4, _, _, _, at4, _, s4, _, _, _) = sortColsSorted(3)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)
    val i3 = qr.arrayIndexes(colIndex3)
    val i4 = qr.arrayIndexes(colIndex4)

    (tpe(t1, dt1, at1), tpe(t2, dt2, at2), tpe(t3, dt3, at3), tpe(t4, dt4, at4)) match {
      case ("string", "string", "string", "string") => indexArray4[String, String, String, String](numRows, str(qr, i1), str(qr, i2), str(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("string", "string", "string", "double") => indexArray4[String, String, String, Double](numRows, str(qr, i1), str(qr, i2), str(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("string", "string", "double", "string") => indexArray4[String, String, Double, String](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("string", "string", "double", "double") => indexArray4[String, String, Double, Double](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("string", "double", "string", "string") => indexArray4[String, Double, String, String](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("string", "double", "string", "double") => indexArray4[String, Double, String, Double](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("string", "double", "double", "string") => indexArray4[String, Double, Double, String](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("string", "double", "double", "double") => indexArray4[String, Double, Double, Double](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("double", "string", "string", "string") => indexArray4[Double, String, String, String](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("double", "string", "string", "double") => indexArray4[Double, String, String, Double](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("double", "string", "double", "string") => indexArray4[Double, String, Double, String](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("double", "string", "double", "double") => indexArray4[Double, String, Double, Double](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("double", "double", "string", "string") => indexArray4[Double, Double, String, String](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("double", "double", "string", "double") => indexArray4[Double, Double, String, Double](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case ("double", "double", "double", "string") => indexArray4[Double, Double, Double, String](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), str(qr, i4), s1, s2, s3, s4)
      case ("double", "double", "double", "double") => indexArray4[Double, Double, Double, Double](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), dou(qr, i4), s1, s2, s3, s4)
      case (a, b, c, d)                             => println(s"ILLEGAL column types: ($a, $b, $c, $d)"); null
    }
  }

  private def indexArray4[T1, T2, T3, T4](
    rowCount: Int,
    array1: Array[T1],
    array2: Array[T2],
    array3: Array[T3],
    array4: Array[T4],
    sort1: String,
    sort2: String,
    sort3: String,
    sort4: String
  )(implicit
    ord1: Ordering[T1],
    ord2: Ordering[T2],
    ord3: Ordering[T3],
    ord4: Ordering[T4]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2, T3, T4)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i), array3(i), array4(i))
      i += 1
    }
    val ordering = Ordering.Tuple4(
      ord(ord1, sort1),
      ord(ord2, sort2),
      ord(ord3, sort3),
      ord(ord4, sort4))
    indexArray.sortBy {
      case (_, v1, v2, v3, v4) => (v1, v2, v3, v4)
    }(ordering).map(_._1)
  }


  private def sortArray5(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val sortColsSorted                                                    = sortCols.sortBy(_.sortPos)
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, _, _, _) = sortColsSorted.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, _, _, _) = sortColsSorted(1)
    val Col(colIndex3, _, _, _, _, t3, dt3, _, _, _, at3, _, s3, _, _, _) = sortColsSorted(2)
    val Col(colIndex4, _, _, _, _, t4, dt4, _, _, _, at4, _, s4, _, _, _) = sortColsSorted(3)
    val Col(colIndex5, _, _, _, _, t5, dt5, _, _, _, at5, _, s5, _, _, _) = sortColsSorted(4)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)
    val i3 = qr.arrayIndexes(colIndex3)
    val i4 = qr.arrayIndexes(colIndex4)
    val i5 = qr.arrayIndexes(colIndex5)

    (tpe(t1, dt1, at1), tpe(t2, dt2, at2), tpe(t3, dt3, at3), tpe(t4, dt4, at4), tpe(t5, dt5, at5)) match {
      case ("string", "string", "string", "string", "string") => indexArray5[String, String, String, String, String](numRows, str(qr, i1), str(qr, i2), str(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "string", "string", "double") => indexArray5[String, String, String, String, Double](numRows, str(qr, i1), str(qr, i2), str(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "string", "double", "string") => indexArray5[String, String, String, Double, String](numRows, str(qr, i1), str(qr, i2), str(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "string", "double", "double") => indexArray5[String, String, String, Double, Double](numRows, str(qr, i1), str(qr, i2), str(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "double", "string", "string") => indexArray5[String, String, Double, String, String](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "double", "string", "double") => indexArray5[String, String, Double, String, Double](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "double", "double", "string") => indexArray5[String, String, Double, Double, String](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "string", "double", "double", "double") => indexArray5[String, String, Double, Double, Double](numRows, str(qr, i1), str(qr, i2), dou(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "string", "string", "string") => indexArray5[String, Double, String, String, String](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "string", "string", "double") => indexArray5[String, Double, String, String, Double](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "string", "double", "string") => indexArray5[String, Double, String, Double, String](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "string", "double", "double") => indexArray5[String, Double, String, Double, Double](numRows, str(qr, i1), dou(qr, i2), str(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "double", "string", "string") => indexArray5[String, Double, Double, String, String](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "double", "string", "double") => indexArray5[String, Double, Double, String, Double](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "double", "double", "string") => indexArray5[String, Double, Double, Double, String](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("string", "double", "double", "double", "double") => indexArray5[String, Double, Double, Double, Double](numRows, str(qr, i1), dou(qr, i2), dou(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "string", "string", "string") => indexArray5[Double, String, String, String, String](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "string", "string", "double") => indexArray5[Double, String, String, String, Double](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "string", "double", "string") => indexArray5[Double, String, String, Double, String](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "string", "double", "double") => indexArray5[Double, String, String, Double, Double](numRows, dou(qr, i1), str(qr, i2), str(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "double", "string", "string") => indexArray5[Double, String, Double, String, String](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "double", "string", "double") => indexArray5[Double, String, Double, String, Double](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "double", "double", "string") => indexArray5[Double, String, Double, Double, String](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "string", "double", "double", "double") => indexArray5[Double, String, Double, Double, Double](numRows, dou(qr, i1), str(qr, i2), dou(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "string", "string", "string") => indexArray5[Double, Double, String, String, String](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "string", "string", "double") => indexArray5[Double, Double, String, String, Double](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "string", "double", "string") => indexArray5[Double, Double, String, Double, String](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "string", "double", "double") => indexArray5[Double, Double, String, Double, Double](numRows, dou(qr, i1), dou(qr, i2), str(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "double", "string", "string") => indexArray5[Double, Double, Double, String, String](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), str(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "double", "string", "double") => indexArray5[Double, Double, Double, String, Double](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), str(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "double", "double", "string") => indexArray5[Double, Double, Double, Double, String](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), dou(qr, i4), str(qr, i5), s1, s2, s3, s4, s5)
      case ("double", "double", "double", "double", "double") => indexArray5[Double, Double, Double, Double, Double](numRows, dou(qr, i1), dou(qr, i2), dou(qr, i3), dou(qr, i4), dou(qr, i5), s1, s2, s3, s4, s5)
      case (a, b, c, d, e)                                    => println(s"ILLEGAL column types: ($a, $b, $c, $d, $e)"); null
    }
  }

  private def indexArray5[T1, T2, T3, T4, T5](
    rowCount: Int,
    array1: Array[T1],
    array2: Array[T2],
    array3: Array[T3],
    array4: Array[T4],
    array5: Array[T5],
    sort1: String,
    sort2: String,
    sort3: String,
    sort4: String,
    sort5: String
  )(implicit
    ord1: Ordering[T1],
    ord2: Ordering[T2],
    ord3: Ordering[T3],
    ord4: Ordering[T4],
    ord5: Ordering[T5]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2, T3, T4, T5)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i), array3(i), array4(i), array5(i))
      i += 1
    }
    val ordering = Ordering.Tuple5(
      ord(ord1, sort1),
      ord(ord2, sort2),
      ord(ord3, sort3),
      ord(ord4, sort4),
      ord(ord5, sort5))
    indexArray.sortBy {
      case (_, v1, v2, v3, v4, v5) => (v1, v2, v3, v4, v5)
    }(ordering).map(_._1)
  }
}