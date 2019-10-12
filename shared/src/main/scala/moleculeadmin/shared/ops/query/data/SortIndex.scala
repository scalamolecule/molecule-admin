package moleculeadmin.shared.ops.query.data

import moleculeadmin.shared.ast.query.{Col, QueryResult}


trait SortIndex {

  def getSortIndex(qr: QueryResult, sortCols: Seq[Col], rowCount: Int): Array[Int] = {
    // println("SORTING...")
    sortCols.size match {
      case 1 => sortArray1(qr, sortCols)
      case 2 => sortArray2(qr, sortCols, rowCount)
      case 3 => sortArray3(qr, sortCols, rowCount)
      case 4 => sortArray4(qr, sortCols, rowCount)
      case 5 => sortArray5(qr, sortCols, rowCount)
      case n => throw new IllegalArgumentException(s"Can only sort by 1-5 columns. Found $n")
    }
  }


  private def ord[T](ordering: Ordering[T], sort: String): Ordering[T] = if (sort == "asc") ordering else ordering.reverse


  private def indexArray1[T](array1: Array[T], sort1: String)(implicit ord1: Ordering[T]): Array[Int] =
    array1.zipWithIndex.sortBy { case (v, i) => v }(ord(ord1, sort1)).map(_._2)


  private def indexArray2[T1, T2](rowCount: Int,
                                  array1: Array[T1], array2: Array[T2],
                                  sort1: String, sort2: String
                                 )(implicit ord1: Ordering[T1], ord2: Ordering[T2]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2)](rowCount)
    var i          = 0
    // println("sort numRows: " + rowCount)
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i))
      i += 1
    }
    val ordering = Ordering.Tuple2(ord(ord1, sort1), ord(ord2, sort2))
    indexArray.sortBy { case (_, v1, v2) => (v1, v2) }(ordering).map(_._1)
  }

  private def indexArray3[T1, T2, T3](rowCount: Int,
                                      array1: Array[T1], array2: Array[T2], array3: Array[T3],
                                      sort1: String, sort2: String, sort3: String
                                     )(implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2, T3)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i), array3(i))
      i += 1
    }
    val ordering = Ordering.Tuple3(ord(ord1, sort1), ord(ord2, sort2), ord(ord3, sort3))
    indexArray.sortBy { case (_, v1, v2, v3) => (v1, v2, v3) }(ordering).map(_._1)
  }

  private def indexArray4[T1, T2, T3, T4](rowCount: Int,
                                          array1: Array[T1], array2: Array[T2], array3: Array[T3], array4: Array[T4],
                                          sort1: String, sort2: String, sort3: String, sort4: String
                                         )(implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2, T3, T4)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i), array3(i), array4(i))
      i += 1
    }
    val ordering = Ordering.Tuple4(ord(ord1, sort1), ord(ord2, sort2), ord(ord3, sort3), ord(ord4, sort4))
    indexArray.sortBy { case (_, v1, v2, v3, v4) => (v1, v2, v3, v4) }(ordering).map(_._1)
  }

  private def indexArray5[T1, T2, T3, T4, T5](rowCount: Int,
                                              array1: Array[T1], array2: Array[T2], array3: Array[T3], array4: Array[T4], array5: Array[T5],
                                              sort1: String, sort2: String, sort3: String, sort4: String, sort5: String
                                             )(implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5]): Array[Int] = {
    val indexArray = new Array[(Int, T1, T2, T3, T4, T5)](rowCount)
    var i          = 0
    while (i < rowCount) {
      indexArray(i) = (i, array1(i), array2(i), array3(i), array4(i), array5(i))
      i += 1
    }
    val ordering = Ordering.Tuple5(ord(ord1, sort1), ord(ord2, sort2), ord(ord3, sort3), ord(ord4, sort4), ord(ord5, sort5))
    indexArray.sortBy { case (_, v1, v2, v3, v4, v5) => (v1, v2, v3, v4, v5) }(ordering).map(_._1)
  }


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
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, _) = sortCols.head

    val i1 = qr.arrayIndexes(colIndex1)
    tpe(t1, dt1, at1) match {
      case "string" => indexArray1[String](str(qr, i1), s1)
      case "double" => indexArray1[Double](dou(qr, i1), s1)
      case a        => println(s"ILLEGAL column type name: $a"); null
    }
  }

  private def sort2(qr: QueryResult, sortCols: Seq[Col], numRows: Int,
                    t1: String, t2: String,
                    dt1: String, dt2: String,
                    at1: String, at2: String,
                    i1: Int, i2: Int,
                    s1: String, s2: String
                   ): Array[Int] = {
    (tpe(t1, dt1, at1), tpe(t2, dt2, at2)) match {
      case ("string", "string") => indexArray2[String, String](numRows, str(qr, i1), str(qr, i2), s1, s2)
      case ("string", "double") => indexArray2[String, Double](numRows, str(qr, i1), dou(qr, i2), s1, s2)
      case ("double", "string") => indexArray2[Double, String](numRows, dou(qr, i1), str(qr, i2), s1, s2)
      case ("double", "double") => indexArray2[Double, Double](numRows, dou(qr, i1), dou(qr, i2), s1, s2)
      case (a, b)               => println(s"ILLEGAL column type names: ($a, $b)"); null
    }
  }
  private def sortArray2(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, p1) = sortCols.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, p2) = sortCols(1)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)

    // sort order
    (p1, p2) match {
      case (1, 2) => sort2(qr, sortCols, numRows, t1, t2, dt1, dt2, at1, at2, i1, i2, s1, s2)
      case (2, 1) => sort2(qr, sortCols, numRows, t2, t1, dt2, dt1, at2, at1, i2, i1, s2, s1)
      case (a, b) => println(s"ILLEGAL sort order positions: ($a, $b)"); null
    }
  }


  private def sort3(qr: QueryResult, sortCols: Seq[Col], numRows: Int,
                    t1: String, t2: String, t3: String,
                    dt1: String, dt2: String, dt3: String,
                    at1: String, at2: String, at3: String,
                    i1: Int, i2: Int, i3: Int,
                    s1: String, s2: String, s3: String,
                   ): Array[Int] = {
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
  private def sortArray3(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, p1) = sortCols.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, p2) = sortCols(1)
    val Col(colIndex3, _, _, _, _, t3, dt3, _, _, _, at3, _, s3, p3) = sortCols(2)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)
    val i3 = qr.arrayIndexes(colIndex3)

    (p1, p2, p3) match {
      case (1, 2, 3) => sort3(qr, sortCols, numRows, t1, t2, t3, dt1, dt2, dt3, at1, at2, at3, i1, i2, i3, s1, s2, s3)
      case (1, 3, 2) => sort3(qr, sortCols, numRows, t1, t3, t2, dt1, dt3, dt2, at1, at3, at2, i1, i3, i2, s1, s3, s2)
      case (2, 1, 3) => sort3(qr, sortCols, numRows, t2, t1, t3, dt2, dt1, dt3, at2, at1, at3, i2, i1, i3, s2, s1, s3)
      case (2, 3, 1) => sort3(qr, sortCols, numRows, t2, t3, t1, dt2, dt3, dt1, at2, at3, at1, i2, i3, i1, s2, s3, s1)
      case (3, 1, 2) => sort3(qr, sortCols, numRows, t3, t1, t2, dt3, dt1, dt2, at3, at1, at2, i3, i1, i2, s3, s1, s2)
      case (3, 2, 1) => sort3(qr, sortCols, numRows, t3, t2, t1, dt3, dt2, dt1, at3, at2, at1, i3, i2, i1, s3, s2, s1)
      case (a, b, c) => println(s"ILLEGAL sort order positions: ($a, $b, $c)"); null
    }
  }


  private def sort4(qr: QueryResult, sortCols: Seq[Col], numRows: Int,
                    t1: String, t2: String, t3: String, t4: String,
                    dt1: String, dt2: String, dt3: String, dt4: String,
                    at1: String, at2: String, at3: String, at4: String,
                    i1: Int, i2: Int, i3: Int, i4: Int,
                    s1: String, s2: String, s3: String, s4: String,
                   ): Array[Int] = {
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
  private def sortArray4(qr: QueryResult, sortCols: Seq[Col], numRows: Int): Array[Int] = {
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, p1) = sortCols.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, p2) = sortCols(1)
    val Col(colIndex3, _, _, _, _, t3, dt3, _, _, _, at3, _, s3, p3) = sortCols(2)
    val Col(colIndex4, _, _, _, _, t4, dt4, _, _, _, at4, _, s4, p4) = sortCols(3)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)
    val i3 = qr.arrayIndexes(colIndex3)
    val i4 = qr.arrayIndexes(colIndex4)

    (p1, p2, p3, p4) match {
      case (1, 2, 3, 4) => sort4(qr, sortCols, numRows, t1, t2, t3, t4, dt1, dt2, dt3, dt4, at1, at2, at3, at4, i1, i2, i3, i4, s1, s2, s3, s4)
      case (1, 2, 4, 3) => sort4(qr, sortCols, numRows, t1, t2, t4, t3, dt1, dt2, dt4, dt3, at1, at2, at4, at3, i1, i2, i4, i3, s1, s2, s4, s3)
      case (1, 3, 2, 4) => sort4(qr, sortCols, numRows, t1, t3, t2, t4, dt1, dt3, dt2, dt4, at1, at3, at2, at4, i1, i3, i2, i4, s1, s3, s2, s4)
      case (1, 3, 4, 2) => sort4(qr, sortCols, numRows, t1, t3, t4, t2, dt1, dt3, dt4, dt2, at1, at3, at4, at2, i1, i3, i4, i2, s1, s3, s4, s2)
      case (1, 4, 2, 3) => sort4(qr, sortCols, numRows, t1, t4, t2, t3, dt1, dt4, dt2, dt3, at1, at4, at2, at3, i1, i4, i2, i3, s1, s4, s2, s3)
      case (1, 4, 3, 2) => sort4(qr, sortCols, numRows, t1, t4, t3, t2, dt1, dt4, dt3, dt2, at1, at4, at3, at2, i1, i4, i3, i2, s1, s4, s3, s2)
      case (2, 1, 3, 4) => sort4(qr, sortCols, numRows, t2, t1, t3, t4, dt2, dt1, dt3, dt4, at2, at1, at3, at4, i2, i1, i3, i4, s2, s1, s3, s4)
      case (2, 1, 4, 3) => sort4(qr, sortCols, numRows, t2, t1, t4, t3, dt2, dt1, dt4, dt3, at2, at1, at4, at3, i2, i1, i4, i3, s2, s1, s4, s3)
      case (2, 3, 1, 4) => sort4(qr, sortCols, numRows, t2, t3, t1, t4, dt2, dt3, dt1, dt4, at2, at3, at1, at4, i2, i3, i1, i4, s2, s3, s1, s4)
      case (2, 3, 4, 1) => sort4(qr, sortCols, numRows, t2, t3, t4, t1, dt2, dt3, dt4, dt1, at2, at3, at4, at1, i2, i3, i4, i1, s2, s3, s4, s1)
      case (2, 4, 1, 3) => sort4(qr, sortCols, numRows, t2, t4, t1, t3, dt2, dt4, dt1, dt3, at2, at4, at1, at3, i2, i4, i1, i3, s2, s4, s1, s3)
      case (2, 4, 3, 1) => sort4(qr, sortCols, numRows, t2, t4, t3, t1, dt2, dt4, dt3, dt1, at2, at4, at3, at1, i2, i4, i3, i1, s2, s4, s3, s1)
      case (3, 1, 2, 4) => sort4(qr, sortCols, numRows, t3, t1, t2, t4, dt3, dt1, dt2, dt4, at3, at1, at2, at4, i3, i1, i2, i4, s3, s1, s2, s4)
      case (3, 1, 4, 2) => sort4(qr, sortCols, numRows, t3, t1, t4, t2, dt3, dt1, dt4, dt2, at3, at1, at4, at2, i3, i1, i4, i2, s3, s1, s4, s2)
      case (3, 2, 1, 4) => sort4(qr, sortCols, numRows, t3, t2, t1, t4, dt3, dt2, dt1, dt4, at3, at2, at1, at4, i3, i2, i1, i4, s3, s2, s1, s4)
      case (3, 2, 4, 1) => sort4(qr, sortCols, numRows, t3, t2, t4, t1, dt3, dt2, dt4, dt1, at3, at2, at4, at1, i3, i2, i4, i1, s3, s2, s4, s1)
      case (3, 4, 1, 2) => sort4(qr, sortCols, numRows, t3, t4, t1, t2, dt3, dt4, dt1, dt2, at3, at4, at1, at2, i3, i4, i1, i2, s3, s4, s1, s2)
      case (3, 4, 2, 1) => sort4(qr, sortCols, numRows, t3, t4, t2, t1, dt3, dt4, dt2, dt1, at3, at4, at2, at1, i3, i4, i2, i1, s3, s4, s2, s1)
      case (4, 1, 2, 3) => sort4(qr, sortCols, numRows, t4, t1, t2, t3, dt4, dt1, dt2, dt3, at4, at1, at2, at3, i4, i1, i2, i3, s4, s1, s2, s3)
      case (4, 1, 3, 2) => sort4(qr, sortCols, numRows, t4, t1, t3, t2, dt4, dt1, dt3, dt2, at4, at1, at3, at2, i4, i1, i3, i2, s4, s1, s3, s2)
      case (4, 2, 1, 3) => sort4(qr, sortCols, numRows, t4, t2, t1, t3, dt4, dt2, dt1, dt3, at4, at2, at1, at3, i4, i2, i1, i3, s4, s2, s1, s3)
      case (4, 2, 3, 1) => sort4(qr, sortCols, numRows, t4, t2, t3, t1, dt4, dt2, dt3, dt1, at4, at2, at3, at1, i4, i2, i3, i1, s4, s2, s3, s1)
      case (4, 3, 1, 2) => sort4(qr, sortCols, numRows, t4, t3, t1, t2, dt4, dt3, dt1, dt2, at4, at3, at1, at2, i4, i3, i1, i2, s4, s3, s1, s2)
      case (4, 3, 2, 1) => sort4(qr, sortCols, numRows, t4, t3, t2, t1, dt4, dt3, dt2, dt1, at4, at3, at2, at1, i4, i3, i2, i1, s4, s3, s2, s1)
      case (a, b, c, d) => println(s"ILLEGAL sort order positions: ($a, $b, $c, $d)"); null
    }
  }


  private def sort5(qr: QueryResult, sortCols: Seq[Col], numRows: Int,
                    t1: String, t2: String, t3: String, t4: String, t5: String,
                    dt1: String, dt2: String, dt3: String, dt4: String, dt5: String,
                    at1: String, at2: String, at3: String, at4: String, at5: String,
                    i1: Int, i2: Int, i3: Int, i4: Int, i5: Int,
                    s1: String, s2: String, s3: String, s4: String, s5: String
                   ): Array[Int] = {
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
  private def sortArray5(qr: QueryResult, sortCols: Seq[Col], numRows: Int) = {
    val Col(colIndex1, _, _, _, _, t1, dt1, _, _, _, at1, _, s1, p1) = sortCols.head
    val Col(colIndex2, _, _, _, _, t2, dt2, _, _, _, at2, _, s2, p2) = sortCols(1)
    val Col(colIndex3, _, _, _, _, t3, dt3, _, _, _, at3, _, s3, p3) = sortCols(2)
    val Col(colIndex4, _, _, _, _, t4, dt4, _, _, _, at4, _, s4, p4) = sortCols(3)
    val Col(colIndex5, _, _, _, _, t5, dt5, _, _, _, at5, _, s5, p5) = sortCols(4)

    val i1 = qr.arrayIndexes(colIndex1)
    val i2 = qr.arrayIndexes(colIndex2)
    val i3 = qr.arrayIndexes(colIndex3)
    val i4 = qr.arrayIndexes(colIndex4)
    val i5 = qr.arrayIndexes(colIndex5)

    p1 match {
      case 1 => (p2, p3, p4, p5) match {
        case (2, 3, 4, 5) => sort5(qr, sortCols, numRows, t1, t2, t3, t4, t5, dt1, dt2, dt3, dt4, dt5, at1, at2, at3, at4, at5, i1, i2, i3, i4, i5, s1, s2, s3, s4, s5)
        case (2, 3, 5, 4) => sort5(qr, sortCols, numRows, t1, t2, t3, t5, t4, dt1, dt2, dt3, dt5, dt4, at1, at2, at3, at5, at4, i1, i2, i3, i5, i4, s1, s2, s3, s5, s4)
        case (2, 4, 3, 5) => sort5(qr, sortCols, numRows, t1, t2, t4, t3, t5, dt1, dt2, dt4, dt3, dt5, at1, at2, at4, at3, at5, i1, i2, i4, i3, i5, s1, s2, s4, s3, s5)
        case (2, 4, 5, 3) => sort5(qr, sortCols, numRows, t1, t2, t4, t5, t3, dt1, dt2, dt4, dt5, dt3, at1, at2, at4, at5, at3, i1, i2, i4, i5, i3, s1, s2, s4, s5, s3)
        case (2, 5, 3, 4) => sort5(qr, sortCols, numRows, t1, t2, t5, t3, t4, dt1, dt2, dt5, dt3, dt4, at1, at2, at5, at3, at4, i1, i2, i5, i3, i4, s1, s2, s5, s3, s4)
        case (2, 5, 4, 3) => sort5(qr, sortCols, numRows, t1, t2, t5, t4, t3, dt1, dt2, dt5, dt4, dt3, at1, at2, at5, at4, at3, i1, i2, i5, i4, i3, s1, s2, s5, s4, s3)
        case (3, 2, 4, 5) => sort5(qr, sortCols, numRows, t1, t3, t2, t4, t5, dt1, dt3, dt2, dt4, dt5, at1, at3, at2, at4, at5, i1, i3, i2, i4, i5, s1, s3, s2, s4, s5)
        case (3, 2, 5, 4) => sort5(qr, sortCols, numRows, t1, t3, t2, t5, t4, dt1, dt3, dt2, dt5, dt4, at1, at3, at2, at5, at4, i1, i3, i2, i5, i4, s1, s3, s2, s5, s4)
        case (3, 4, 2, 5) => sort5(qr, sortCols, numRows, t1, t3, t4, t2, t5, dt1, dt3, dt4, dt2, dt5, at1, at3, at4, at2, at5, i1, i3, i4, i2, i5, s1, s3, s4, s2, s5)
        case (3, 4, 5, 2) => sort5(qr, sortCols, numRows, t1, t3, t4, t5, t2, dt1, dt3, dt4, dt5, dt2, at1, at3, at4, at5, at2, i1, i3, i4, i5, i2, s1, s3, s4, s5, s2)
        case (3, 5, 2, 4) => sort5(qr, sortCols, numRows, t1, t3, t5, t2, t4, dt1, dt3, dt5, dt2, dt4, at1, at3, at5, at2, at4, i1, i3, i5, i2, i4, s1, s3, s5, s2, s4)
        case (3, 5, 4, 2) => sort5(qr, sortCols, numRows, t1, t3, t5, t4, t2, dt1, dt3, dt5, dt4, dt2, at1, at3, at5, at4, at2, i1, i3, i5, i4, i2, s1, s3, s5, s4, s2)
        case (4, 2, 3, 5) => sort5(qr, sortCols, numRows, t1, t4, t2, t3, t5, dt1, dt4, dt2, dt3, dt5, at1, at4, at2, at3, at5, i1, i4, i2, i3, i5, s1, s4, s2, s3, s5)
        case (4, 2, 5, 3) => sort5(qr, sortCols, numRows, t1, t4, t2, t5, t3, dt1, dt4, dt2, dt5, dt3, at1, at4, at2, at5, at3, i1, i4, i2, i5, i3, s1, s4, s2, s5, s3)
        case (4, 3, 2, 5) => sort5(qr, sortCols, numRows, t1, t4, t3, t2, t5, dt1, dt4, dt3, dt2, dt5, at1, at4, at3, at2, at5, i1, i4, i3, i2, i5, s1, s4, s3, s2, s5)
        case (4, 3, 5, 2) => sort5(qr, sortCols, numRows, t1, t4, t3, t5, t2, dt1, dt4, dt3, dt5, dt2, at1, at4, at3, at5, at2, i1, i4, i3, i5, i2, s1, s4, s3, s5, s2)
        case (4, 5, 2, 3) => sort5(qr, sortCols, numRows, t1, t4, t5, t2, t3, dt1, dt4, dt5, dt2, dt3, at1, at4, at5, at2, at3, i1, i4, i5, i2, i3, s1, s4, s5, s2, s3)
        case (4, 5, 3, 2) => sort5(qr, sortCols, numRows, t1, t4, t5, t3, t2, dt1, dt4, dt5, dt3, dt2, at1, at4, at5, at3, at2, i1, i4, i5, i3, i2, s1, s4, s5, s3, s2)
        case (5, 2, 3, 4) => sort5(qr, sortCols, numRows, t1, t5, t2, t3, t4, dt1, dt5, dt2, dt3, dt4, at1, at5, at2, at3, at4, i1, i5, i2, i3, i4, s1, s5, s2, s3, s4)
        case (5, 2, 4, 3) => sort5(qr, sortCols, numRows, t1, t5, t2, t4, t3, dt1, dt5, dt2, dt4, dt3, at1, at5, at2, at4, at3, i1, i5, i2, i4, i3, s1, s5, s2, s4, s3)
        case (5, 3, 2, 4) => sort5(qr, sortCols, numRows, t1, t5, t3, t2, t4, dt1, dt5, dt3, dt2, dt4, at1, at5, at3, at2, at4, i1, i5, i3, i2, i4, s1, s5, s3, s2, s4)
        case (5, 3, 4, 2) => sort5(qr, sortCols, numRows, t1, t5, t3, t4, t2, dt1, dt5, dt3, dt4, dt2, at1, at5, at3, at4, at2, i1, i5, i3, i4, i2, s1, s5, s3, s4, s2)
        case (5, 4, 2, 3) => sort5(qr, sortCols, numRows, t1, t5, t4, t2, t3, dt1, dt5, dt4, dt2, dt3, at1, at5, at4, at2, at3, i1, i5, i4, i2, i3, s1, s5, s4, s2, s3)
        case (5, 4, 3, 2) => sort5(qr, sortCols, numRows, t1, t5, t4, t3, t2, dt1, dt5, dt4, dt3, dt2, at1, at5, at4, at3, at2, i1, i5, i4, i3, i2, s1, s5, s4, s3, s2)
        case (a, b, c, d) => println(s"ILLEGAL sort order positions: ($a, $b, $c, $d)"); null
      }
      case 2 => (p2, p3, p4, p5) match {
        case (1, 3, 4, 5) => sort5(qr, sortCols, numRows, t2, t1, t3, t4, t5, dt2, dt1, dt3, dt4, dt5, at2, at1, at3, at4, at5, i2, i1, i3, i4, i5, s2, s1, s3, s4, s5)
        case (1, 3, 5, 4) => sort5(qr, sortCols, numRows, t2, t1, t3, t5, t4, dt2, dt1, dt3, dt5, dt4, at2, at1, at3, at5, at4, i2, i1, i3, i5, i4, s2, s1, s3, s5, s4)
        case (1, 4, 3, 5) => sort5(qr, sortCols, numRows, t2, t1, t4, t3, t5, dt2, dt1, dt4, dt3, dt5, at2, at1, at4, at3, at5, i2, i1, i4, i3, i5, s2, s1, s4, s3, s5)
        case (1, 4, 5, 3) => sort5(qr, sortCols, numRows, t2, t1, t4, t5, t3, dt2, dt1, dt4, dt5, dt3, at2, at1, at4, at5, at3, i2, i1, i4, i5, i3, s2, s1, s4, s5, s3)
        case (1, 5, 3, 4) => sort5(qr, sortCols, numRows, t2, t1, t5, t3, t4, dt2, dt1, dt5, dt3, dt4, at2, at1, at5, at3, at4, i2, i1, i5, i3, i4, s2, s1, s5, s3, s4)
        case (1, 5, 4, 3) => sort5(qr, sortCols, numRows, t2, t1, t5, t4, t3, dt2, dt1, dt5, dt4, dt3, at2, at1, at5, at4, at3, i2, i1, i5, i4, i3, s2, s1, s5, s4, s3)
        case (3, 1, 4, 5) => sort5(qr, sortCols, numRows, t2, t3, t1, t4, t5, dt2, dt3, dt1, dt4, dt5, at2, at3, at1, at4, at5, i2, i3, i1, i4, i5, s2, s3, s1, s4, s5)
        case (3, 1, 5, 4) => sort5(qr, sortCols, numRows, t2, t3, t1, t5, t4, dt2, dt3, dt1, dt5, dt4, at2, at3, at1, at5, at4, i2, i3, i1, i5, i4, s2, s3, s1, s5, s4)
        case (3, 4, 1, 5) => sort5(qr, sortCols, numRows, t2, t3, t4, t1, t5, dt2, dt3, dt4, dt1, dt5, at2, at3, at4, at1, at5, i2, i3, i4, i1, i5, s2, s3, s4, s1, s5)
        case (3, 4, 5, 1) => sort5(qr, sortCols, numRows, t2, t3, t4, t5, t1, dt2, dt3, dt4, dt5, dt1, at2, at3, at4, at5, at1, i2, i3, i4, i5, i1, s2, s3, s4, s5, s1)
        case (3, 5, 1, 4) => sort5(qr, sortCols, numRows, t2, t3, t5, t1, t4, dt2, dt3, dt5, dt1, dt4, at2, at3, at5, at1, at4, i2, i3, i5, i1, i4, s2, s3, s5, s1, s4)
        case (3, 5, 4, 1) => sort5(qr, sortCols, numRows, t2, t3, t5, t4, t1, dt2, dt3, dt5, dt4, dt1, at2, at3, at5, at4, at1, i2, i3, i5, i4, i1, s2, s3, s5, s4, s1)
        case (4, 1, 3, 5) => sort5(qr, sortCols, numRows, t2, t4, t1, t3, t5, dt2, dt4, dt1, dt3, dt5, at2, at4, at1, at3, at5, i2, i4, i1, i3, i5, s2, s4, s1, s3, s5)
        case (4, 1, 5, 3) => sort5(qr, sortCols, numRows, t2, t4, t1, t5, t3, dt2, dt4, dt1, dt5, dt3, at2, at4, at1, at5, at3, i2, i4, i1, i5, i3, s2, s4, s1, s5, s3)
        case (4, 3, 1, 5) => sort5(qr, sortCols, numRows, t2, t4, t3, t1, t5, dt2, dt4, dt3, dt1, dt5, at2, at4, at3, at1, at5, i2, i4, i3, i1, i5, s2, s4, s3, s1, s5)
        case (4, 3, 5, 1) => sort5(qr, sortCols, numRows, t2, t4, t3, t5, t1, dt2, dt4, dt3, dt5, dt1, at2, at4, at3, at5, at1, i2, i4, i3, i5, i1, s2, s4, s3, s5, s1)
        case (4, 5, 1, 3) => sort5(qr, sortCols, numRows, t2, t4, t5, t1, t3, dt2, dt4, dt5, dt1, dt3, at2, at4, at5, at1, at3, i2, i4, i5, i1, i3, s2, s4, s5, s1, s3)
        case (4, 5, 3, 1) => sort5(qr, sortCols, numRows, t2, t4, t5, t3, t1, dt2, dt4, dt5, dt3, dt1, at2, at4, at5, at3, at1, i2, i4, i5, i3, i1, s2, s4, s5, s3, s1)
        case (5, 1, 3, 4) => sort5(qr, sortCols, numRows, t2, t5, t1, t3, t4, dt2, dt5, dt1, dt3, dt4, at2, at5, at1, at3, at4, i2, i5, i1, i3, i4, s2, s5, s1, s3, s4)
        case (5, 1, 4, 3) => sort5(qr, sortCols, numRows, t2, t5, t1, t4, t3, dt2, dt5, dt1, dt4, dt3, at2, at5, at1, at4, at3, i2, i5, i1, i4, i3, s2, s5, s1, s4, s3)
        case (5, 3, 1, 4) => sort5(qr, sortCols, numRows, t2, t5, t3, t1, t4, dt2, dt5, dt3, dt1, dt4, at2, at5, at3, at1, at4, i2, i5, i3, i1, i4, s2, s5, s3, s1, s4)
        case (5, 3, 4, 1) => sort5(qr, sortCols, numRows, t2, t5, t3, t4, t1, dt2, dt5, dt3, dt4, dt1, at2, at5, at3, at4, at1, i2, i5, i3, i4, i1, s2, s5, s3, s4, s1)
        case (5, 4, 1, 3) => sort5(qr, sortCols, numRows, t2, t5, t4, t1, t3, dt2, dt5, dt4, dt1, dt3, at2, at5, at4, at1, at3, i2, i5, i4, i1, i3, s2, s5, s4, s1, s3)
        case (5, 4, 3, 1) => sort5(qr, sortCols, numRows, t2, t5, t4, t3, t1, dt2, dt5, dt4, dt3, dt1, at2, at5, at4, at3, at1, i2, i5, i4, i3, i1, s2, s5, s4, s3, s1)
        case (a, b, c, d) => println(s"ILLEGAL sort order positions: ($a, $b, $c, $d)"); null
      }
      case 3 => (p2, p3, p4, p5) match {
        case (1, 2, 4, 5) => sort5(qr, sortCols, numRows, t3, t1, t2, t4, t5, dt3, dt1, dt2, dt4, dt5, at3, at1, at2, at4, at5, i3, i1, i2, i4, i5, s3, s1, s2, s4, s5)
        case (1, 2, 5, 4) => sort5(qr, sortCols, numRows, t3, t1, t2, t5, t4, dt3, dt1, dt2, dt5, dt4, at3, at1, at2, at5, at4, i3, i1, i2, i5, i4, s3, s1, s2, s5, s4)
        case (1, 4, 2, 5) => sort5(qr, sortCols, numRows, t3, t1, t4, t2, t5, dt3, dt1, dt4, dt2, dt5, at3, at1, at4, at2, at5, i3, i1, i4, i2, i5, s3, s1, s4, s2, s5)
        case (1, 4, 5, 2) => sort5(qr, sortCols, numRows, t3, t1, t4, t5, t2, dt3, dt1, dt4, dt5, dt2, at3, at1, at4, at5, at2, i3, i1, i4, i5, i2, s3, s1, s4, s5, s2)
        case (1, 5, 2, 4) => sort5(qr, sortCols, numRows, t3, t1, t5, t2, t4, dt3, dt1, dt5, dt2, dt4, at3, at1, at5, at2, at4, i3, i1, i5, i2, i4, s3, s1, s5, s2, s4)
        case (1, 5, 4, 2) => sort5(qr, sortCols, numRows, t3, t1, t5, t4, t2, dt3, dt1, dt5, dt4, dt2, at3, at1, at5, at4, at2, i3, i1, i5, i4, i2, s3, s1, s5, s4, s2)
        case (2, 1, 4, 5) => sort5(qr, sortCols, numRows, t3, t2, t1, t4, t5, dt3, dt2, dt1, dt4, dt5, at3, at2, at1, at4, at5, i3, i2, i1, i4, i5, s3, s2, s1, s4, s5)
        case (2, 1, 5, 4) => sort5(qr, sortCols, numRows, t3, t2, t1, t5, t4, dt3, dt2, dt1, dt5, dt4, at3, at2, at1, at5, at4, i3, i2, i1, i5, i4, s3, s2, s1, s5, s4)
        case (2, 4, 1, 5) => sort5(qr, sortCols, numRows, t3, t2, t4, t1, t5, dt3, dt2, dt4, dt1, dt5, at3, at2, at4, at1, at5, i3, i2, i4, i1, i5, s3, s2, s4, s1, s5)
        case (2, 4, 5, 1) => sort5(qr, sortCols, numRows, t3, t2, t4, t5, t1, dt3, dt2, dt4, dt5, dt1, at3, at2, at4, at5, at1, i3, i2, i4, i5, i1, s3, s2, s4, s5, s1)
        case (2, 5, 1, 4) => sort5(qr, sortCols, numRows, t3, t2, t5, t1, t4, dt3, dt2, dt5, dt1, dt4, at3, at2, at5, at1, at4, i3, i2, i5, i1, i4, s3, s2, s5, s1, s4)
        case (2, 5, 4, 1) => sort5(qr, sortCols, numRows, t3, t2, t5, t4, t1, dt3, dt2, dt5, dt4, dt1, at3, at2, at5, at4, at1, i3, i2, i5, i4, i1, s3, s2, s5, s4, s1)
        case (4, 1, 2, 5) => sort5(qr, sortCols, numRows, t3, t4, t1, t2, t5, dt3, dt4, dt1, dt2, dt5, at3, at4, at1, at2, at5, i3, i4, i1, i2, i5, s3, s4, s1, s2, s5)
        case (4, 1, 5, 2) => sort5(qr, sortCols, numRows, t3, t4, t1, t5, t2, dt3, dt4, dt1, dt5, dt2, at3, at4, at1, at5, at2, i3, i4, i1, i5, i2, s3, s4, s1, s5, s2)
        case (4, 2, 1, 5) => sort5(qr, sortCols, numRows, t3, t4, t2, t1, t5, dt3, dt4, dt2, dt1, dt5, at3, at4, at2, at1, at5, i3, i4, i2, i1, i5, s3, s4, s2, s1, s5)
        case (4, 2, 5, 1) => sort5(qr, sortCols, numRows, t3, t4, t2, t5, t1, dt3, dt4, dt2, dt5, dt1, at3, at4, at2, at5, at1, i3, i4, i2, i5, i1, s3, s4, s2, s5, s1)
        case (4, 5, 1, 2) => sort5(qr, sortCols, numRows, t3, t4, t5, t1, t2, dt3, dt4, dt5, dt1, dt2, at3, at4, at5, at1, at2, i3, i4, i5, i1, i2, s3, s4, s5, s1, s2)
        case (4, 5, 2, 1) => sort5(qr, sortCols, numRows, t3, t4, t5, t2, t1, dt3, dt4, dt5, dt2, dt1, at3, at4, at5, at2, at1, i3, i4, i5, i2, i1, s3, s4, s5, s2, s1)
        case (5, 1, 2, 4) => sort5(qr, sortCols, numRows, t3, t5, t1, t2, t4, dt3, dt5, dt1, dt2, dt4, at3, at5, at1, at2, at4, i3, i5, i1, i2, i4, s3, s5, s1, s2, s4)
        case (5, 1, 4, 2) => sort5(qr, sortCols, numRows, t3, t5, t1, t4, t2, dt3, dt5, dt1, dt4, dt2, at3, at5, at1, at4, at2, i3, i5, i1, i4, i2, s3, s5, s1, s4, s2)
        case (5, 2, 1, 4) => sort5(qr, sortCols, numRows, t3, t5, t2, t1, t4, dt3, dt5, dt2, dt1, dt4, at3, at5, at2, at1, at4, i3, i5, i2, i1, i4, s3, s5, s2, s1, s4)
        case (5, 2, 4, 1) => sort5(qr, sortCols, numRows, t3, t5, t2, t4, t1, dt3, dt5, dt2, dt4, dt1, at3, at5, at2, at4, at1, i3, i5, i2, i4, i1, s3, s5, s2, s4, s1)
        case (5, 4, 1, 2) => sort5(qr, sortCols, numRows, t3, t5, t4, t1, t2, dt3, dt5, dt4, dt1, dt2, at3, at5, at4, at1, at2, i3, i5, i4, i1, i2, s3, s5, s4, s1, s2)
        case (5, 4, 2, 1) => sort5(qr, sortCols, numRows, t3, t5, t4, t2, t1, dt3, dt5, dt4, dt2, dt1, at3, at5, at4, at2, at1, i3, i5, i4, i2, i1, s3, s5, s4, s2, s1)
        case (a, b, c, d) => println(s"ILLEGAL sort order positions: ($a, $b, $c, $d)"); null
      }
      case 4 => (p2, p3, p4, p5) match {
        case (1, 2, 3, 5) => sort5(qr, sortCols, numRows, t4, t1, t2, t3, t5, dt4, dt1, dt2, dt3, dt5, at4, at1, at2, at3, at5, i4, i1, i2, i3, i5, s4, s1, s2, s3, s5)
        case (1, 2, 5, 3) => sort5(qr, sortCols, numRows, t4, t1, t2, t5, t3, dt4, dt1, dt2, dt5, dt3, at4, at1, at2, at5, at3, i4, i1, i2, i5, i3, s4, s1, s2, s5, s3)
        case (1, 3, 2, 5) => sort5(qr, sortCols, numRows, t4, t1, t3, t2, t5, dt4, dt1, dt3, dt2, dt5, at4, at1, at3, at2, at5, i4, i1, i3, i2, i5, s4, s1, s3, s2, s5)
        case (1, 3, 5, 2) => sort5(qr, sortCols, numRows, t4, t1, t3, t5, t2, dt4, dt1, dt3, dt5, dt2, at4, at1, at3, at5, at2, i4, i1, i3, i5, i2, s4, s1, s3, s5, s2)
        case (1, 5, 2, 3) => sort5(qr, sortCols, numRows, t4, t1, t5, t2, t3, dt4, dt1, dt5, dt2, dt3, at4, at1, at5, at2, at3, i4, i1, i5, i2, i3, s4, s1, s5, s2, s3)
        case (1, 5, 3, 2) => sort5(qr, sortCols, numRows, t4, t1, t5, t3, t2, dt4, dt1, dt5, dt3, dt2, at4, at1, at5, at3, at2, i4, i1, i5, i3, i2, s4, s1, s5, s3, s2)
        case (2, 1, 3, 5) => sort5(qr, sortCols, numRows, t4, t2, t1, t3, t5, dt4, dt2, dt1, dt3, dt5, at4, at2, at1, at3, at5, i4, i2, i1, i3, i5, s4, s2, s1, s3, s5)
        case (2, 1, 5, 3) => sort5(qr, sortCols, numRows, t4, t2, t1, t5, t3, dt4, dt2, dt1, dt5, dt3, at4, at2, at1, at5, at3, i4, i2, i1, i5, i3, s4, s2, s1, s5, s3)
        case (2, 3, 1, 5) => sort5(qr, sortCols, numRows, t4, t2, t3, t1, t5, dt4, dt2, dt3, dt1, dt5, at4, at2, at3, at1, at5, i4, i2, i3, i1, i5, s4, s2, s3, s1, s5)
        case (2, 3, 5, 1) => sort5(qr, sortCols, numRows, t4, t2, t3, t5, t1, dt4, dt2, dt3, dt5, dt1, at4, at2, at3, at5, at1, i4, i2, i3, i5, i1, s4, s2, s3, s5, s1)
        case (2, 5, 1, 3) => sort5(qr, sortCols, numRows, t4, t2, t5, t1, t3, dt4, dt2, dt5, dt1, dt3, at4, at2, at5, at1, at3, i4, i2, i5, i1, i3, s4, s2, s5, s1, s3)
        case (2, 5, 3, 1) => sort5(qr, sortCols, numRows, t4, t2, t5, t3, t1, dt4, dt2, dt5, dt3, dt1, at4, at2, at5, at3, at1, i4, i2, i5, i3, i1, s4, s2, s5, s3, s1)
        case (3, 1, 2, 5) => sort5(qr, sortCols, numRows, t4, t3, t1, t2, t5, dt4, dt3, dt1, dt2, dt5, at4, at3, at1, at2, at5, i4, i3, i1, i2, i5, s4, s3, s1, s2, s5)
        case (3, 1, 5, 2) => sort5(qr, sortCols, numRows, t4, t3, t1, t5, t2, dt4, dt3, dt1, dt5, dt2, at4, at3, at1, at5, at2, i4, i3, i1, i5, i2, s4, s3, s1, s5, s2)
        case (3, 2, 1, 5) => sort5(qr, sortCols, numRows, t4, t3, t2, t1, t5, dt4, dt3, dt2, dt1, dt5, at4, at3, at2, at1, at5, i4, i3, i2, i1, i5, s4, s3, s2, s1, s5)
        case (3, 2, 5, 1) => sort5(qr, sortCols, numRows, t4, t3, t2, t5, t1, dt4, dt3, dt2, dt5, dt1, at4, at3, at2, at5, at1, i4, i3, i2, i5, i1, s4, s3, s2, s5, s1)
        case (3, 5, 1, 2) => sort5(qr, sortCols, numRows, t4, t3, t5, t1, t2, dt4, dt3, dt5, dt1, dt2, at4, at3, at5, at1, at2, i4, i3, i5, i1, i2, s4, s3, s5, s1, s2)
        case (3, 5, 2, 1) => sort5(qr, sortCols, numRows, t4, t3, t5, t2, t1, dt4, dt3, dt5, dt2, dt1, at4, at3, at5, at2, at1, i4, i3, i5, i2, i1, s4, s3, s5, s2, s1)
        case (5, 1, 2, 3) => sort5(qr, sortCols, numRows, t4, t5, t1, t2, t3, dt4, dt5, dt1, dt2, dt3, at4, at5, at1, at2, at3, i4, i5, i1, i2, i3, s4, s5, s1, s2, s3)
        case (5, 1, 3, 2) => sort5(qr, sortCols, numRows, t4, t5, t1, t3, t2, dt4, dt5, dt1, dt3, dt2, at4, at5, at1, at3, at2, i4, i5, i1, i3, i2, s4, s5, s1, s3, s2)
        case (5, 2, 1, 3) => sort5(qr, sortCols, numRows, t4, t5, t2, t1, t3, dt4, dt5, dt2, dt1, dt3, at4, at5, at2, at1, at3, i4, i5, i2, i1, i3, s4, s5, s2, s1, s3)
        case (5, 2, 3, 1) => sort5(qr, sortCols, numRows, t4, t5, t2, t3, t1, dt4, dt5, dt2, dt3, dt1, at4, at5, at2, at3, at1, i4, i5, i2, i3, i1, s4, s5, s2, s3, s1)
        case (5, 3, 1, 2) => sort5(qr, sortCols, numRows, t4, t5, t3, t1, t2, dt4, dt5, dt3, dt1, dt2, at4, at5, at3, at1, at2, i4, i5, i3, i1, i2, s4, s5, s3, s1, s2)
        case (5, 3, 2, 1) => sort5(qr, sortCols, numRows, t4, t5, t3, t2, t1, dt4, dt5, dt3, dt2, dt1, at4, at5, at3, at2, at1, i4, i5, i3, i2, i1, s4, s5, s3, s2, s1)
        case (a, b, c, d) => println(s"ILLEGAL sort order positions: ($a, $b, $c, $d)"); null
      }
      case 5 => (p2, p3, p4, p5) match {
        case (1, 2, 3, 4) => sort5(qr, sortCols, numRows, t5, t1, t2, t3, t4, dt5, dt1, dt2, dt3, dt4, at5, at1, at2, at3, at4, i5, i1, i2, i3, i4, s5, s1, s2, s3, s4)
        case (1, 2, 4, 3) => sort5(qr, sortCols, numRows, t5, t1, t2, t4, t3, dt5, dt1, dt2, dt4, dt3, at5, at1, at2, at4, at3, i5, i1, i2, i4, i3, s5, s1, s2, s4, s3)
        case (1, 3, 2, 4) => sort5(qr, sortCols, numRows, t5, t1, t3, t2, t4, dt5, dt1, dt3, dt2, dt4, at5, at1, at3, at2, at4, i5, i1, i3, i2, i4, s5, s1, s3, s2, s4)
        case (1, 3, 4, 2) => sort5(qr, sortCols, numRows, t5, t1, t3, t4, t2, dt5, dt1, dt3, dt4, dt2, at5, at1, at3, at4, at2, i5, i1, i3, i4, i2, s5, s1, s3, s4, s2)
        case (1, 4, 2, 3) => sort5(qr, sortCols, numRows, t5, t1, t4, t2, t3, dt5, dt1, dt4, dt2, dt3, at5, at1, at4, at2, at3, i5, i1, i4, i2, i3, s5, s1, s4, s2, s3)
        case (1, 4, 3, 2) => sort5(qr, sortCols, numRows, t5, t1, t4, t3, t2, dt5, dt1, dt4, dt3, dt2, at5, at1, at4, at3, at2, i5, i1, i4, i3, i2, s5, s1, s4, s3, s2)
        case (2, 1, 3, 4) => sort5(qr, sortCols, numRows, t5, t2, t1, t3, t4, dt5, dt2, dt1, dt3, dt4, at5, at2, at1, at3, at4, i5, i2, i1, i3, i4, s5, s2, s1, s3, s4)
        case (2, 1, 4, 3) => sort5(qr, sortCols, numRows, t5, t2, t1, t4, t3, dt5, dt2, dt1, dt4, dt3, at5, at2, at1, at4, at3, i5, i2, i1, i4, i3, s5, s2, s1, s4, s3)
        case (2, 3, 1, 4) => sort5(qr, sortCols, numRows, t5, t2, t3, t1, t4, dt5, dt2, dt3, dt1, dt4, at5, at2, at3, at1, at4, i5, i2, i3, i1, i4, s5, s2, s3, s1, s4)
        case (2, 3, 4, 1) => sort5(qr, sortCols, numRows, t5, t2, t3, t4, t1, dt5, dt2, dt3, dt4, dt1, at5, at2, at3, at4, at1, i5, i2, i3, i4, i1, s5, s2, s3, s4, s1)
        case (2, 4, 1, 3) => sort5(qr, sortCols, numRows, t5, t2, t4, t1, t3, dt5, dt2, dt4, dt1, dt3, at5, at2, at4, at1, at3, i5, i2, i4, i1, i3, s5, s2, s4, s1, s3)
        case (2, 4, 3, 1) => sort5(qr, sortCols, numRows, t5, t2, t4, t3, t1, dt5, dt2, dt4, dt3, dt1, at5, at2, at4, at3, at1, i5, i2, i4, i3, i1, s5, s2, s4, s3, s1)
        case (3, 1, 2, 4) => sort5(qr, sortCols, numRows, t5, t3, t1, t2, t4, dt5, dt3, dt1, dt2, dt4, at5, at3, at1, at2, at4, i5, i3, i1, i2, i4, s5, s3, s1, s2, s4)
        case (3, 1, 4, 2) => sort5(qr, sortCols, numRows, t5, t3, t1, t4, t2, dt5, dt3, dt1, dt4, dt2, at5, at3, at1, at4, at2, i5, i3, i1, i4, i2, s5, s3, s1, s4, s2)
        case (3, 2, 1, 4) => sort5(qr, sortCols, numRows, t5, t3, t2, t1, t4, dt5, dt3, dt2, dt1, dt4, at5, at3, at2, at1, at4, i5, i3, i2, i1, i4, s5, s3, s2, s1, s4)
        case (3, 2, 4, 1) => sort5(qr, sortCols, numRows, t5, t3, t2, t4, t1, dt5, dt3, dt2, dt4, dt1, at5, at3, at2, at4, at1, i5, i3, i2, i4, i1, s5, s3, s2, s4, s1)
        case (3, 4, 1, 2) => sort5(qr, sortCols, numRows, t5, t3, t4, t1, t2, dt5, dt3, dt4, dt1, dt2, at5, at3, at4, at1, at2, i5, i3, i4, i1, i2, s5, s3, s4, s1, s2)
        case (3, 4, 2, 1) => sort5(qr, sortCols, numRows, t5, t3, t4, t2, t1, dt5, dt3, dt4, dt2, dt1, at5, at3, at4, at2, at1, i5, i3, i4, i2, i1, s5, s3, s4, s2, s1)
        case (4, 1, 2, 3) => sort5(qr, sortCols, numRows, t5, t4, t1, t2, t3, dt5, dt4, dt1, dt2, dt3, at5, at4, at1, at2, at3, i5, i4, i1, i2, i3, s5, s4, s1, s2, s3)
        case (4, 1, 3, 2) => sort5(qr, sortCols, numRows, t5, t4, t1, t3, t2, dt5, dt4, dt1, dt3, dt2, at5, at4, at1, at3, at2, i5, i4, i1, i3, i2, s5, s4, s1, s3, s2)
        case (4, 2, 1, 3) => sort5(qr, sortCols, numRows, t5, t4, t2, t1, t3, dt5, dt4, dt2, dt1, dt3, at5, at4, at2, at1, at3, i5, i4, i2, i1, i3, s5, s4, s2, s1, s3)
        case (4, 2, 3, 1) => sort5(qr, sortCols, numRows, t5, t4, t2, t3, t1, dt5, dt4, dt2, dt3, dt1, at5, at4, at2, at3, at1, i5, i4, i2, i3, i1, s5, s4, s2, s3, s1)
        case (4, 3, 1, 2) => sort5(qr, sortCols, numRows, t5, t4, t3, t1, t2, dt5, dt4, dt3, dt1, dt2, at5, at4, at3, at1, at2, i5, i4, i3, i1, i2, s5, s4, s3, s1, s2)
        case (4, 3, 2, 1) => sort5(qr, sortCols, numRows, t5, t4, t3, t2, t1, dt5, dt4, dt3, dt2, dt1, at5, at4, at3, at2, at1, i5, i4, i3, i2, i1, s5, s4, s3, s2, s1)
        case (a, b, c, d) => println(s"ILLEGAL sort order positions: ($a, $b, $c, $d)"); null
      }
      case n => println(s"ILLEGAL sort order positions: ($p1, $p2, $p3, $p4, $p5)"); null
    }
  }
}


//  val l = Seq(1, 2, 3, 4, 5)
//  println((for {
//    a <- l
//    b <- l.filterNot(_ == a)
//    c <- l.filterNot(v => v == a || v == b)
//    d <- l.filterNot(v => v == a || v == b || v == c)
//    e <- l.filterNot(v => v == a || v == b || v == c || v == d)
//  } yield {
//    s"case ($a, $b, $c, $d, $e) => sort5(qr, sortCols, numRows, t$a, t$b, t$c, t$d, t$e, i$a, i$b, i$c, i$d, i$e, s$a, s$b, s$c, s$d, s$e)"
//  }).mkString("\n"))
//
//  val y = Seq(
//    ("string", "String", "str"),
//    ("double", "Double", "dou"),
//  )
//  println((for {
//    (s1, t1, f1) <- y
//    (s2, t2, f2) <- y
//    (s3, t3, f3) <- y
//    (s4, t4, f4) <- y
//    (s5, t5, f5) <- y
//  } yield {
////    s"""case ("$s1", "$s2") => indexArray2[$t1, $t2](numRows, $f1(qr, o1, i1), $f2(qr, o2, i2), s1, s2)"""
////    s"""case ("$s1", "$s2", "$s3") => indexArray2[$t1, $t2, $t3](numRows, $f1(qr, o1, i1), $f2(qr, o2, i2), $f3(qr, o3, i3), s1, s2, s3)"""
////    s"""case ("$s1", "$s2", "$s3", "$s4") => indexArray4[$t1, $t2, $t3, $t4](numRows, $f1(qr, o1, i1), $f2(qr, o2, i2), $f3(qr, o3, i3), $f4(qr, o4, i4), s1, s2, s3, s4)"""
//    s"""case ("$s1", "$s2", "$s3", "$s4", "$s5") => indexArray5[$t1, $t2, $t3, $t4, $t5](numRows, $f1(qr, o1, i1), $f2(qr, o2, i2), $f3(qr, o3, i3), $f4(qr, o4, i4), $f5(qr, o5, i5), s1, s2, s3, s4, s5)"""
//  }).mkString("\n"))