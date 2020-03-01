package moleculeadmin.shared.ops.query

import molecule.ast.model._
import molecule.transform.Model2Query.coalesce
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ast.schema.{Attr, Ns}
import moleculeadmin.shared.util.HelpersAdmin
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


trait ColOps extends HelpersAdmin {

  private def getColType(attr: String, card: Int, tpe: String): String = (card, tpe) match {
    case (1, "String" | "Boolean" | "Date" | "UUID" | "URI" | "BigInt" | "BigDecimal") => "string"
    case (1, "Int" | "Long" | "ref" | "Float" | "Double")                              => "double"
    case (2, "String" | "Boolean" | "Date" | "UUID" | "URI" | "BigInt" | "BigDecimal") => "listString"
    case (2, "Int" | "Long" | "ref" | "Float" | "Double")                              => "listDouble"
    case (_, "String" | "Boolean" | "Date" | "UUID" | "URI" | "BigInt" | "BigDecimal") => "mapString"
    case (_, "Int" | "Long" | "ref" | "Float" | "Double")                              => "mapDouble"
    case _ if attr == dummy                                                            => "double"
    case _                                                                             =>
      throw new RuntimeException(s"Unexpected colType input: $attr   -   $card   -   $tpe")
  }

  def getAggrType(value: Value) = value match {
    case fn: Fn   => fn match {
      case Fn("count" | "count-distinct", _)                   => "aggrInt"
      case Fn("avg" | "variance" | "stddev", _)                => "aggrDouble"
      case Fn("min" | "max" | "rand" | "sum" | "median", None) => "aggrSingle"
      case Fn("sample", Some(1))                               => "aggrSingleSample"
      case Fn("min" | "max" | "sample", Some(_))               => "aggrList"
      case Fn("rand", Some(_))                                 => "aggrListRand"
      case other                                               =>
        throw new RuntimeException(s"Unexpected fn: $other")
    }
    case Distinct => "aggrListDistinct"
    case _        => ""
  }

  def getExpr(value: Value): String = value match {
    case Distinct                                => "distinct"
    case Fn("not", _)                            => "not"
    case Eq(Nil)                                 => "nil"
    case Eq((seq: Seq[_]) :: Nil) if seq.isEmpty => "nil"
    case Eq((seq: Seq[_]) :: Nil)                => s"= $seq"
    case Eq(arg :: Nil)                          => s"= ${renderValue(arg)}"
    case Eq(args)                                => "= " + args.mkString(" OR ")
    case Neq(arg :: Nil)                         => s"!= ${renderValue(arg)}"
    case Neq(args)                               => s"!= ${renderValue(args.head)}"
    case Gt(arg)                                 => s"> ${renderValue(arg)}"
    case Ge(arg)                                 => s">= ${renderValue(arg)}"
    case Lt(arg)                                 => s"< ${renderValue(arg)}"
    case Le(arg)                                 => s"<= ${renderValue(arg)}"
    case Fn(fn, Some(i))                         => s"$fn($i)"
    case Fn(fn, _) if coalesce(fn)               => fn
    case Fn(fn, _)                               => fn
    case Fulltext(arg :: Nil)                    => s"word: $arg"
    case Fulltext(args)                          => s"words: $args"
    case _                                       => ""
  }

  val singleAggrTypes = Seq(
    "aggrInt", "aggrDouble", "aggrSingle", "aggrSingleSample")

  val nonMenuExprs = Seq(
    "orig",
    "count", "count-distinct",
    "sum", "avg", "median", "variance", "stddev",
    "t", "tx", "txInstant"
  )

  def isEditable(
    cols: Seq[Col],
    colIndex: Int,
    nsAlias: String,
    nsFull: String
  ): Boolean = {
    cols.foldRight(
      0, // checking
      false // editable
    ) {
      // skip cols after asking col
      case (col,
      (0, false)) if col.colIndex > colIndex => (0, false)

      // Not editable if aggr or tx
      case (Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, attrExpr, _, _, _),
      (0, false)) if nonMenuExprs.contains(attrExpr) => (0, false)

      // Start checking
      case (Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
      (0, false)) => (1, false)

      // We have en entity id in namespace
      case (Col(_, _, `nsAlias`, `nsFull`, "e", _, _, _, _, _, _, _, _, _, _),
      (1, false)) => (1, true)

      // Other cols before asking col - still no entity id - continue checking
      case (Col(_, _, `nsAlias`, `nsFull`, _, _, _, _, _, _, _, _, _, _, _),
      (1, false)) => (1, false)

      // Attributes before `e` in this namespace disqualifies editing
      case (Col(_, _, `nsAlias`, `nsFull`, _, _, _, _, _, _, _, _, _, _, _),
      (1, true)) => (0, false)

      // Previous namespaces
      case (_, (_, editable)) => (0, editable)
    }._2
  }

  def getEidColIndex(
    cols: Seq[Col],
    colIndex: Int,
    nsAlias: String,
    nsFull: String
  ): Int = {
    cols.foldRight(-1) {
      // skip asking cols after
      case (col, -1) if col.colIndex > colIndex => -1

      // Entity id col
      case (Col(colIndex, _, `nsAlias`, `nsFull`, "e",
      _, _, _, _, _, _, _, _, _, _), -1) => colIndex

      // Previous cols
      case (_, colIndex) => colIndex
    }
  }

  case class ResolveAttrs(cols: Seq[Col]) {
    var hasGroupEdit = false
    val attrCount    = mutable.Map.empty[String, Int]
    val attrIndex    = mutable.Map.empty[Int, Int]

    cols.collect {
      case col if col.attrExpr != "edit" =>
        val cleanAttr = if (col.card > 1) clean(col.attr) else col.attr
        val count     = attrCount.getOrElse(cleanAttr, 0)
        attrIndex += col.colIndex -> (count + 1)
        attrCount(cleanAttr) = count + 1

      case col if col.attrExpr == "edit" =>
        hasGroupEdit = true
        val cleanAttr = if (col.card > 1) clean(col.attr) else col.attr
        val count     = attrCount.getOrElse(cleanAttr, 0)
        attrIndex += col.colIndex -> count
        attrCount(cleanAttr) = count
    }

    //    attrCount foreach println
    //    println("--------")
    //    attrIndex.toList.sortBy(_._1) foreach println

    def postfixed(col: Col): String = {
      if (hasGroupEdit) {
        val cleanAttr = if (col.card > 1) clean(col.attr) else col.attr
        cleanAttr + (if (attrCount(cleanAttr) == 1) "" else attrIndex(col.colIndex))
      } else col.attr
    }

    def postfix(col: Col): String = {
      if (hasGroupEdit) {
        val cleanAttr = if (col.card > 1) clean(col.attr) else col.attr
        if (attrCount(cleanAttr) == 1) "" else s"${attrIndex(col.colIndex)}"
      } else ""
    }
  }

  def getTxArrays(
    cols: Seq[Col],
    qr: QueryResult,
    nsAlias: String,
    nsFull: String,
    attr: String
  ): (
    Int,
      Option[Array[Option[Double]]], Int,
      Option[Array[Option[Double]]], Int,
      Option[Array[Option[String]]], Int
    ) = {
    cols.foldLeft(
      0,
      Option.empty[Array[Option[Double]]], 0,
      Option.empty[Array[Option[Double]]], 0,
      Option.empty[Array[Option[String]]], 0
    ) {
      case (_, Col(colIndex1, _, `nsAlias`, `nsFull`, "e", _, _, _, _, _, _, _, _, _, _)) =>
        (colIndex1, None, 0, None, 0, None, 0)

      case (
        (eColIndex, _, _, _, _, _, _),
        Col(colIndex1, _, _, `nsFull`, `attr`, _, _, _, _, _, _, "t", _, _, _)
        ) =>
        (eColIndex,
          Some(qr.num(qr.arrayIndexes(colIndex1))), colIndex1, None, 0, None, 0)

      case (
        (eColIndex, tArray, tIndex, _, _, _, _),
        Col(colIndex1, _, _, `nsFull`, `attr`, _, _, _, _, _, _, "tx", _, _, _)
        ) =>
        (eColIndex, tArray, tIndex,
          Some(qr.num(qr.arrayIndexes(colIndex1))), colIndex1, None, 0)

      case (
        (eColIndex, tArray, tIndex, txArray, txIndex, _, _),
        Col(colIndex1, _, _, `nsFull`, `attr`, _, _, _, _, _, _, "txInstant", _, _, _)
        ) =>
        (eColIndex, tArray, tIndex, txArray, txIndex,
          Some(qr.str(qr.arrayIndexes(colIndex1))), colIndex1)

      case (acc, _) => acc
    }
  }

  def getEidTableColIndexes(cols: Seq[Col]): Seq[Int] = {
    cols.collect {
      case Col(colIndex, _, _, _, "e", _, _, _, _, _, aggrType, _, _, _, _)
        if aggrType.isEmpty => colIndex + 1
    }
  }

  def getGroupableCols(cols: Seq[Col]): Seq[Col] = {
    cols.foldLeft(-1, "", Seq.empty[Col]) {
      // eid is first in new ns
      case ((-1, "", cols), Col(_, _, nsAlias, _, "e", _, _, _, _, _, _, _, _, _, _)) =>
        (1, nsAlias, cols)

      // new namespace
      case ((0, curNsAlias, cols), Col(_, _, nsAlias, _, "e", _, _, _, _, _, _, _, _, _, _))
        if curNsAlias != nsAlias =>
        (1, nsAlias, cols)

      case ((1, curNsAlias, cols), col@Col(_, _, nsAlias, _, _, _, _, _, _, _, _, attrExpr, _, _, _))
        if curNsAlias == nsAlias && !nonMenuExprs.contains(attrExpr) =>
        (1, nsAlias, cols :+ col)

      case ((_, _, cols), Col(_, _, nsAlias, _, _, _, _, _, _, _, _, _, _, _, _)) =>
        (0, nsAlias, cols)
    }._3
  }

  def getCols(elements: Seq[Element])
             (implicit nsMap: Map[String, Ns]): Seq[Col] = {
    var i          = 0
    val cols       = new ListBuffer[Col]()
    var related    = 0
    var curNsAlias = ""
    var prevAttr   = ""
    elements.foreach {
      case Atom(nsFull, attr, tpe, card, value, enumPrefix, _, keys) if attr.last != '_' => {
        if (curNsAlias.isEmpty)
          curNsAlias = nsFull

        val enums = if (enumPrefix.isDefined) {
          val attr0 = clean(attr)
          nsMap(nsFull).attrs.collectFirst {
            case Attr(_, `attr0`, _, _, enumsOpt, _, _, _, _, _, _, _, _) =>
              enumsOpt
          }.getOrElse(Option.empty[Set[String]])
            .fold(Seq.empty[String])(_.toSeq.sorted)
        } else {
          Seq.empty[String]
        }
        val expr  = if (keys == Seq("edit"))
          "edit"
        else if (keys.nonEmpty && keys.head == "orig")
          "orig"
        else
          getExpr(value)
        cols += Col(
          i, related, curNsAlias, nsFull, attr, tpe,
          getColType(attr, card, tpe),
          card,
          attr.last == '$',
          enums,
          getAggrType(value),
          expr
        )
        i += 1
        prevAttr = attr
      }

      case _: Atom => // tacit attribute

      case Generic(nsFull, attr, tpe, value) =>
        if (curNsAlias.isEmpty)
          curNsAlias = nsFull

        attr match {
          case "e" =>
            cols += Col(i, related, curNsAlias, nsFull, attr, tpe, "double", 1, aggrType = getAggrType(value), attrExpr = getExpr(value))
            i += 1

          case "t" | "tx" =>
            cols += Col(i, related, curNsAlias, nsFull, prevAttr, "Long", "double", 1, attrExpr = attr)
            i += 1

          case "txInstant" =>
            cols += Col(i, related, curNsAlias, nsFull, prevAttr, "Date", "string", 1, attrExpr = attr)
            i += 1

          case _ =>
        }

      case Bond(_, refAttr1, _, _, _) =>
        related = 1
        curNsAlias = refAttr1.capitalize

      case _: ReBond =>
      case e         => throw new IllegalArgumentException("Unexpected element for table layout: " + e)
    }

    cols
  }


  def colSettings(cols: Seq[Col]): Seq[(Int, String, Int)] =
    cols.map(c => (c.colIndex, c.sortDir, c.sortPos))


  def getSortedColumns(cols: Seq[Col], colIndex: Int, additive: Boolean): Seq[Col] = {

    def singleToggle(): Seq[Col] = cols.map {
      case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "", _, _)     => col.copy(sortDir = "asc", sortPos = 0)
      case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "asc", _, _)  => col.copy(sortDir = "desc", sortPos = 0)
      case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "desc", _, _) => col.copy(sortDir = "", sortPos = 0)
      case col                                                                => col.copy(sortDir = "", sortPos = 0)
    }

    if (additive) {
      val curSortCols = cols.filter(_.sortDir.nonEmpty)
      curSortCols.size match {
        case 0 => singleToggle()
        case 1 => {
          val curSortCol = curSortCols.head
          val i          = curSortCol.colIndex
          if (curSortCol.colIndex == colIndex) {
            // toggle existing (same effect as without holding shift down)
            singleToggle()
          } else {
            // Add new asc and position both
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => col.copy(sortDir = "asc", sortPos = 2)
              case col@Col(`i`, _, _, _, _, _, _, _, _, _, _, _, _, _, _)        => col.copy(sortPos = 1)
              case col                                                           => col
            }
          }
        }

        case 2 => {
          if (curSortCols.exists(c => c.colIndex == colIndex && c.sortDir == "desc")) {
            // Turning off cur col sorting - one sort left (no position)
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "desc", _, _)       => col.copy(sortDir = "", sortPos = 0)
              case col@Col(i, _, _, _, _, _, _, _, _, _, _, _, sort, _, _) if sort.nonEmpty => col.copy(sortPos = 0)
              case col                                                                      => col
            }
          } else {
            val newLastSortPos = curSortCols.map(_.sortPos).max + 1
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "", _, _)    => col.copy(sortDir = "asc", sortPos = newLastSortPos)
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "asc", _, _) => col.copy(sortDir = "desc")
              case col                                                               => col
            }
          }
        }

        case more => {
          if (curSortCols.exists(c => c.colIndex == colIndex && c.sortDir == "desc")) {
            // Turning off cur col sorting - shift all higher positions down
            val outgoingPos = curSortCols.find(_.colIndex == colIndex).get.sortPos
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "desc", _, _)                                      => col.copy(sortDir = "", sortPos = 0)
              case col@Col(_, _, _, _, _, _, _, _, _, _, _, _, sort, sortPos, _) if sort.nonEmpty && sortPos > outgoingPos => col.copy(sortPos = sortPos - 1)
              case col                                                                                                     => col
            }
          } else {
            val newLastSortPos = curSortCols.map(_.sortPos).max + 1
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "", _, _)    => col.copy(sortDir = "asc", sortPos = newLastSortPos)
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "asc", _, _) => col.copy(sortDir = "desc")
              case col                                                               => col
            }
          }
        }
      }
    } else {
      // Non-additive (simple click) clears all previous positions and toggles this column
      singleToggle()
    }
  }
}
