package moleculeadmin.shared.ops.query

import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ast.schema.{Attr, Ns}
import molecule.ast.model._
import molecule.transform.Model2Query.coalesce
import moleculeadmin.shared.util.HelpersAdmin
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
    //    "edit",
    "orig",
    "count", "count-distinct",
    "sum", "avg", "median", "variance", "stddev",
    "t", "tx", "txInstant"
  )

  def isEditable(cols: Seq[Col],
                 i: Int,
                 nsAlias: String,
                 nsFull: String): Boolean = {
    cols.foldRight(
      0, // checking
      false // editable
    ) {
      // skip cols after asking col
      case (col,
      (0, false)) if col.colIndex > i => (0, false)

      // Not editable if aggr or tx
      case (Col(`i`, _, _, _, _, _, _, _, _, _, _, attrExpr, _, _),
      (0, false)) if nonMenuExprs.contains(attrExpr) => (0, false)

      // Start checking
      case (Col(`i`, _, _, _, _, _, _, _, _, _, _, _, _, _),
      (0, false)) => (1, false)

      // We have en entity id in namespace
      case (Col(_, _, `nsAlias`, `nsFull`, "e", _, _, _, _, _, _, _, _, _),
      (1, false)) => (1, true)

      // Other cols before asking col - still no entity id - continue checking
      case (Col(_, _, `nsAlias`, `nsFull`, _, _, _, _, _, _, _, _, _, _),
      (1, false)) => (1, false)

      // Attributes before `e` in this namespace disqualifies editing
      case (Col(_, _, `nsAlias`, `nsFull`, _, _, _, _, _, _, _, _, _, _),
      (1, true)) => (0, false)

      // Previous namespaces
      case (_, (_, editable)) => (0, editable)
    }._2
  }

  def getEidColIndex(cols: Seq[Col],
                     i: Int,
                     nsAlias: String,
                     nsFull: String): Int = {
    cols.foldRight(-1) {
      // skip asking col and cols after
      case (col, -1) if col.colIndex >= i => -1

      // Entity id col
      case (Col(colIndex, _, `nsAlias`, `nsFull`, "e",
      _, _, _, _, _, _, _, _, _), -1) => colIndex

      // Previous cols
      case (_, colIndex) => colIndex
    }
  }

  def getEidColIndexes(cols: Seq[Col]): Seq[Int] = {
    cols.collect {
      case Col(colIndex, _, _, _, "e", _, _, _, _, _, _, _, _, _) => colIndex + 1
    }
  }

  def getCols(elements: Seq[Element])
             (implicit nsMap: Map[String, Ns]): Seq[Col] = {
    var i          = 0
    val cols       = new ListBuffer[Col]()
    var related    = 0
    var firstNs    = true
    var curNsAlias = ""
    var curNs      = ""
    var curRefAttr = ""
    var curRefNs   = ""
    var prevAttr   = ""
    elements.foreach {
      case Atom(nsFull, attr, tpe, card, value, enumPrefix, _, keys) if attr.last != '_' => {
        if (firstNs) {
          curNsAlias = nsFull
          curNs = nsFull
          firstNs = false
        }
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
          i, related, curNsAlias, curNs, attr, tpe,
          getColType(attr, card, tpe),
          card,
          attr.last == '$',
          enums,
          getAggrType(value),
          expr
        )
        i += 1

        curRefAttr = ""
        curRefNs = ""
        prevAttr = attr
      }

      case _: Atom => // tacit attribute

      case Generic(nsFull, attr, tpe, value) =>
        val nsAlias = if (curNsAlias.nonEmpty) curNsAlias else nsFull
        attr match {
          case "e" =>
            cols += Col(i, related, nsAlias, nsFull, attr, tpe, "double", 1, aggrType = getAggrType(value), attrExpr = getExpr(value))
            i += 1

          case "t" | "tx" =>
            cols += Col(i, related, nsAlias, nsFull, prevAttr, "Long", "double", 1, attrExpr = attr)
            i += 1

          case "txInstant" =>
            cols += Col(i, related, nsAlias, nsFull, prevAttr, "Date", "string", 1, attrExpr = attr)
            i += 1

          case _ =>
        }

      case Bond(_, refAttr1, refNs1, _, _) =>
        related = 1
        curRefAttr = if (curRefAttr.isEmpty) refAttr1.capitalize else curRefAttr + " > " + refAttr1.capitalize
        curRefNs = if (curRefNs.isEmpty) refNs1 else curRefNs + " > " + refNs1
        curNsAlias = curRefAttr
        curNs = curRefNs

      case _: ReBond =>
      case e         => throw new IllegalArgumentException("Unexpected element for table layout: " + e)
    }

    cols
  }


  def getSortedColumns(cols: Seq[Col], colIndex: Int, additive: Boolean): Seq[Col] = {

    def singleToggle(): Seq[Col] = cols.map {
      case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "", _)     => col.copy(sortDir = "asc", sortPos = 0)
      case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "asc", _)  => col.copy(sortDir = "desc", sortPos = 0)
      case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "desc", _) => col.copy(sortDir = "", sortPos = 0)
      case col                                                             => col.copy(sortDir = "", sortPos = 0)
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
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, _, _) => col.copy(sortDir = "asc", sortPos = 2)
              case col@Col(`i`, _, _, _, _, _, _, _, _, _, _, _, _, _)        => col.copy(sortPos = 1)
              case col                                                        => col
            }
          }
        }

        case 2 => {
          if (curSortCols.exists(c => c.colIndex == colIndex && c.sortDir == "desc")) {
            // Turning off cur col sorting - one sort left (no position)
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "desc", _)             => col.copy(sortDir = "", sortPos = 0)
              case col@Col(i, _, _, _, _, _, _, _, _, _, _, _, sort, sortPos) if sort.nonEmpty => col.copy(sortPos = 0)
              case col                                                                         => col
            }
          } else {
            val newLastSortPos = curSortCols.map(_.sortPos).max + 1
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "", _)    => col.copy(sortDir = "asc", sortPos = newLastSortPos)
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "asc", _) => col.copy(sortDir = "desc")
              case col                                                            => col
            }
          }
        }

        case more => {
          if (curSortCols.exists(c => c.colIndex == colIndex && c.sortDir == "desc")) {
            // Turning off cur col sorting - shift all higher positions down
            val outgoingPos = curSortCols.find(_.colIndex == colIndex).get.sortPos
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "desc", _)                                      => col.copy(sortDir = "", sortPos = 0)
              case col@Col(_, _, _, _, _, _, _, _, _, _, _, _, sort, sortPos) if sort.nonEmpty && sortPos > outgoingPos => col.copy(sortPos = sortPos - 1)
              case col                                                                                                  => col
            }
          } else {
            val newLastSortPos = curSortCols.map(_.sortPos).max + 1
            cols.map {
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "", _)    => col.copy(sortDir = "asc", sortPos = newLastSortPos)
              case col@Col(`colIndex`, _, _, _, _, _, _, _, _, _, _, _, "asc", _) => col.copy(sortDir = "desc")
              case col                                                            => col
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
