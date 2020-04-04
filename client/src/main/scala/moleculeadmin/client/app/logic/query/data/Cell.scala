package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.edit.{RetractEid, _}
import moleculeadmin.client.app.logic.query.marker.Toggle
import moleculeadmin.client.app.logic.query.views.Base
import moleculeadmin.client.app.html.query.datatable.BodyElements
import moleculeadmin.shared.ast.query.{QueryResult, _}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.document
import org.scalajs.dom.html.{TableCell, TableRow, TableSection}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.collection.mutable


abstract class Cell(
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult
)(implicit ctx: Ctx.Owner)
  extends TxLambdas(cols, qr)
    with TypeValidation
    with BodyElements
    with ColOps {

  // current entity id for updates of subsequent attributes on each row
  var e                           = 0L
  var curStarToggler : () => Unit = _
  var curFlagToggler : () => Unit = _
  var curCheckToggler: () => Unit = _

  // Avoid creating id variable for each cell
  //  var id = ""

  protected def cellLambda(colIndex: Int): Int => JsDom.TypedTag[TableCell] = {
    val arrayIndex = qr.arrayIndexes(colIndex)

    val Col(_, related, nsAlias, nsFull, attr, attrType, colType,
    card, _, enums, _, expr, _, _, _) = cols(colIndex)

    lazy val noCount    = expr != "count" && expr != "count-distinct"
    lazy val aggregates = Seq(
      "count", "count-distinct", "sum", "avg", "median", "variance", "stddev"
    )

    val cellType = "" match {
      case _ if aggregates.contains(expr)             => "aggr"
      case _ if attr == "e" && noCount                => "eid"
      case _ if attrType == "ref" && noCount          => "ref"
      case _ if expr == "t"                           => "t"
      case _ if expr == "tx"                          => "tx"
      case _ if expr == "txInstant"                   => "txI"
      case _ if attrType == "String" && enums.isEmpty => "str"
      case _ if attrType == "Date"                    => "date"
      case _ if attrType.startsWith("Big")            => "big"
      case _                                          => ""
    }

    // e has to be first within namespace to allow editing
    val groupEdit = expr == "edit"
    val editable  = groupEdit || isEditable(cols, colIndex, nsAlias, nsFull)
    lazy val showAll = expr == "orig" || expr == "edit"


    def idBase(colIndex: Int): Int => String =
      (rowIndex: Int) => s"col-${colIndex + 1} row-${rowIndex + 1}"

    // Create cell id and assign togglers
    val mkId: Int => String = (rowIndex: Int) => {
      val id = idBase(colIndex)(rowIndex)
      starTogglers = starTogglers + (id -> curStarToggler)
      flagTogglers = flagTogglers + (id -> curFlagToggler)
      checkTogglers = checkTogglers + (id -> curCheckToggler)
      id
    }

    /**
     * @tparam T cardinality 1: String / Double
     *           cardinality 2: List[String] / List[Double]
     *           cardinality 3: Map[String, String] / Map[String, Double]
     **/
    def update[T](
      origArray: Array[Option[T]],
      array: Array[Option[T]],
      rowIndex: Int,
      baseClass: String
    ): () => Unit = {
      val cellId : String    = idBase(colIndex)(rowIndex)
      val oldVOpt: Option[T] = array(rowIndex)
      val isNum  : Boolean   = Seq(
        "Int", "Long", "ref", "datom", "Float", "Double").contains(attrType)
      val updater            = card match {
        case 1 => UpdateCardOne(
          cols, qr, origArray, array, baseClass, colType, colIndex, rowIndex,
          related, nsAlias, nsFull, attr, attrType, enums, expr
        )
        case 2 => UpdateCardMany(
          cols, qr, origArray, array, baseClass, colType, rowIndex,
          related, nsAlias, nsFull, attr, attrType, enums, cellType, expr
        )
        case 3 => UpdateCardMap(
          cols, qr, origArray, array, baseClass, rowIndex,
          related, nsAlias, nsFull, attr, attrType, enums, expr
        )
      }
      () => {
        val cell: TableCell = document.getElementById(cellId).asInstanceOf[TableCell]
        val row : TableRow  = cell.parentNode.asInstanceOf[TableRow]
        val eid : Long      = cell.getAttribute("eid").toLong
        updater.update(mkId, cellId, cell, row, eid, oldVOpt, isNum)
      }
    }

    def getOrigArray[T](arrays: List[Array[Option[T]]]): Array[Option[T]] = {
      if (groupEdit)
        arrays(arrayIndex - 1)
      else
        Array.empty[Option[T]]
    }

    def getClassLambda[T](
      origArray: Array[Option[T]],
      array: Array[Option[T]]
    ): (String, Int) => String = {
      if (groupEdit)
        (baseClass: String, rowIndex: Int) => {
          val oldV = origArray(rowIndex)
          val newV = array(rowIndex)
          if (oldV == newV)
            baseClass
          else
            newV match {
              case None                    => s"$baseClass retract"
              case Some(_) if oldV.isEmpty => s"$baseClass assert"
              case Some(_)                 => s"$baseClass update"
            }
        }
      else
        (baseClass: String, _: Int) => baseClass
    }

    colType match {

      // card one --------------------------------------------------------------

      case _ if cellType == "aggr" =>
        val array = qr.num(arrayIndex)
        (rowIndex: Int) =>
          array(rowIndex).fold(
            _tdNoAggrEdit(mkId(rowIndex))
          )(
            _tdOneNumNoAggrEdit(mkId(rowIndex), _)
          )

      case "string" =>
        val origArray = getOrigArray(qr.str)
        val array     = qr.str(arrayIndex)
        if (editable) {
          val getCls = getClassLambda(origArray, array)
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                val s = array(rowIndex)
                _tdOneStrEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  if (s.getOrElse("").startsWith("http"))
                    _urlSpan(s.get, true)
                  else
                    _optStr2frags(s),
                  update(origArray, array, rowIndex, "str")
                )

            case "date" =>
              (rowIndex: Int) =>
                _tdOneDateEdit(
                  mkId(rowIndex),
                  getCls("date", rowIndex),
                  e,
                  array(rowIndex),
                  update(origArray, array, rowIndex, "date")
                )

            case "big" =>
              (rowIndex: Int) =>
                _tdOneNumEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex),
                  e,
                  array(rowIndex),
                  update(origArray, array, rowIndex, "num")
                )

            case _ =>
              (rowIndex: Int) =>
                _tdOneEdit(
                  mkId(rowIndex),
                  getCls("", rowIndex),
                  e,
                  array(rowIndex),
                  update(origArray, array, rowIndex, "")
                )
          }
        } else {
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(s =>
                  _tdNoEdit(mkId(rowIndex))(
                    if (s.startsWith("http"))
                      _urlSpan(s, false)
                    else
                      _str2frags(s)
                  )
                )

            case "date" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(d =>
                  _tdOneDate(mkId(rowIndex), truncateDateStr(d))
                )

            case "big" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(
                  _tdOneNumNoEdit(mkId(rowIndex), _)
                )

            case "txI" => txInstantLambda(mkId, arrayIndex, colIndex)

            case _ =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(
                  _tdNoEdit(mkId(rowIndex))(_)
                )
          }
        }

      case "double" =>
        val origArray = getOrigArray(qr.num)
        val array     = qr.num(arrayIndex)
        cellType match {
          case "eid" =>
            val length      = array.length
            val starIndex   = new Array[Boolean](length)
            val flagIndex   = new Array[Boolean](length)
            val checkIndex  = new Array[Boolean](length)
            val entityIndex = mutable.LongMap.empty[List[Int]]
            var eid         = 0L
            var i           = 0
            while (i < length) {
              eid = array(i).get.toLong
              entityIndex.get(eid) match {
                case Some(ii) => entityIndex(eid) = ii :+ i
                case None     => entityIndex(eid) = List(i)
              }
              starIndex(i) = curStars.contains(eid)
              flagIndex(i) = curFlags.contains(eid)
              checkIndex(i) = curChecks.contains(eid)
              i += 1
            }

            val tableCol = colIndex + 1
            curEntityIndexes(tableCol) = entityIndex
            curStarIndexes(tableCol) = starIndex
            curFlagIndexes(tableCol) = flagIndex
            curCheckIndexes(tableCol) = checkIndex

            (rowIndex: Int) =>
              // Set entity id for updates of subsequent attribute values
              e = array(rowIndex).fold(0L)(_.toLong)
              val eid = e

              curStarToggler = () =>
                Toggle(tableBody, "star", curStars.contains(eid), eid = eid)
              curFlagToggler = () =>
                Toggle(tableBody, "flag", curFlags.contains(eid), eid = eid)
              curCheckToggler = () =>
                Toggle(tableBody, "check", curChecks.contains(eid), eid = eid)

              _tdOneEid(
                mkId(rowIndex),
                eid,
                curEntity,
                setCurEid(false)(eid),
                lockCurEid(false)(eid),
                if (starIndex(rowIndex)) mark.starOn else mark.starOff,
                if (flagIndex(rowIndex)) mark.flagOn else mark.flagOff,
                if (checkIndex(rowIndex)) mark.checkOn else mark.checkOff,
                () => RetractEid(eid),
                curStarToggler,
                curFlagToggler,
                curCheckToggler,
              )

          case "ref" if groupEdit =>
            (rowIndex: Int) =>
              _tdOneRefEdit2(
                mkId(rowIndex),
                e,
                array(rowIndex),
                update(origArray, array, rowIndex, "num")
              )

          case "ref" if editable =>
            (rowIndex: Int) =>
              _tdOneRefEdit(
                mkId(rowIndex),
                e,
                array(rowIndex),
                curEntity,
                setCurEid(true),
                lockCurEid(true),
                update(origArray, array, rowIndex, "num")
              )

          case _ if editable =>
            val getCls = getClassLambda(origArray, array)
            (rowIndex: Int) =>
              _tdOneNumEdit(
                mkId(rowIndex),
                getCls("num", rowIndex),
                e,
                array(rowIndex),
                update(origArray, array, rowIndex, "num")
              )

          case "ref" =>
            (rowIndex: Int) =>
              array(rowIndex).fold(_tdNoEdit(mkId(rowIndex)))(v =>
                _tdOneRef(
                  mkId(rowIndex),
                  v.toLong,
                  curEntity,
                  setCurEid(false),
                  lockCurEid(false),
                )
              )

          case "t"  => tLambda(mkId, arrayIndex, colIndex)
          case "tx" => txLambda(mkId, arrayIndex, colIndex)

          case _ =>
            (rowIndex: Int) =>
              mkId(rowIndex)
              array(rowIndex).fold(
                _tdNoEdit(mkId(rowIndex))
              )(
                _tdOneNumNoEdit(mkId(rowIndex), _)
              )
        }


      // card many -------------------------------------------------------------

      case "listString" =>
        val origArray = getOrigArray(qr.listStr)
        val array     = qr.listStr(arrayIndex)
        if (editable) {
          val getCls = getClassLambda(origArray, array)
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                _tdManyStringEdit(
                  mkId(rowIndex),
                  getCls("items", rowIndex),
                  e,
                  array(rowIndex).getOrElse(List.empty[String]).sorted.map(s =>
                    if (s.startsWith("http"))
                      Seq(_urlSpan(s, true))
                    else
                      _str2frags(s)
                  ),
                  update(origArray, array, rowIndex, "items")
                )

            case "date" =>
              (rowIndex: Int) =>
                _tdManyDateEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  array(rowIndex).getOrElse(List.empty[String]).sorted,
                  update(origArray, array, rowIndex, "str")
                )

            case "big" =>
              (rowIndex: Int) =>
                _tdManyStringBigEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex),
                  e,
                  array(rowIndex).getOrElse(List.empty[String]).sorted,
                  update(origArray, array, rowIndex, "num")
                )

            case _ =>
              (rowIndex: Int) =>
                _tdManyStringOtherEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  array(rowIndex).getOrElse(List.empty[String]).sorted,
                  update(origArray, array, rowIndex, "str")
                )
          }
        } else {
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(_tdNoEdit(mkId(rowIndex)))(vs =>
                  _tdManyStringUrl(
                    mkId(rowIndex),
                    vs.sorted.map { s =>
                      if (s.startsWith("http"))
                        Seq(_urlSpan(s, false))
                      else
                        _str2frags(s)
                    },
                    "items", showAll
                  )
                )

            case "date" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(vs =>
                  _tdManyDate(mkId(rowIndex), vs.sorted, showAll)
                )

            case "big" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(vs =>
                  _tdManyString(mkId(rowIndex), vs.sorted, "num", showAll)
                )

            case _ =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(vs =>
                  _tdManyString(mkId(rowIndex), vs.sorted, "str", showAll)
                )
          }
        }

      case "listDouble" =>
        val origArray = getOrigArray(qr.listNum)
        val array     = qr.listNum(arrayIndex)
        cellType match {
          case "eid" =>
            (rowIndex: Int) =>
              array(rowIndex).fold(
                _tdNoEdit(mkId(rowIndex))
              )(vs =>
                _tdManyRef(
                  mkId(rowIndex),
                  vs,
                  curEntity,
                  setCurEid(false),
                  lockCurEid(false),
                )
              )

          case "ref" if groupEdit =>
            val getCls = getClassLambda(origArray, array)
            (rowIndex: Int) =>
              array(rowIndex).fold(
                _tdNoEdit(mkId(rowIndex))
              )(vs =>
                _tdManyRefGroupEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex),
                  e,
                  vs.map(_.toLong).sorted,
                  (ref: Long) => () => curEntity() = ref,
                  update(origArray, array, rowIndex, "")
                )
              )

          case "ref" if editable =>
            (rowIndex: Int) =>
              _tdManyRefEdit(
                mkId(rowIndex),
                e,
                curEntity,
                array(rowIndex).getOrElse(List.empty[Double]).map(_.toLong).sorted,
                setCurEid(false),
                lockCurEid(false),
                update(origArray, array, rowIndex, "")
              )

          case "ref" =>
            (rowIndex: Int) =>
              array(rowIndex).fold(
                _tdNoEdit(mkId(rowIndex))
              )(vs =>
                _tdManyRef(
                  mkId(rowIndex),
                  vs.sorted,
                  curEntity,
                  setCurEid(false),
                  lockCurEid(false),
                  true
                )
              )

          case _ if editable =>
            val getCls = getClassLambda(origArray, array)
            (rowIndex: Int) =>
              _tdManyDoubleEdit(
                mkId(rowIndex),
                getCls("num", rowIndex),
                e,
                array(rowIndex).getOrElse(List.empty[Double]).sorted,
                update(origArray, array, rowIndex, "num")
              )

          case _ =>
            (rowIndex: Int) =>
              array(rowIndex).fold(
                _tdNoEdit(mkId(rowIndex))
              )(vs =>
                _tdManyDouble(mkId(rowIndex), vs.sorted, showAll)
              )
        }


      // map -------------------------------------------------------------------

      case "mapString" =>
        val origArray = getOrigArray(qr.mapStr)
        val array     = qr.mapStr(arrayIndex)
        val getCls    = getClassLambda(origArray, array)
        if (editable) {
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                _tdMapStrEdit(
                  mkId(rowIndex),
                  getCls("items", rowIndex),
                  e,
                  array(rowIndex).getOrElse(Map.empty[String, String]),
                  update(origArray, array, rowIndex, "items")
                )

            case "date" =>
              (rowIndex: Int) =>
                _tdMapDateEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  array(rowIndex).getOrElse(Map.empty[String, String]),
                  update(origArray, array, rowIndex, "str")
                )

            case _ =>
              (rowIndex: Int) =>
                _tdMapStrOtherEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  array(rowIndex).getOrElse(Map.empty[String, String]),
                  update(origArray, array, rowIndex, "str")
                )
          }
        } else {
          cellType match {
            case "date" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(
                  _tdMapDate(mkId(rowIndex), _)
                )

            case "str" =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(
                  _tdMapStr(mkId(rowIndex), _)
                )

            case _ =>
              (rowIndex: Int) =>
                array(rowIndex).fold(
                  _tdNoEdit(mkId(rowIndex))
                )(
                  _tdMapStrOther(mkId(rowIndex), _)
                )
          }
        }

      case "mapDouble" =>
        val origArray = getOrigArray(qr.mapNum)
        val array     = qr.mapNum(arrayIndex)
        val getCls    = getClassLambda(origArray, array)
        if (editable) {
          rowIndex: Int =>
            _tdMapDoubleEdit(
              mkId(rowIndex),
              getCls("str", rowIndex),
              e,
              array(rowIndex).getOrElse(Map.empty[String, Double]),
              update(origArray, array, rowIndex, "str")
            )
        } else {
          rowIndex: Int =>
            array(rowIndex).fold(
              _tdNoEdit(mkId(rowIndex))
            )(
              _tdMapDouble(mkId(rowIndex), _)
            )
        }
    }
  }
}
