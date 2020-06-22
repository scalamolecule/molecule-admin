package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.html.query.datatable.BodyElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.edit.{RetractEid, _}
import moleculeadmin.client.app.logic.query.marker.Toggle
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


  protected def cellLambda(colIndex: Int): Int => JsDom.TypedTag[TableCell] = {
    val arrayIndex = qr.arrayIndexes(colIndex)

    val Col(_, related, nsAlias, nsFull, attr, attrType, colType,
    card, _, enums, _, expr, _, _, _, kind) = cols(colIndex)

    lazy val noCount    = expr != "count" && expr != "count-distinct"
    lazy val aggregates = Seq(
      "count", "count-distinct", "sum", "avg", "median", "variance", "stddev"
    )

    val cellType = "" match {
      case _ if aggregates.contains(expr)             => "aggr"
      case _ if attr == "e" && noCount                => "eid"
      case _ if attrType == "ref" && noCount          => "ref"
      case _ if kind == "t"                           => "t"
      case _ if kind == "tx"                          => "tx"
      case _ if kind == "txInstant"                   => "txI"
      case _ if attrType == "String" && enums.isEmpty => "str"
      case _ if attrType == "Date"                    => "date"
      case _ if attrType.startsWith("Big")            => "big"
      case _                                          => ""
    }

    // e has to be first within namespace to allow editing
    val isGroupEdit = kind == "edit"
    val editable    = isGroupEdit || isEditable(cols, colIndex, nsAlias, nsFull)
    lazy val showAll = kind == "orig" || kind == "edit"


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
      editArray: Array[Option[T]],
      rowIndex: Int,
      baseClass: String
    ): () => Unit = {
      val cellId: String  = idBase(colIndex)(rowIndex)
      val isNum : Boolean = Seq(
        "Int", "Long", "ref", "datom", "Float", "Double").contains(attrType)
      val updater         = card match {
        case 1 => UpdateCardOne(
          cols, qr, origArray, editArray, baseClass, colType, colIndex, rowIndex,
          related, nsAlias, nsFull, attr, attrType, enums, kind
        )
        case 2 => UpdateCardMany(
          cols, qr, origArray, editArray, baseClass, colType, rowIndex,
          related, nsAlias, nsFull, attr, attrType, enums, cellType, kind
        )
        case 3 => UpdateCardMap(
          cols, qr, origArray, editArray, baseClass, rowIndex,
          related, nsAlias, nsFull, attr, attrType, enums, kind
        )
      }
      () => {
        val cell: TableCell = document.getElementById(cellId).asInstanceOf[TableCell]
        val row : TableRow  = cell.parentNode.asInstanceOf[TableRow]
        // Unmark row when going out of focus
        row.className = "view"
        val eid: Long = cell.getAttribute("eid").toLong
        updater.update(mkId, cellId, cell, row, eid, isNum, isGroupEdit)
      }
    }

    def getOrigArray[T](arrays: List[Array[Option[T]]]): Array[Option[T]] = {
      if (isGroupEdit)
        arrays(arrayIndex - 1)
      else
        Array.empty[Option[T]]
    }

    def getClassLambda[T](
      origArray: Array[Option[T]],
      editArray: Array[Option[T]]
    ): (String, Int) => String = {
      if (isGroupEdit)
        (baseClass: String, rowIndex: Int) => {
          val oldV = origArray(rowIndex)
          val newV = editArray(rowIndex)
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

    val markRow = (cellId: String) => { () =>
      document.getElementById(cellId)
        .parentNode.asInstanceOf[TableRow].className = "edit"
    }


    colType match {

      // card one --------------------------------------------------------------

      case _ if cellType == "aggr" =>
        val editArray = qr.num(arrayIndex)
        (rowIndex: Int) =>
          editArray(rowIndex).fold(
            _tdOneAggr(mkId(rowIndex))
          )(_tdOneAggr(mkId(rowIndex))(_))

      case "string" =>
        val origArray = getOrigArray(qr.str)
        val editArray = qr.str(arrayIndex)
        if (editable) {
          val getCls = getClassLambda(origArray, editArray)
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                val s = editArray(rowIndex)
                _tdOneStrEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  if (s.getOrElse("").startsWith("http"))
                    _urlSpan(s.get, true)
                  else
                    _optStr2frags(s),
                  update(origArray, editArray, rowIndex, "str"),
                  markRow
                )

            case "date" =>
              (rowIndex: Int) =>
                _tdOneDateEdit(
                  mkId(rowIndex),
                  getCls("date", rowIndex),
                  e,
                  editArray(rowIndex),
                  update(origArray, editArray, rowIndex, "date"),
                  markRow
                )

            case "big" =>
              (rowIndex: Int) =>
                _tdOneNumEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex),
                  e,
                  editArray(rowIndex),
                  update(origArray, editArray, rowIndex, "num"),
                  markRow
                )

            case _ =>
              (rowIndex: Int) =>
                _tdOneEdit(
                  mkId(rowIndex),
                  getCls("", rowIndex),
                  e,
                  editArray(rowIndex),
                  update(origArray, editArray, rowIndex, ""),
                  markRow
                )
          }
        } else {
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdOneStr(mkId(rowIndex))
                )(s =>
                  _tdOneStr(mkId(rowIndex))(
                    if (s.startsWith("http"))
                      _urlSpan(s, false)
                    else
                      _str2frags(s)
                  )
                )

            case "date" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdOneDate(mkId(rowIndex))
                )(d =>
                  _tdOneDate(mkId(rowIndex))(truncateDateStr(d))
                )

            case "big" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdOneNum(mkId(rowIndex))
                )(_tdOneNum(mkId(rowIndex))(_))

            case "txI" => txInstantLambda(mkId, arrayIndex, colIndex)

            case _ =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdOneStr(mkId(rowIndex))
                )(_tdOneStr(mkId(rowIndex))(_))
          }
        }

      case "double" =>
        val origArray = getOrigArray(qr.num)
        val editArray = qr.num(arrayIndex)
        cellType match {
          case "eid" =>
            (rowIndex: Int) =>
              // Set entity id for updates of subsequent attribute values
              e = editArray(rowIndex).fold(0L)(_.toLong)
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
                if (curStars.contains(eid)) mark.starOn else mark.starOff,
                if (curFlags.contains(eid)) mark.flagOn else mark.flagOff,
                if (curChecks.contains(eid)) mark.checkOn else mark.checkOff,
                () => RetractEid(eid),
                curStarToggler,
                curFlagToggler,
                curCheckToggler,
              )

          case "ref" if isGroupEdit =>
            (rowIndex: Int) =>
              _tdOneRefEdit2(
                mkId(rowIndex),
                e,
                editArray(rowIndex),
                update(origArray, editArray, rowIndex, "num"),
                markRow
              )

          case "ref" if editable =>
            (rowIndex: Int) =>
              _tdOneRefEdit(
                mkId(rowIndex),
                e,
                editArray(rowIndex),
                curEntity,
                setCurEid(true),
                lockCurEid(true),
                update(origArray, editArray, rowIndex, "num")
              )

          case _ if editable =>
            val getCls = getClassLambda(origArray, editArray)
            (rowIndex: Int) =>
              _tdOneNumEdit(
                mkId(rowIndex),
                getCls("num", rowIndex),
                e,
                editArray(rowIndex),
                update(origArray, editArray, rowIndex, "num"),
                markRow
              )

          case "ref" =>
            (rowIndex: Int) =>
              editArray(rowIndex).fold(
                _tdOneStr(mkId(rowIndex))
              )(v =>
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
              editArray(rowIndex).fold(
                _tdOneNum(mkId(rowIndex))
              )(_tdOneNum(mkId(rowIndex))(_))
        }


      // card many -------------------------------------------------------------

      case "listString" =>
        val origArray = getOrigArray(qr.listStr)
        val editArray = qr.listStr(arrayIndex)
        if (editable) {
          val getCls = getClassLambda(origArray, editArray)
          cellType match {
            case "str" if enums.isEmpty =>
              (rowIndex: Int) =>
                _tdManyStringItemEdit(
                  mkId(rowIndex),
                  getCls("items", rowIndex),
                  e,
                  editArray(rowIndex).getOrElse(List.empty[String]).sorted.map(s =>
                    if (s.startsWith("http"))
                      Seq(_urlSpan(s, true))
                    else
                      _str2frags(s)
                  ),
                  update(origArray, editArray, rowIndex, "items"),
                  markRow
                )

            case "big" =>
              (rowIndex: Int) =>
                _tdManyStringEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex),
                  e,
                  editArray(rowIndex).getOrElse(List.empty[String]).sorted,
                  update(origArray, editArray, rowIndex, "num"),
                  markRow
                )

            case _ =>
              (rowIndex: Int) =>
                _tdManyStringEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  editArray(rowIndex).getOrElse(List.empty[String]).sorted,
                  update(origArray, editArray, rowIndex, "str"),
                  markRow
                )
          }
        } else {
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "items")
                )(vs =>
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
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "str")
                )(vs =>
                  _tdManyDate(mkId(rowIndex), vs.sorted, showAll)
                )

            case "big" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "num")
                )(vs =>
                  _tdManyString(mkId(rowIndex), vs.sorted, "num", showAll)
                )

            case _ =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "str")
                )(vs =>
                  _tdManyString(mkId(rowIndex), vs.sorted, "str", showAll)
                )
          }
        }

      case "listDouble" =>
        val origArray = getOrigArray(qr.listNum)
        val editArray = qr.listNum(arrayIndex)
        cellType match {
          case "eid" =>
            (rowIndex: Int) =>
              editArray(rowIndex).fold(
                _tdMany(mkId(rowIndex), "num")
              )(vs =>
                _tdManyRef(
                  mkId(rowIndex),
                  vs,
                  curEntity,
                  setCurEid(false),
                  lockCurEid(false),
                )
              )

          case "ref" if isGroupEdit =>
            val getCls = getClassLambda(origArray, editArray)
            (rowIndex: Int) =>
              editArray(rowIndex).fold(
                _tdMany(mkId(rowIndex), "num")
              )(vs =>
                _tdManyRefGroupEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex),
                  e,
                  vs.map(_.toLong).sorted,
                  (ref: Long) => () => curEntity() = ref,
                  update(origArray, editArray, rowIndex, ""),
                  markRow
                )
              )

          case "ref" if editable =>
            (rowIndex: Int) =>
              _tdManyRefEdit(
                mkId(rowIndex),
                e,
                curEntity,
                editArray(rowIndex).getOrElse(List.empty[Double]).map(_.toLong).sorted,
                setCurEid(false),
                lockCurEid(false),
                update(origArray, editArray, rowIndex, ""),
                markRow
              )

          case "ref" =>
            (rowIndex: Int) =>
              editArray(rowIndex).fold(
                _tdMany(mkId(rowIndex), "num")
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
            val getCls = getClassLambda(origArray, editArray)
            (rowIndex: Int) =>
              _tdManyDoubleEdit(
                mkId(rowIndex),
                getCls("num", rowIndex),
                e,
                editArray(rowIndex).getOrElse(List.empty[Double]).sorted,
                update(origArray, editArray, rowIndex, "num"),
                markRow
              )

          case _ =>
            (rowIndex: Int) =>
              editArray(rowIndex).fold(
                _tdMany(mkId(rowIndex), "num")
              )(vs =>
                _tdManyDouble(mkId(rowIndex), vs.sorted, showAll)
              )
        }


      // map -------------------------------------------------------------------

      case "mapString" =>
        val origArray = getOrigArray(qr.mapStr)
        val editArray = qr.mapStr(arrayIndex)
        val getCls    = getClassLambda(origArray, editArray)
        if (editable) {
          cellType match {
            case "str" =>
              (rowIndex: Int) =>
                _tdMapStrEdit(
                  mkId(rowIndex),
                  getCls("items", rowIndex),
                  e,
                  editArray(rowIndex).getOrElse(Map.empty[String, String]),
                  update(origArray, editArray, rowIndex, "items"),
                  markRow
                )

            case "date" =>
              (rowIndex: Int) =>
                _tdMapDateEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  editArray(rowIndex).getOrElse(Map.empty[String, String]),
                  update(origArray, editArray, rowIndex, "str"),
                  markRow
                )

            case _ =>
              (rowIndex: Int) =>
                _tdMapStrOtherEdit(
                  mkId(rowIndex),
                  getCls("str", rowIndex),
                  e,
                  editArray(rowIndex).getOrElse(Map.empty[String, String]),
                  update(origArray, editArray, rowIndex, "str"),
                  markRow
                )
          }
        } else {
          cellType match {
            case "date" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdMapItems(mkId(rowIndex))
                )(
                  _tdMapDate(mkId(rowIndex), _)
                )

            case "str" =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdMapItems(mkId(rowIndex))
                )(
                  _tdMapStr(mkId(rowIndex), _)
                )

            case _ =>
              (rowIndex: Int) =>
                editArray(rowIndex).fold(
                  _tdMapItems(mkId(rowIndex))
                )(
                  _tdMapStrOther(mkId(rowIndex), _)
                )
          }
        }

      case "mapDouble" =>
        val origArray = getOrigArray(qr.mapNum)
        val editArray = qr.mapNum(arrayIndex)
        val getCls    = getClassLambda(origArray, editArray)
        if (editable) {
          rowIndex: Int =>
            _tdMapDoubleEdit(
              mkId(rowIndex),
              getCls("str", rowIndex),
              e,
              editArray(rowIndex).getOrElse(Map.empty[String, Double]),
              update(origArray, editArray, rowIndex, "str"),
              markRow
            )
        } else {
          rowIndex: Int =>
            editArray(rowIndex).fold(
              _tdMapItems(mkId(rowIndex))
            )(
              _tdMapDouble(mkId(rowIndex), _)
            )
        }
    }
  }
}
