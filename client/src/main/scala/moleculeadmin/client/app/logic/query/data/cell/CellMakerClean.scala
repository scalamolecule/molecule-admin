package moleculeadmin.client.app.logic.query.data.cell

import moleculeadmin.client.app.css.Mark
import moleculeadmin.client.app.logic.query.QueryState.{curChecks, curEntity, curFlags, curStars}
import moleculeadmin.client.app.logic.query.data.edit.RetractEid
import moleculeadmin.client.app.logic.query.marker.Toggle
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.{TableCell, TableSection}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._

abstract class CellMakerClean(
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult,
)(implicit ctx: Ctx.Owner) extends CellsBase(cols, qr) {

  protected def cellLambda(colIndex: Int): Int => JsDom.TypedTag[TableCell] = {
    For(colIndex).get
  }

  case class For(colIndex: Int) extends CellMakerData(colIndex) {

    def get: Int => JsDom.TypedTag[TableCell] = {

      colType match {

        // card one ------------------------------------------------------------

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
                  if (curStars.contains(eid)) Mark.starOn else Mark.starOff,
                  if (curFlags.contains(eid)) Mark.flagOn else Mark.flagOff,
                  if (curChecks.contains(eid)) Mark.checkOn else Mark.checkOff,
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
                editArray(rowIndex).fold(
                  _tdOneNum(mkId(rowIndex))
                )(_tdOneNum(mkId(rowIndex))(_))
          }


        // card many -----------------------------------------------------------

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
                    editArray(rowIndex).getOrElse(List.empty[String])
                      .sorted.map(s =>
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
                  editArray(rowIndex).getOrElse(List.empty[Double])
                    .map(_.toLong).sorted,
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


        // map -----------------------------------------------------------------

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
}