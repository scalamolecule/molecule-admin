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


abstract class CellMakerGrid(
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult
)(implicit ctx: Ctx.Owner) extends CellsBase(cols, qr) {

  val dummyGetV = (_: Int) => ""

  protected def cellLambdaGrid(colIndex: Int):
  (Int => String, (Int, String) => JsDom.TypedTag[TableCell]) = {
    For(colIndex).get
  }

  case class For(colIndex: Int) extends CellMakerData(colIndex) {

    def get: (Int => String, (Int, String) => JsDom.TypedTag[TableCell]) = {

      colType match {

        // card one ------------------------------------------------------------

        case _ if cellType == "aggr" =>
          val editArray = qr.num(arrayIndex)
          val getV      = (rowIndex: Int) => editArray(rowIndex).fold("")(_.toString)
          (getV, (rowIndex: Int, prio: String) =>
            editArray(rowIndex).fold(
              _tdOneAggr(mkId(rowIndex), prio)
            )(_tdOneAggr(mkId(rowIndex), prio)(_)))

        case "string" =>
          val origArray = getOrigArray(qr.str)
          val editArray = qr.str(arrayIndex)
          val getV      = (rowIndex: Int) => editArray(rowIndex).getOrElse("")
          if (editable) {
            val getCls = getClassLambda(origArray, editArray)
            cellType match {
              case "str" =>
                (
                  getV,
                  (rowIndex: Int, prio: String) => {
                    val s = editArray(rowIndex)
                    _tdOneStrEdit(
                      mkId(rowIndex),
                      getCls("str", rowIndex) + prio,
                      e,
                      if (s.getOrElse("").startsWith("http"))
                        _urlSpan(s.get, true)
                      else
                        _optStr2frags(s),
                      update(origArray, editArray, rowIndex, "str" + prio),
                      markRow
                    )
                  }
                )

              case "date" =>
                (getV, (rowIndex: Int, prio: String) =>
                  _tdOneDateEdit(
                    mkId(rowIndex),
                    getCls("date", rowIndex) + prio,
                    e,
                    editArray(rowIndex),
                    update(origArray, editArray, rowIndex, "date" + prio),
                    markRow
                  ))

              case "big" =>
                (getV, (rowIndex: Int, prio: String) =>
                  _tdOneNumEdit(
                    mkId(rowIndex),
                    getCls("num", rowIndex) + prio,
                    e,
                    editArray(rowIndex),
                    update(origArray, editArray, rowIndex, "num" + prio),
                    markRow
                  ))

              case _ =>
                (getV, (rowIndex: Int, prio: String) =>
                  _tdOneEdit(
                    mkId(rowIndex),
                    getCls("", rowIndex) + prio,
                    e,
                    editArray(rowIndex),
                    update(origArray, editArray, rowIndex, prio),
                    markRow
                  ))
            }
          } else {
            cellType match {
              case "str" =>
                (getV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdOneStr(mkId(rowIndex), prio)
                  )(s =>
                    _tdOneStr(mkId(rowIndex), prio)(
                      if (s.startsWith("http"))
                        _urlSpan(s, false)
                      else
                        _str2frags(s)
                    )
                  ))

              case "date" =>
                (getV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdOneDate(mkId(rowIndex), prio)
                  )(d =>
                    _tdOneDate(mkId(rowIndex), prio)(truncateDateStr(d))
                  ))

              case "big" =>
                (getV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdOneNum(mkId(rowIndex), prio)
                  )(_tdOneNum(mkId(rowIndex), prio)(_)))

              case "txI" => (getV, txInstantLambdaNested(mkId, arrayIndex, colIndex))

              case _ =>
                (getV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdOneStr(mkId(rowIndex), prio)
                  )(_tdOneStr(mkId(rowIndex), prio)(_)))
            }
          }

        case "double" =>
          val origArray = getOrigArray(qr.num)
          val editArray = qr.num(arrayIndex)
          val getV      = (rowIndex: Int) => editArray(rowIndex).fold("")(_.toString)
          cellType match {
            case "eid" =>
              (
                getV,
                (rowIndex: Int, prio: String) => {
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
                    prio
                  )
                }
              )

            case "ref" if isGroupEdit =>
              (getV, (rowIndex: Int, prio: String) =>
                _tdOneRefEdit2(
                  mkId(rowIndex),
                  e,
                  editArray(rowIndex),
                  update(origArray, editArray, rowIndex, "num" + prio),
                  markRow,
                  prio
                ))

            case "ref" if editable =>
              (getV, (rowIndex: Int, prio: String) =>
                _tdOneRefEdit(
                  mkId(rowIndex),
                  e,
                  editArray(rowIndex),
                  curEntity,
                  setCurEid(true),
                  lockCurEid(true),
                  update(origArray, editArray, rowIndex, "num" + prio),
                  prio
                ))

            case _ if editable =>
              val getCls = getClassLambda(origArray, editArray)
              (getV, (rowIndex: Int, prio: String) =>
                _tdOneNumEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex) + prio,
                  e,
                  editArray(rowIndex),
                  update(origArray, editArray, rowIndex, "num" + prio),
                  markRow
                ))

            case "ref" =>
              (getV, (rowIndex: Int, prio: String) =>
                editArray(rowIndex).fold(
                  _tdOneStr(mkId(rowIndex), prio)
                )(v =>
                  _tdOneRef(
                    mkId(rowIndex),
                    v.toLong,
                    curEntity,
                    setCurEid(false),
                    lockCurEid(false),
                    prio
                  )
                ))

            case "t"  => (getV, tLambdaNested(mkId, arrayIndex, colIndex))
            case "tx" => (getV, txLambdaNested(mkId, arrayIndex, colIndex))

            case _ =>
              (getV, (rowIndex: Int, prio: String) =>
                editArray(rowIndex).fold(
                  _tdOneNum(mkId(rowIndex), prio)
                )(_tdOneNum(mkId(rowIndex), prio)(_)))
          }


        // card many -----------------------------------------------------------

        case "listString" =>
          val origArray = getOrigArray(qr.listStr)
          val editArray = qr.listStr(arrayIndex)
          if (editable) {
            val getCls = getClassLambda(origArray, editArray)
            cellType match {
              case "str" if enums.isEmpty =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  _tdManyStringItemEdit(
                    mkId(rowIndex),
                    getCls("items", rowIndex) + prio,
                    e,
                    editArray(rowIndex).getOrElse(List.empty[String])
                      .sorted.map(s =>
                      if (s.startsWith("http"))
                        Seq(_urlSpan(s, true))
                      else
                        _str2frags(s)
                    ),
                    update(origArray, editArray, rowIndex, "items" + prio),
                    markRow
                  ))

              case "big" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  _tdManyStringEdit(
                    mkId(rowIndex),
                    getCls("num", rowIndex) + prio,
                    e,
                    editArray(rowIndex).getOrElse(List.empty[String]).sorted,
                    update(origArray, editArray, rowIndex, "num" + prio),
                    markRow
                  ))

              case _ =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  _tdManyStringEdit(
                    mkId(rowIndex),
                    getCls("str", rowIndex) + prio,
                    e,
                    editArray(rowIndex).getOrElse(List.empty[String]).sorted,
                    update(origArray, editArray, rowIndex, "str" + prio),
                    markRow
                  ))
            }
          } else {
            cellType match {
              case "str" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMany(mkId(rowIndex), "items" + prio)
                  )(vs =>
                    _tdManyStringUrl(
                      mkId(rowIndex),
                      vs.sorted.map { s =>
                        if (s.startsWith("http"))
                          Seq(_urlSpan(s, false))
                        else
                          _str2frags(s)
                      },
                      "items" + prio,
                      showAll
                    )
                  ))

              case "date" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMany(mkId(rowIndex), "str" + prio)
                  )(vs =>
                    _tdManyDate(mkId(rowIndex), vs.sorted, showAll, prio)
                  ))

              case "big" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMany(mkId(rowIndex), "num" + prio)
                  )(vs =>
                    _tdManyString(
                      mkId(rowIndex), vs.sorted, "num" + prio, showAll
                    )
                  ))

              case _ =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMany(mkId(rowIndex), "str" + prio)
                  )(vs =>
                    _tdManyString(
                      mkId(rowIndex), vs.sorted, "str" + prio, showAll
                    )
                  ))
            }
          }

        case "listDouble" =>
          val origArray = getOrigArray(qr.listNum)
          val editArray = qr.listNum(arrayIndex)
          cellType match {
            case "eid" =>
              (dummyGetV, (rowIndex: Int, prio: String) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "num" + prio)
                )(vs =>
                  _tdManyRef(
                    mkId(rowIndex),
                    vs,
                    curEntity,
                    setCurEid(false),
                    lockCurEid(false),
                    prio = prio
                  )
                ))

            case "ref" if isGroupEdit =>
              val getCls = getClassLambda(origArray, editArray)
              (dummyGetV, (rowIndex: Int, prio: String) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "num" + prio)
                )(vs =>
                  _tdManyRefGroupEdit(
                    mkId(rowIndex),
                    getCls("num", rowIndex) + prio,
                    e,
                    vs.map(_.toLong).sorted,
                    (ref: Long) => () => curEntity() = ref,
                    update(origArray, editArray, rowIndex,  prio),
                    markRow
                  )
                ))

            case "ref" if editable =>
              (dummyGetV, (rowIndex: Int, prio: String) =>
                _tdManyRefEdit(
                  mkId(rowIndex),
                  e,
                  curEntity,
                  editArray(rowIndex).getOrElse(List.empty[Double])
                    .map(_.toLong).sorted,
                  setCurEid(false),
                  lockCurEid(false),
                  update(origArray, editArray, rowIndex, prio),
                  markRow,
                  prio
                ))

            case "ref" =>
              (dummyGetV, (rowIndex: Int, prio: String) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "num" + prio)
                )(vs =>
                  _tdManyRef(
                    mkId(rowIndex),
                    vs.sorted,
                    curEntity,
                    setCurEid(false),
                    lockCurEid(false),
                    true,
                    prio = prio
                  )
                ))

            case _ if editable =>
              val getCls = getClassLambda(origArray, editArray)
              (dummyGetV, (rowIndex: Int, prio: String) =>
                _tdManyDoubleEdit(
                  mkId(rowIndex),
                  getCls("num", rowIndex) + prio,
                  e,
                  editArray(rowIndex).getOrElse(List.empty[Double]).sorted,
                  update(origArray, editArray, rowIndex, "num" + prio),
                  markRow
                ))

            case _ =>
              (dummyGetV, (rowIndex: Int, prio: String) =>
                editArray(rowIndex).fold(
                  _tdMany(mkId(rowIndex), "num" + prio)
                )(vs =>
                  _tdManyDouble(mkId(rowIndex), vs.sorted, showAll, prio)
                ))
          }


        // map -----------------------------------------------------------------

        case "mapString" =>
          val origArray = getOrigArray(qr.mapStr)
          val editArray = qr.mapStr(arrayIndex)
          val getCls    = getClassLambda(origArray, editArray)
          if (editable) {
            cellType match {
              case "str" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  _tdMapStrEdit(
                    mkId(rowIndex),
                    getCls("items", rowIndex) + prio,
                    e,
                    editArray(rowIndex).getOrElse(Map.empty[String, String]),
                    update(origArray, editArray, rowIndex, "items" + prio),
                    markRow
                  ))

              case "date" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  _tdMapDateEdit(
                    mkId(rowIndex),
                    getCls("str", rowIndex) + prio,
                    e,
                    editArray(rowIndex).getOrElse(Map.empty[String, String]),
                    update(origArray, editArray, rowIndex, "str" + prio),
                    markRow
                  ))

              case _ =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  _tdMapStrOtherEdit(
                    mkId(rowIndex),
                    getCls("str", rowIndex) + prio,
                    e,
                    editArray(rowIndex).getOrElse(Map.empty[String, String]),
                    update(origArray, editArray, rowIndex, "str" + prio),
                    markRow
                  ))
            }
          } else {
            cellType match {
              case "date" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMapItems(mkId(rowIndex), prio)
                  )(
                    _tdMapDate(mkId(rowIndex), _, prio)
                  ))

              case "str" =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMapItems(mkId(rowIndex), prio)
                  )(
                    _tdMapStr(mkId(rowIndex), _, prio)
                  ))

              case _ =>
                (dummyGetV, (rowIndex: Int, prio: String) =>
                  editArray(rowIndex).fold(
                    _tdMapItems(mkId(rowIndex), prio)
                  )(
                    _tdMapStrOther(mkId(rowIndex), _, prio)
                  ))
            }
          }

        case "mapDouble" =>
          val origArray = getOrigArray(qr.mapNum)
          val editArray = qr.mapNum(arrayIndex)
          val getCls    = getClassLambda(origArray, editArray)
          if (editable) {
            (dummyGetV, (rowIndex: Int, prio: String) =>
              _tdMapDoubleEdit(
                mkId(rowIndex),
                getCls("str", rowIndex) + prio,
                e,
                editArray(rowIndex).getOrElse(Map.empty[String, Double]),
                update(origArray, editArray, rowIndex, "str" + prio),
                markRow
              ))
          } else {
            (dummyGetV, (rowIndex: Int, prio: String) =>
              editArray(rowIndex).fold(
                _tdMapItems(mkId(rowIndex), prio)
              )(
                _tdMapDouble(mkId(rowIndex), _, prio)
              ))
          }
      }
    }
  }
}
