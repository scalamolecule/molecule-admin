package moleculeadmin.client.app.logic.query.views

import autowire._
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.logic.query.Callbacks
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.html.query.ViewElements
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ops.query.ModelOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import org.scalajs.dom.{Element, document}
import org.scalajs.dom.html.{Span, TableCell}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class Base(implicit ctx: Ctx.Owner)
  extends Callbacks with ViewElements with ModelOps with TreeOps {

  def setCurEid(opt: Boolean): Long => () => Unit = {
    if (opt) {
      (eid: Long) =>
        () => {
          if (eid > 0 && !curEntityLocked.now)
            curEntity() = eid
        }
    } else {
      (eid: Long) =>
        () => {
          if (!curEntityLocked.now)
            curEntity() = eid
        }
    }
  }

  def lockCurEid(opt: Boolean): Long => () => Unit = {
    if (opt) {
      (eid: Long) =>
        () =>
          if (eid > 0) {
            if (curEntityLocked.now) {
              if (eid == curEntity.now)
                curEntityLocked() = false
              curEntity() = eid
            } else {
              curEntityLocked() = true
            }
          }
    } else {
      (eid: Long) =>
        () => {
          if (curEntityLocked.now) {
            if (eid == curEntity.now)
              curEntityLocked() = false
            curEntity() = eid
          } else {
            curEntityLocked() = true
          }
        }
    }
  }

  // Recursively add entity row to view
  def addEntityRows(
    parentElementId: String,
    eid: Long,
    expanded: Boolean,
    level: Int
  ): Unit = {
    queryWireAjax().touchEntity(db, eid).call().foreach { data =>
      val parentElement = document.getElementById(parentElementId)
      if (parentElement != null) {
        parentElement.innerHTML = ""
        data.foreach {
          case (":db/ident", _) => // skip enum idents like :country/US

          case (attr, v) if viewCellTypes.contains(attr) =>
            val cellType    = viewCellTypes(attr)
            val valueCellId = parentElementId + attr + level
            val valueCell   = getValueCell(cellType, valueCellId, v, expanded, level)
            val attrCell    = getAttrCell(attr, cellType, valueCellId, valueCell, expanded)
            parentElement.appendChild(
              tr(
                attrCell,
                valueCell
              ).render
            )

          case (attr, v) =>
          // println(s"Ignore deleted attribute $attr -> `$v`")
        }
      }
    }
  }

  def openTriangle: TypedTag[Span] = span(
    cls := s"oi oi-caret-right",
    fontSize := "10px",
    color := "#8a8a8a",
    paddingRight := 4
  )

  def getValueCell(
    cellType: String,
    valueCellId: String,
    v: String,
    expanded: Boolean,
    level: Int,
    asserted: Boolean = true
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = cellType match {
    case "str" => td(
      id := valueCellId,
      if (asserted) () else cls := "retracted",
      if (v.startsWith("http"))
        a(
          href := v,
          target := "_blank",
          rel := "noopener noreferrer",
          onmouseover := { () => curUrl() = v },
          v
        )
      else
        _str2frags(v)
    )

    case "num" => td(
      id := valueCellId,
      cls := (if (asserted) "num" else "num retracted"),
      v
    )

    case "date" => td(
      id := valueCellId,
      if (asserted) () else cls := "retracted",
      truncateDateStr(v)
    )

    case "ref" => {
      val ref = v.toLong
      if (expanded) {
        if (level < entityLevels) {
          // Expand sub level after current level has rendered
          addEntityRows(valueCellId, v.toLong, true, level + 1)
        }
        td(
          id := valueCellId,
          cls := Rx(
            if (ref == curEntity())
              "eidChosen" + (if (asserted) "" else " retracted")
            else
              "eid" + (if (asserted) "" else " retracted")
          ),
          a(href := "#",
            openTriangle,
            color := "#444",
            ref,
            onmouseover := { () =>
              // Recursively open entity
              addEntityRows(valueCellId, ref, expanded, level + 1)
            }
          )
        )
      } else {
        td(
          id := valueCellId,
          cls := Rx(
            if (ref == curEntity())
              "eidChosen" + (if (asserted) "" else " retracted")
            else
              "eid" + (if (asserted) "" else " retracted")
          ),
          color := "#444",
          ref
        )
      }
    }

    case "enum" => td(
      id := valueCellId,
      if (asserted) () else cls := "retracted",
      v.split('/')(1)
    )

    case "other" => td(
      id := valueCellId,
      if (asserted) () else cls := "retracted",
      v
    )

    case "strSet" => td(
      id := valueCellId,
      if (asserted) () else cls := "retracted",
      expandingList(
        v.split("__~~__").toSeq.sorted.map { s =>
          if (s.startsWith("http"))
            li(
              a(
                href := s, target := "_blank", rel := "noopener noreferrer",
                onmouseover := { () => curUrl() = s },
                s
              )
            )
          else
            li(_str2frags(s))
        },
        true
      )
    )

    case "numSet" => td(
      id := valueCellId,
      cls := (if (asserted) "num" else "num retracted"),
      expandingList(
        v.split("__~~__").toSeq.map(_.toDouble).sorted.map(n => li(n)))
    )

    case "refSet" =>
      if (expanded) {
        val eidsWithIndexes = v.split("__~~__").toSeq.map(_.toLong).zipWithIndex
        if (level < entityLevels) {
          // Expand sub level after current level has rendered
          eidsWithIndexes.foreach {
            case (refId, i) =>
              addEntityRows(valueCellId + "-" + i, refId, true, level + 1)
          }
        }
        td(
          id := valueCellId,
          cls := (if (asserted) "eid" else "retracted"),
          table(
            eidsWithIndexes.map {
              case (refId, i) =>
                val refCellId = valueCellId + "-" + i
                tr(
                  td(
                    id := refCellId,
                    cls := Rx(
                      if (refId == curEntity())
                        "eidChosen" + (if (asserted) "" else " retracted")
                      else
                        "eid" + (if (asserted) "" else " retracted")
                    ),
                    a(
                      href := "#",
                      openTriangle,
                      color := "#444",
                      refId,
                      // Recursively open entity
                      onmouseover := { () =>
                        addEntityRows(refCellId, refId, expanded, level + 1)
                      }
                    )
                  )
                )
            }
          )
        )
      } else {
        val ref = v.toLong
        // Separate row for each value returned from tx lookup
        td(
          id := valueCellId,
          cls := Rx(
            if (ref == curEntity())
              "eidChosen" + (if (asserted) "" else " retracted")
            else
              "eid" + (if (asserted) "" else " retracted")
          ),
          color := "#444",
          ref
        )
      }

    case "dateSet" =>
      td(
        id := valueCellId,
        if (asserted) () else cls := "retracted",
        expandingList(
          v.split("__~~__").toSeq.sorted.map(s => li(truncateDateStr(s)))
        )
      )

    case "enumSet" =>
      if (expanded)
        td(
          id := valueCellId,
          if (asserted) () else cls := "retracted",
          v.split("__~~__").toSeq.sorted
            .flatMap(enumAttr => Seq(span(enumAttr.split('/')(1)), br)).init
        )
      else
        td(
          id := valueCellId,
          if (asserted) () else cls := "retracted",
          v.split('/')(1)
        )

    case "otherSet" => // Boolean, UUID, URI
      if (expanded)
        td(
          id := valueCellId,
          if (asserted) () else cls := "retracted",
          expandingList(
            v.split("__~~__").toSeq.sorted.map(s => li(s))
          )
        )
      else
        td(
          id := valueCellId,
          if (asserted) () else cls := "retracted",
          v
        )

    case map if map.endsWith("Map") =>
      if (expanded) {
        val rawPairs = v.split("__~~__").toSeq
          .map { pair =>
            val List(k, v) = pair.split("@", 2).toList
            (k, v)
          }.sortBy(_._1)

        cellType match {
          case "strMap" => mapCell(valueCellId, rawPairs,
            (v1: String) => td(_str2frags(v1)), asserted)

          case "dateMap" => mapCell(valueCellId, rawPairs,
            (v1: String) => td(truncateDateStr(v1)), asserted)

          case _ => mapCell(valueCellId, rawPairs, (v1: String) => td(v1), asserted)
        }
      } else {
        val List(k, v1) = v.split("@", 2).toList
        td(
          id := valueCellId,
          if (asserted) () else cls := "retracted",
          table(cls := "mapPairs",
            cellType match {
              case "dateMap"                         => mapRow(k, td(truncateDateStr(v1)))
              case "strMap" if v1.startsWith("http") => mapRow(k, td(
                a(href := s"$v", target := "_blank", rel := "noopener noreferrer", v1)
              ))
              case "strMap"                          => mapRow(k, td(_str2frags(v1)))
              case _                                 => mapRow(k, td(v1))
            }
          )
        )
      }

    case _ => td(id := valueCellId, v)
  }


  def getAttrCell(attr: String,
                  cellType: String,
                  valueCellId: String,
                  unexpandedValueCell: TypedTag[TableCell],
                  expand: Boolean
                 ): TypedTag[TableCell] = {
    (if (expand) th() else td()) (
      attr,
      if (curAttrs.contains(attr)) cls := "selectedAttr" else (),
      cellType match {
        case "ref" | "refSet" =>
          onmouseover := { () =>
            val expandedValueCell = document.getElementById(valueCellId)
            expandedValueCell.parentNode
              .replaceChild(unexpandedValueCell.render, expandedValueCell)
          }
        case _                => ()
      }
    )
  }
}
