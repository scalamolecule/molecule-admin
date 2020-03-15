package moleculeadmin.client.app.domain.query.views

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.Callbacks
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.ViewElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ops.query.ModelOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import org.scalajs.dom.document
import org.scalajs.dom.html.{Span, TableCell}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


class Base(implicit ctx: Ctx.Owner)
  extends Callbacks with ViewElements with ModelOps with TreeOps {
  type keepBooPickleImport_Base = PickleState

  def setCurEid(opt: Boolean): Long => () => Unit = {
    if (opt) {
      (eid: Long) =>
        () => {
          if (eid > 0 && !curEntityLocked)
            curEntity() = eid
        }
    } else {
      (eid: Long) =>
        () => {
          if (!curEntityLocked)
            curEntity() = eid
        }
    }
  }

  def lockCurEid(opt: Boolean): Long => () => Unit = {
    if (opt) {
      (eid: Long) =>
        () =>
          if (eid > 0) {
            if (curEntityLocked) {
              if (eid == curEntity.now)
                curEntityLocked = false
              curEntity() = eid
            } else {
              curEntityLocked = true
            }
          }
    } else {
      (eid: Long) =>
        () => {
          if (curEntityLocked) {
            if (eid == curEntity.now)
              curEntityLocked = false
            curEntity() = eid
          } else {
            curEntityLocked = true
          }
        }
    }
  }

  // Recursively add entity row to view
  def addEntityRows(parentElementId: String, eid: Long, expand: Boolean, level: Int): Unit = {
    val viewElement = document.getElementById(parentElementId)
    if (viewElement != null) {
      queryWire().touchEntity(db, eid).call().foreach { data =>
        viewElement.innerHTML = ""
        data.foreach {
          case (":db/ident", _) => // skip enum idents like :country/US
          case (attr, v)        =>
            val cellType   = viewCellTypes(attr)
            val vElementId = parentElementId + attr + level
            val valueCell  = getValueCell(cellType, vElementId, v, expand, level)
            val attrCell   = getAttrCell(attr, cellType, vElementId, valueCell, expand)
            viewElement.appendChild(
              tr(
                attrCell,
                valueCell
              ).render
            )
        }
      }
    }
  }

  def openTriangle: TypedTag[Span] = span(
    cls := s"oi oi-caret-right",
    fontSize := "10px",
    color := "#8a8a8a",
    paddingRight := 2
  )

  def getValueCell(
    cellType: String,
    valueCellId: String,
    v: String,
    expanding: Boolean,
    level: Int,
    asserted: Boolean = true
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = cellType match {
    case "str" => td(
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
      cls := (if (asserted) "num" else "num retracted"),
      v
    )

    case "date" => td(
      if (asserted) () else cls := "retracted",
      truncateDateStr(v)
    )

    case "ref" => {
      val ref = v.toLong
      if (expanding) {
        td(
          cls := Rx(
            if (ref == curEntity())
              "eidChosen" + (if (asserted) "" else " retracted")
            else
              "eid" + (if (asserted) "" else " retracted")
          ),
          id := valueCellId,
          a(href := "#",
            openTriangle,
            color := "#444",
            ref,
            onmouseover := { () =>
              // Recursively open entity
              addEntityRows(valueCellId, ref, expanding, level + 1)
            }
          )
        )
      } else {
        td(
          cls := Rx(
            if (ref == curEntity())
              "eidChosen" + (if (asserted) "" else " retracted")
            else
              "eid" + (if (asserted) "" else " retracted")
          ),
          id := valueCellId,
          ref
        )
      }


    }

    case "enum" => td(
      if (asserted) () else cls := "retracted",
      v.split('/')(1)
    )

    case "other" => td(
      if (asserted) () else cls := "retracted",
      v
    )

    case "strSet" => td(
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
      cls := (if (asserted) "num" else "num retracted"),
      expandingList(
        v.split("__~~__").toSeq.map(_.toDouble).sorted.map(n => li(n)))
    )

    case "refSet" => {
      if (expanding) {
        td(
          id := valueCellId,
          cls := (if (asserted) "eid" else "retracted"),
          table(
            v.split("__~~__").toSeq.zipWithIndex.map {
              case (v1, i) =>
                val ref          = v1.toLong
                val eidElementId = valueCellId + "-" + i
                tr(
                  td(
                    id := eidElementId,
                    cls := Rx(
                      if (ref == curEntity())
                        "eidChosen" + (if (asserted) "" else " retracted")
                      else
                        "eid" + (if (asserted) "" else " retracted")
                    ),
                    a(
                      href := "#",
                      openTriangle,
                      color := "#444",
                      ref,
                      // Recursively open entity
                      onmouseover := { () =>
                        addEntityRows(eidElementId, ref, expanding, level + 1)
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
          ref
        )
      }
    }

    case "dateSet" =>
      td(
        if (asserted) () else cls := "retracted",
        expandingList(
          v.split("__~~__").toSeq.sorted.map(s => li(truncateDateStr(s)))
        )
      )

    case "enumSet" =>
      if (expanding)
        td(
          if (asserted) () else cls := "retracted",
          v.split("__~~__").toSeq.sorted
            .flatMap(enumAttr => Seq(span(enumAttr.split('/')(1)), br)).init
        )
      else
        td(
          if (asserted) () else cls := "retracted",
          v.split('/')(1)
        )

    case "otherSet" => // Boolean, UUID, URI
      if (expanding)
        td(
          if (asserted) () else cls := "retracted",
          expandingList(
            v.split("__~~__").toSeq.sorted.map(s => li(s))
          )
        )
      else
        td(
          if (asserted) () else cls := "retracted",
          v
        )

    case map if map.endsWith("Map") =>
      if (expanding) {
        val rawPairs = v.split("__~~__").toSeq
          .map { pair =>
            val List(k, v) = pair.split("@", 2).toList
            (k, v)
          }.sortBy(_._1)

        cellType match {
          case "strMap" => mapCell(rawPairs,
            (v1: String) => td(_str2frags(v1)), asserted)

          case "dateMap" => mapCell(rawPairs,
            (v1: String) => td(truncateDateStr(v1)), asserted)

          case _ => mapCell(rawPairs, (v1: String) => td(v1), asserted)
        }
      } else {
        val List(k, v1) = v.split("@", 2).toList
        td(
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

    case _ => td(v)
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
