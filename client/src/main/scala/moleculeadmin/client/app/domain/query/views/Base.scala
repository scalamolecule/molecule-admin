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
import org.scalajs.dom.html.TableCell
import rx.Ctx
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


class Base(implicit val ctx: Ctx.Owner)
  extends Callbacks with ViewElements with ModelOps with TreeOps {
  type keepBooPickleImport_Base = PickleState

  // Recursively add entity row to view
  def addEntityRows(parentElementId: String, eid: Long, txs: Boolean, level: Int): Unit = {
    val viewElement = document.getElementById(parentElementId)
    if (viewElement != null) {
      queryWire().touchEntity(db, eid).call().foreach { data =>
        viewElement.innerHTML = ""
        data.foreach {
          case (":db/ident", _) => // skip enum idents like :country/US
          case (attr, v)        =>
            val cellType   = viewCellTypes(attr)
            val vElementId = parentElementId + attr + level
            val valueCell  = getValueCell(cellType, vElementId, v, txs, level)
            val attrCell   = getAttrCell(attr, cellType, vElementId, valueCell, txs)
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

  def getValueCell(cellType: String,
                   valueCellId: String,
                   v: String,
                   txs: Boolean,
                   level: Int,
                   asserted: Boolean = true
                  ): TypedTag[TableCell] = cellType match {
    case "str" => td(
      if (asserted) () else cls := "retracted",
      if (v.startsWith("http"))
        a(href := s"$v", target := "_blank", rel := "noopener noreferrer", v)
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

    case "ref" => td(
      cls := (if (asserted) "eid" else "eid retracted"),
      id := valueCellId,
      a(href := "#", v,
        onmouseover := { () =>
          // Recursively open entity
          addEntityRows(valueCellId, v.toLong, txs, level + 1)
        })
    )

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
      //          if (txs && level == 0) _str2frags(v) else
      expandingList(
        v.split("__~~__").toSeq.sorted.map { s =>
          if (v.startsWith("http"))
            li(a(href := s"$v", target := "_blank", rel := "noopener noreferrer", v))
          else
            li(_str2frags(s))
        },
        true
      )
    )

    case "numSet" => td(
      cls := (if (asserted) "num" else "num retracted"),
      //          if (txs && level == 0) v else
      expandingList(
        v.split("__~~__").toSeq.map(_.toDouble).sorted.map(n => li(n))))

    case "refSet" => {
      val valueElement = if (txs && level == 0) {
        // Separate row for each value returned from tx lookup
        a(href := "#", v, onmouseover := { () =>
          // Recursively open entity
          addEntityRows(valueCellId, v.toLong, txs, level + 1)
        })
      } else {
        table(
          v.split("__~~__").toSeq.zipWithIndex.map {
            case (eid, i) =>
              val eidElementId = valueCellId + "-" + i
              tr(td(id := eidElementId, cls := "eid",
                a(href := "#", eid, onmouseover := { () =>
                  addEntityRows(eidElementId, eid.toLong, txs, level + 1)
                })))
          }
        )
      }
      td(
        id := valueCellId,
        cls := (if (asserted) "eid" else "retracted"),
        valueElement
      )
    }

    case "dateSet" => td(
      if (asserted) () else cls := "retracted",
      expandingList(
        v.split("__~~__").toSeq.sorted.map(s => li(truncateDateStr(s))))
    )

    case "enumSet" =>
      if (txs && level == 0)
        td(v.split('/')(1))
      else
        td(
          v.split("__~~__").toSeq.sorted
            .flatMap(enumAttr => Seq(span(enumAttr.split('/')(1)), br)).init)

    case "otherSet" => // Boolean, UUID, URI
      if (txs && level == 0)
        td(v)
      else
        td(expandingList(
          v.split("__~~__").toSeq.sorted.map(s => li(s))))

    case map if map.endsWith("Map") =>
      if (txs && level == 0) {
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
      } else {
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

          case _ => mapCell(rawPairs,
            (v1: String) => td(v1), asserted)

        }
      }

    case _ => td(v)
  }


  def getAttrCell(attr: String,
                  cellType: String,
                  valueCellId: String,
                  unexpandedValueCell: TypedTag[TableCell],
                  txs: Boolean
                 ): TypedTag[TableCell] = {
    (if (txs) td() else th()) (
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
