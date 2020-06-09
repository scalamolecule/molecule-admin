package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.html.AppElements
import moleculeadmin.client.app.html.query.datatable.HeadElements
import moleculeadmin.client.app.logic.query.KeyEvents
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.groupEdit._
import moleculeadmin.client.app.logic.query.marker.{Toggle, ToggleOffAll}
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.data.FilterFactory
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import org.scalajs.dom.html.{TableCell, TableSection}
import org.scalajs.dom.raw.Node
import org.scalajs.dom.{MouseEvent, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._
import util.client.rx.RxBindings


case class DataTableHead(tableBody: TableSection)(implicit ctx: Ctx.Owner)
  extends RxBindings with ColOps with ModelOps
    with HeadElements with KeyEvents with FilterFactory
    with AppElements {


  def populate(tableHead: TableSection): Rx.Dynamic[Node] = Rx {
    //    println("---- head ----")

    // re-calculate column attr headers on change
    val cols         = columns()
    val colCount     = cols.size
    val attrResolver = ResolveAttrs(columns.now)
    var nss          = Seq.empty[(String, String, Int)]
    val attrCells    = new Array[JsDom.TypedTag[TableCell]](1 + colCount)
    val inputCells   = new Array[JsDom.TypedTag[TableCell]](1 + colCount)

    // Row number column
    attrCells(0) = _rowNumberCell
    inputCells(0) = td(cls := "header")

    var colIndex       = 0
    var col   : Col    = null
    var prevNs: String = ""
    while (colIndex < colCount) {
      // Add namespace or expand colspan if ns is same as for the previous attribute
      col = cols(colIndex)
      prevNs = col.nsFull
      nss match {
        case Nil                        =>
          nss = Seq((col.nsAlias, prevNs, 1))
        case _ if prevNs == nss.last._2 =>
          nss = nss.init :+ (nss.last._1, col.nsFull, nss.last._3 + 1)
        case _                          =>
          nss = nss :+ (col.nsAlias, col.nsFull, 1)
      }

      colIndex += 1
      attrCells(colIndex) = attrCell(col, attrResolver)
      inputCells(colIndex) = inputCell(col)
    }
    val toggleCell = _openCloseQueryBuilder(
      querySelection() == "",
      () => toggleQueryBuilder
    )
    val nsCells    = toggleCell +: nss.map {
      case (nsAlias, _, i) =>
        td(
          cls := "header",
          colspan := i,
          nsAlias
        )
    }
    tableHead.innerHTML = ""
    tableHead.appendChild(tr(nsCells).render)
    tableHead.appendChild(tr(attrCells.toSeq).render)
    tableHead.appendChild(tr(inputCells.toSeq).render)
  }


  def attrCell(
    col: Col,
    attrResolver: ResolveAttrs
  ): JsDom.TypedTag[TableCell] = {
    val Col(colIndex, _, nsAlias, nsFull, attr, _, colType, card, _, _,
    aggrType, expr, sortDir, sortPos, _) = col

    val syncId   = "filter-" + colIndex
    val postfix  = attrResolver.postfix(col)
    val sortable = card == 1 || singleAggrTypes.contains(aggrType)
    val sort     = sortAction(colIndex)
    val editable = isEditable(columns.now, colIndex, nsAlias, nsFull)

    val edit = { _: MouseEvent =>
      modelElements() = toggleEdit(modelElements.now, colIndex, nsFull, attr)
    }

    val save   = { _: MouseEvent => GroupSave(col).save() }
    val cancel = { _: MouseEvent =>
      resetEditColToOrigColCache(colIndex, colType)
      modelElements() = toggleEdit(modelElements.now, colIndex, nsFull, attr)
    }

    val retractEntities = { _: MouseEvent => GroupRetract(col).entities() }
    val retractValues   = { _: MouseEvent => GroupRetract(col).values() }

    val togglers: Seq[MouseEvent => Unit] = if (attr == "e")
      togglerActions(colIndex) else Nil

    val editExprOps                     = EditExprs(col)
    val (editDropdownId, editExprItems) = (
      editExprOps.editDropdownId,
      editExprOps.editExprItems
    )

    if (sortable) {
      val (joinAttrs, joinMaker) = if (attr == "e")
        joinVars(colIndex, nsFull) else (Nil, null)

      _attrHeaderSortable(
        syncId, attr, postfix, expr, sortDir, sortPos, sort,
        editable, edit, save, cancel,
        retractEntities, retractValues,
        togglers,
        joinAttrs, joinMaker,
        editDropdownId, editExprItems
      )

    } else {
      _attrHeader(
        attr, postfix, expr,
        editable, edit, save, cancel,
        retractEntities, retractValues,
        editDropdownId, editExprItems
      )
    }
  }

  private def sortAction(colIndex: Int): MouseEvent => Unit = { e: MouseEvent =>
    if (columns.now.size == 5 &&
      !columns.now.exists(_.colIndex == colIndex)) {
      window.alert("Can sort maximum 5 columns.")
    } else {
      //        println("------------ sort ------------")
      // Let only columns() propagate change
      offset.kill()
      // Show first page with each new sort
      offset() = 0
      columns() = getSortedColumns(
        columns.now, colIndex, e.getModifierState("Shift")
      )
      // update sorting for matching recent if any
      recentQueries = recentQueries.map {
        case q if q.molecule == curMolecule.now =>
          q.copy(colSettings = colSettings(columns.now, filters.now))
        case q                                  => q
      }
    }
  }

  private def togglerActions(colIndex: Int): Seq[MouseEvent => Unit] = {
    Seq(
      { _: MouseEvent => Toggle(tableBody, "star", false, colIndex) },
      { _: MouseEvent => Toggle(tableBody, "flag", false, colIndex) },
      { _: MouseEvent => Toggle(tableBody, "check", false, colIndex) },
      { _: MouseEvent => Toggle(tableBody, "star", true, colIndex) },
      { _: MouseEvent => Toggle(tableBody, "flag", true, colIndex) },
      { _: MouseEvent => Toggle(tableBody, "check", true, colIndex) },
      { _: MouseEvent => ToggleOffAll(tableBody, "star") },
      { _: MouseEvent => ToggleOffAll(tableBody, "flag") },
      { _: MouseEvent => ToggleOffAll(tableBody, "check") },
    )
  }

  private def joinVars(
    colIndex: Int,
    nsFull: String
  ): (Seq[NsData], JoinMaker) = {
    val joins     = GroupJoin(colIndex, nsFull)
    val joinMaker = (
      nsFull: String,
      refAttr: String,
      refCard: Int,
      refNs: String,
      valueAttr: String,
      attrType: String,
      isEnum: Boolean,
      value: String
    ) => joins.create(
      nsFull, refAttr, refCard, refNs, valueAttr, attrType, isEnum, value
    )
    (joins.attrs, joinMaker)
  }


  private def resetEditColToOrigColCache(colIndex: Int, colType: String): Unit = {
    val qr           = cachedQueryResult
    val arrayIndexes = qr.arrayIndexes

    def revert[T](arrays: List[Array[Option[T]]]): List[Array[Option[T]]] = {
      val origColIndex = arrayIndexes(colIndex - 1)
      val editColIndex = arrayIndexes(colIndex)
      arrays.zipWithIndex.map {
        case (_, `editColIndex`) => arrays(origColIndex)
        case (a, _)              => a
      }
    }

    val origQueryResult = colType match {
      case "string"     => qr.copy(str = revert(qr.str))
      case "double"     => qr.copy(num = revert(qr.num))
      case "listString" => qr.copy(listStr = revert(qr.listStr))
      case "listDouble" => qr.copy(listNum = revert(qr.listNum))
      case "mapString"  => qr.copy(mapStr = revert(qr.mapStr))
      case "mapDouble"  => qr.copy(mapNum = revert(qr.mapNum))
    }
    cachedQueryResult = origQueryResult
  }


  def inputCell(col: Col): JsDom.TypedTag[TableCell] = {
    val Col(colIndex, _, _, _, _, attrType,
    colType, _, _, _, _, attrExpr, _, _, _) = col

    val filterId        = "filter-" + colIndex
    val defaultEditExpr = EditExprs(col).defaultEditExpr

    def editCell(): JsDom.TypedTag[TableCell] = {
      val applyLambda = { () =>
        // Only update after pressing Enter (so that paging is not activated)
        if (editCellId.nonEmpty) {
          colType match {
            case "string"     => GroupEdit(col, filterId).string()
            case "double"     => GroupEdit(col, filterId).double()
            case "listString" => GroupEdit(col, filterId).listString()
            case "listDouble" => GroupEdit(col, filterId).listDouble()
            case "mapString"  => GroupEdit(col, filterId).mapString()
            case "mapDouble"  => GroupEdit(col, filterId).mapDouble()
          }
        }
      }
      _attrEditCell(filterId, defaultEditExpr, applyLambda)
    }

    def filterCell(): JsDom.TypedTag[TableCell] = {
      val editable    = if (
        attrType == "datom" ||
          Seq("orig", "t", "tx", "txInstant").contains(attrExpr)
      ) "" else " editable"
      val filterExpr  = filters.now.get(colIndex).fold("")(_.filterExpr)
      val applyFilter = { () =>
        val filterCell = document.getElementById(filterId)
        val newFilterExpr = filterCell.textContent.trim
        // Let only filters() propagate change
        offset.kill()
        offset() = 0
        if (newFilterExpr.isEmpty) {
          _markFilterCell(filterCell, false)
          filterCell.setAttribute("style", "background-color: white")
          filters() = filters.now - colIndex
        } else {
          _markFilterCell(filterCell, true)
          filterCell.setAttribute("style", "background-color: #9de9ff")
          createFilter(col, newFilterExpr, attrType != "String") match {
            case Some(filter) =>
              // Overwrite filter at colIndex
              filters() = filters.now + (colIndex -> filter)
            case None         => filters() = filters.now - colIndex
          }
        }
        // Update grouped values
        groupedColIndexes.recalc()
      }
      _attrFilterCell(editable, filterId, filterExpr, applyFilter)
    }
    if (attrExpr == "edit") editCell() else filterCell()
  }
}
