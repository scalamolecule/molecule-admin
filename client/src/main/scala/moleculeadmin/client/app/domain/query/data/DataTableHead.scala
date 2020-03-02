package moleculeadmin.client.app.domain.query.data

import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.groupEdit.{GroupEdit, GroupJoin, GroupSave}
import moleculeadmin.client.app.domain.query.marker.{Toggle, ToggleOffAll}
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.element.query.datatable.HeadElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ast.schema
import moleculeadmin.shared.ast.schema.Attr
import moleculeadmin.shared.ops.query.data.FilterFactory
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import org.scalajs.dom.html.{TableCell, TableHeaderCell, TableSection}
import org.scalajs.dom.raw.Node
import org.scalajs.dom.{MouseEvent, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class DataTableHead(tableBody: TableSection)(implicit ctx: Ctx.Owner)
  extends RxBindings with ColOps with ModelOps
    with HeadElements with KeyEvents with FilterFactory
    with AppElements {


  def attrSortCell(
    col: Col,
    attrResolver: ResolveAttrs
  ): JsDom.TypedTag[TableCell] = {
    //      println("attrSortCell...")
    val Col(colIndex, _, nsAlias, nsFull, attr, _, colType, card, _, _,
    aggrType, expr, sortDir, sortPos, _) = col

    val postfix  = attrResolver.postfix(col)
    val sortable = card == 1 || singleAggrTypes.contains(aggrType)
    val sort     = { e: MouseEvent =>
      if (columns.now.size == 5 &&
        !columns.now.exists(_.colIndex == colIndex)) {
        window.alert("Can sort maximum 5 columns.")
      } else {
        //            println("------------ sort ------------")
        // Let only columns() propagate change
        offset.kill()
        // Show first page with each new sort
        offset() = 0
        columns() = getSortedColumns(
          columns.now, colIndex, e.getModifierState("Shift")
        )
      }
    }
    val editable = isEditable(columns.now, colIndex, nsAlias, nsFull)
    val edit     = { _: MouseEvent =>
      modelElements() = toggleEdit(modelElements.now, colIndex, nsFull, attr)
    }
    val save     = { _: MouseEvent =>
      colType match {
        case "string"     => GroupSave(col).string()
        case "double"     => GroupSave(col).double()
        case "listString" => GroupSave(col).listString()
        case "listDouble" => GroupSave(col).listDouble()
        case "mapString"  => GroupSave(col).mapString()
        case "mapDouble"  => GroupSave(col).mapDouble()
      }
    }
    val retractEntities  = { _: MouseEvent =>
      println("retract entities... " + attr)
      //      modelElements() = toggleEdit(modelElements.now, i, nsFull, attr)
    }
    val retractValues  = { _: MouseEvent =>
      println("retract values... " + attr)
      //      modelElements() = toggleEdit(modelElements.now, i, nsFull, attr)
    }
    val cancel   = { _: MouseEvent =>
      resetEditColToOrigColCache(colIndex, colType)
      modelElements() = toggleEdit(modelElements.now, colIndex, nsFull, attr)
    }

    val togglers: Seq[MouseEvent => Unit] = if (attr == "e") {
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
    } else Nil

    if (sortable) {
      if (attr == "e") {
        val joins     = GroupJoin(colIndex, nsFull)
        val joinMaker = (
          part: String,
          nsFull: String,
          refAttr: String,
          refCard: Int,
          refNs: String,
          valueAttr: String,
          attrType: String,
          isEnum: Boolean,
          value: String
        ) => joins.create(
          part, nsFull, refAttr, refCard, refNs, valueAttr, attrType, isEnum, value
        )

        _attrHeaderSortable(
          attr, postfix, expr, sortDir, sortPos, sort, editable,
          edit, save, cancel, retractEntities, retractValues,
          togglers,
          joins.attrs, joinMaker
        )
      } else {
        _attrHeaderSortable(
          attr, postfix, expr, sortDir, sortPos, sort, editable,
          edit, save, cancel, retractEntities, retractValues,
          togglers,
        )
      }
    } else {
      _attrHeader(
        attr, postfix, expr, editable,
        edit, save, cancel, retractEntities, retractValues)
    }
  }

  def resetEditColToOrigColCache(colIndex: Int, colType: String): Unit = {
    val curQueryCache   = queryCache
    val qr: QueryResult = curQueryCache.queryResult
    val arrayIndexes    = qr.arrayIndexes

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
    queryCache = curQueryCache.copy(queryResult = origQueryResult)
  }


  def attrFilterCell(
    col: Col,
    attrResolver: ResolveAttrs
  ): JsDom.TypedTag[TableCell] = {
    val Col(colIndex, _, _, _, _, _,
    colType, card, opt, _, _, attrExpr, _, _, _) = col

    val filterId = "filter-" + colIndex
    val attr     = attrResolver.postfixed(col)

    def editCell(): JsDom.TypedTag[TableCell] = {
      def s(i: Int) = "\u00a0" * i
      val lambdaRaw = card match {
        case 1 if opt =>
          s"""$attr match {
             |${s(2)}case Some(v) => Some(v)
             |${s(2)}case None${s(3)} => None
             |}""".stripMargin
        case 1        => s"Some($attr)"
        case 2        => s"$attr.map(v => v)"
        case 3        =>
          s"""$attr.map {
             |${s(2)}case (k, v) => (k, v)
             |}""".stripMargin
      }

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
      val skipSpin    = { () => processing() = "" }
      _attrEditCell(filterId, lambdaRaw, applyLambda, skipSpin)
    }

    def filterCell(): JsDom.TypedTag[TableCell] = {
      val filterExpr  = filters.now.get(colIndex).fold("")(_.filterExpr)
      val applyFilter = { () =>
        val filterExpr = document.getElementById(filterId).textContent.trim
        // Let only filters() propagate change
        offset.kill()
        offset() = 0
        if (filterExpr.isEmpty) {
          filters() = filters.now - colIndex
        } else {
          createFilter(col, filterExpr, curStars, curFlags, curChecks) match {
            case Some(filter) => filters() = filters.now + (colIndex -> filter)
            case None         => filters() = filters.now - colIndex
          }
        }
        // Update grouped values
        groupedColIndexes.recalc()
      }
      _attrFilterCell(filterId, filterExpr, applyFilter)
    }

    if (attrExpr == "edit") editCell() else filterCell()
  }


  def populate(tableHead: TableSection): Rx.Dynamic[Node] = Rx {
    //    println("---- head ----")

    // re-calculate column attr headers on change
    val cols           = columns()
    val colCount       = cols.size
    val attrResolver   = ResolveAttrs(columns.now)
    var nss            = Seq.empty[(String, String, Int)]
    val sortCells      = new Array[JsDom.TypedTag[TableCell]](colCount)
    val filterCells    = new Array[JsDom.TypedTag[TableCell]](colCount)
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

      sortCells(colIndex) = attrSortCell(col, attrResolver)
      filterCells(colIndex) = attrFilterCell(col, attrResolver)
      colIndex += 1
    }
    val toggleCell = _openCloseQueryBuilder(
      querySelection() == "", () => toggleQueryBuilder)
    val nsCells    = nss.map { case (nsAlias, _, i) => td(colspan := i, nsAlias) }
    tableHead.innerHTML = ""
    tableHead.appendChild(tr(toggleCell +: nsCells).render)
    tableHead.appendChild(tr(_rowNumberCell +: sortCells).render)
    tableHead.appendChild(tr(td() +: filterCells).render)
  }
}
