package moleculeadmin.client.app.domain.query.data

import boopickle.Default._
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.groupedit.{GroupEdit, GroupSave}
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.app.element.query.datatable.HeadElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.data.FilterFactory
import moleculeadmin.shared.ops.query.{ColOps, ModelOps}
import org.scalajs.dom.html.{TableHeaderCell, TableSection}
import org.scalajs.dom.raw.Node
import org.scalajs.dom.{MouseEvent, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class DataTableHead(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with ModelOps
    with HeadElements with KeyEvents with FilterFactory
    with AppElements {

  type keepBooPickleImport = PickleState


  def attrSortCell(col: Col): JsDom.TypedTag[TableHeaderCell] = {
    //      println("attrSortCell...")
    val Col(colIndex, _, nsAlias, nsFull, attr, _, colType, card, _, _,
    aggrType, expr, sortDir, sortPos) = col

    val attrFull = s":$nsFull/$attr"
    val sortable = card == 1 || singleAggrTypes.contains(aggrType)
    val sort     = { e: MouseEvent =>
      if (cachedCols.size == 5 &&
        !cachedCols.exists(_.colIndex == colIndex)) {
        window.alert("Can sort maximum 5 columns.")
      } else {
        //            println("------------ sort ------------")
        // Let only columns() propagate change
        offset.kill()
        // Show first page with each new sort
        offset() = 0
        columns() = getSortedColumns(
          columns.now, colIndex, e.getModifierState("Shift"))
      }
    }
    val editable = isEditable(columns.now, colIndex, nsAlias, nsFull)
    val edit     = { _: MouseEvent =>
      println("edit... " + attr)
      modelElements() = toggleEdit(modelElements.now, colIndex, nsFull, attr)
    }
    val save     = { _: MouseEvent =>
      println(s"Save $attrFull changes...")
      colType match {
        case "string"     => GroupSave(db, col).string()
        case "double"     => GroupSave(db, col).double()
        case "listString" => GroupSave(db, col).listString()
        case "listDouble" => GroupSave(db, col).listDouble()
        case "mapString"  => GroupSave(db, col).mapString()
        case "mapDouble"  => GroupSave(db, col).mapDouble()
      }
    }
    val retract  = { _: MouseEvent =>
      println("retract... " + attr)
      //      modelElements() = toggleEdit(modelElements.now, i, nsFull, attr)
    }
    val cancel   = { _: MouseEvent =>
      println("cancel... " + attr)
      resetEditColToOrigColCache(colIndex, colType)
      // remove edit column from model and redraw
      modelElements() = toggleEdit(modelElements.now, colIndex, nsFull, attr)
    }
    if (sortable) {
      _attrHeaderSortable(attr, card, expr, sortDir, sortPos, sort,
        editable, edit, save, cancel, retract)
    } else {
      _attrHeader(attr, card, expr, editable, edit, save, cancel, retract)
    }
  }

  def resetEditColToOrigColCache(colIndex: Int, colType: String) = {
    val curQueryCache =
      queryCache.now.find(_.modelElements == modelElements.now).get

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
    queryCache() = curQueryCache.copy(queryResult = origQueryResult) +:
      queryCache.now.filterNot(_.modelElements == modelElements.now)
  }


  def attrFilterCell(col: Col): JsDom.TypedTag[TableHeaderCell] = {
    val Col(colIndex, _, _, _, attr, _, colType, card, opt, _, _, attrExpr, _, _) = col

    val filterId  = "filter-" + colIndex
    val cleanAttr = clean(attr)

    def lambdaCell(): JsDom.TypedTag[TableHeaderCell] = {
      def s(i: Int) = "\u00a0" * i
      val lambdaRaw = card match {
        case 1 if opt =>
          s"""$attr match {
             |${s(2)}case Some(v) => Some(v)
             |${s(2)}case None${s(3)} => None
             |}""".stripMargin
        case 1        => s"Some($attr)"
        case 2        => s"$cleanAttr.map(v => v)"
        case 3        =>
          s"""$cleanAttr.map {
             |${s(2)}case (k, v) => (k, v)
             |}""".stripMargin
      }

      val applyLambda = { () =>
        // Only update after pressing Enter (so that paging doesn't initiates)
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
      _attrLambdaCell(filterId, lambdaRaw, applyLambda, skipSpin)
    }

    def filterCell(): JsDom.TypedTag[TableHeaderCell] = {
      val rawFilter = filters.now.get(colIndex).fold("")(_.filterExpr)
      val doFilter  = { () =>
        val filterExpr = document.getElementById(filterId).textContent.trim
        // Let only filters() propagate change
        offset.kill()
        offset() = 0
        if (filterExpr.isEmpty) {
          filters() = filters.now - colIndex
        } else {
          createFilter(col, filterExpr) match {
            case Some(filter) =>
              filters() = filters.now + (colIndex -> filter)
            case None         =>
              filters() = filters.now - colIndex
          }
        }
      }
      _attrFilterCell(filterId, rawFilter, doFilter)
    }

    if (attrExpr == "edit") lambdaCell() else filterCell()
  }


  def populate(tableHead: TableSection): Rx.Dynamic[Node] = Rx {
    //    println("---- head ----")

    // re-calculate column attr headers on change
    val cols        = columns()
    val colCount    = cols.size
    var nss         = Seq.empty[(String, Int)]
    val sortCells   = new Array[JsDom.TypedTag[TableHeaderCell]](colCount)
    val filterCells = new Array[JsDom.TypedTag[TableHeaderCell]](colCount)
    var colIndex    = 0
    var col: Col    = null
    var ns : String = ""
    while (colIndex < colCount) {
      // Add namespace or expand colspan if ns is same as for the previous attribute
      col = cols(colIndex)
      ns = col.nsAlias
      nss match {
        case Nil                    => nss = Seq(ns -> 1)
        case _ if nss.last._1 == ns => nss = nss.init :+ (nss.last._1 -> (nss.last._2 + 1))
        case _                      => nss = nss :+ (ns -> 1)
      }
      sortCells(colIndex) = attrSortCell(col)
      filterCells(colIndex) = attrFilterCell(col)
      colIndex += 1
    }
    val toggleCell = _openCloseQueryBuilder(selection() == "q", () => toggleQuery)
    val nsCells    = nss.map { case (ns, i) => th(colspan := i, ns) }
    tableHead.innerHTML = ""
    tableHead.appendChild(tr(toggleCell +: nsCells).render)
    tableHead.appendChild(tr(_rowNumberCell +: sortCells).render)
    tableHead.appendChild(tr(th() +: filterCells).render)
  }
}
