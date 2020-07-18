package moleculeadmin.client.app.logic.query.data.cell

import moleculeadmin.client.app.html.query.datatable.BodyElements
import moleculeadmin.client.app.logic.query.QueryState.{checkTogglers, flagTogglers, starTogglers}
import moleculeadmin.client.app.logic.query.data.TypeValidation
import moleculeadmin.client.app.logic.query.data.edit.{TxLambdas, UpdateCardMany, UpdateCardMap, UpdateCardOne}
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.document
import org.scalajs.dom.html.{TableCell, TableRow}
import rx.Ctx

abstract class CellsBase(cols: Seq[Col], qr: QueryResult)
                        (implicit ctx: Ctx.Owner)
  extends TxLambdas(cols, qr) {

  // current entity id for updates of subsequent attributes on each row
  var e                           = 0L
  var curStarToggler : () => Unit = _
  var curFlagToggler : () => Unit = _
  var curCheckToggler: () => Unit = _


  abstract class CellMakerData(colIndex: Int) {

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

    def idBase(colIndex: Int): Int => String =
      (rowIndex: Int) => s"col-${colIndex + 1} row-${rowIndex + 1}"

    val mkId: Int => String = (rowIndex: Int) => {
      val id = idBase(colIndex)(rowIndex)
      starTogglers = starTogglers + (id -> curStarToggler)
      flagTogglers = flagTogglers + (id -> curFlagToggler)
      checkTogglers = checkTogglers + (id -> curCheckToggler)
      id
    }


    // e has to be first within namespace to allow editing
    val isGroupEdit = kind == "edit"
    val editable    = isGroupEdit || isEditable(cols, colIndex, nsAlias, nsFull)
    lazy val showAll = kind == "orig" || kind == "edit"

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
  }
}