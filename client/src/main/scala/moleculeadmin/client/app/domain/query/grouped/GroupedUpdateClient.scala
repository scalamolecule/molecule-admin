package moleculeadmin.client.app.domain.query.grouped
import moleculeadmin.client.app.domain.query.data.edit.UpdateClient
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.{TableCell, TableRow}
import org.scalajs.dom.raw.NodeList
import rx.Ctx
import scalatags.JsDom.all.td
import moleculeadmin.client.app.domain.query.QueryState._

// Facade to UpdateClient to avoid redundant code in GroupedData
case class GroupedUpdateClient[T](
  qr: QueryResult,
  valueArray: Array[Option[T]],

  arrayIndex: Int,
  colIndex: Int,

  nsAlias: String,
  nsFull: String,
  attrName: String,
  attrType: String,
  enums: Seq[String],
)(implicit ctx: Ctx.Owner) extends UpdateClient[T](
  columns.now, qr, Array.empty[Option[T]], valueArray, "",
  arrayIndex, colIndex, -1,
  -1, nsAlias, nsFull, attrName, enums
) {

  // Not used by GroupedData
  override def update(
    cellId: String,
    cell: TableCell,
    row: TableRow,
    eid: Long,
    oldVOpt: Option[T],
    isNum: Boolean): Unit = ???


  def updateClient(
    t: Long, tx: Long, txInstant: String,
    tableRows: NodeList,
    newVopt: Option[T],
    oldVopt: Option[T],
    valueColIndex: Int,
    affectedRows: List[Int],
    affectedIndexes: Array[Int]
  ): Unit = updateClient(
    t, tx, txInstant,
    td().render,
    tableRows.item(0).asInstanceOf[TableRow],
    0L,
    newVopt,
    oldVopt,
    valueColIndex,
    affectedRows,
    affectedIndexes
  )
}
