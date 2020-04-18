package moleculeadmin.client.app.logic.query.data.groupEdit.ops

import molecule.util.DateHandling
import org.scalajs.dom.html.{LI, TableCell}
import org.scalajs.dom.{Node, NodeList}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.collection.immutable.Map


// Update visible cell values and mark edge color

case class UpdateCells(
  colIndex: Int,
  attrType: String,
  card: Int,
  tableRows: NodeList
) extends DateHandling {

  def cardOne[ColType](
    cellBaseClass: String,
    colValueToNode: ColType => Node
  ): (Int, Option[ColType], Option[ColType]) => Unit = {
    var cells   : NodeList  = null
    var editCell: TableCell = null
    val editColIndex        = colIndex + 1
    val formatValue         = attrType match {
      case "Date" => (optV: Option[ColType]) =>
        optV.fold(Option.empty[ColType])(v =>
          Some(truncateDateStr(v.toString).asInstanceOf[ColType])
        )
      case _      => (optV: Option[ColType]) => optV
    }
    (tableRowIndex: Int,
     oldVopt0: Option[ColType],
     newVopt: Option[ColType]
    ) => {
      val oldVopt = formatValue(oldVopt0)
      cells = tableRows.item(tableRowIndex).childNodes
      editCell = cells.item(editColIndex).asInstanceOf[TableCell]
      editCell.innerHTML = ""
      newVopt match {
        case None if oldVopt != newVopt =>
          editCell.className = s"$cellBaseClass retract"

        case None =>
          editCell.className = cellBaseClass

        case Some(v) if oldVopt.isEmpty =>
          editCell.className = s"$cellBaseClass assert"
          editCell.appendChild(colValueToNode(v))

        case Some(v) if oldVopt != newVopt =>
          editCell.className = s"$cellBaseClass update"
          editCell.appendChild(colValueToNode(v))

        case Some(v) =>
          editCell.className = cellBaseClass
          editCell.appendChild(colValueToNode(v))
      }
    }
  }


  def cardMany[ColType](
    cellBaseClass: String,
    colValueToItems: ColType => Seq[TypedTag[LI]]
  ): (Int, Option[ColType], Option[ColType]) => Unit = {
    var cells       : NodeList  = null
    var editCell    : TableCell = null
    val editColIndex: Int       = colIndex + 1

    val formatValue: Option[ColType] => ColType = attrType match {
      case "Date" => card match {
        case 2 =>
          optV: Option[ColType] =>
            optV.getOrElse[List[String]](Nil)
              .map(v => truncateDateStr(v))
              .asInstanceOf[ColType]
        case 3 =>
          optV: Option[ColType] =>
            optV.getOrElse[Map[String, String]](Map.empty[String, String])
              .map { case (k, v) => k -> truncateDateStr(v) }
              .asInstanceOf[ColType]
      }
      case _      => card match {
        case 2 =>
          optV: Option[ColType] =>
            optV.getOrElse(List.empty[String]).asInstanceOf[ColType]
        case 3 =>
          optV: Option[ColType] =>
            optV.get.asInstanceOf[Map[String, String]]
              .asInstanceOf[ColType]
      }
    }

    (tableRowIndex: Int,
     oldVsOpt: Option[ColType],
     newVsOpt: Option[ColType]
    ) => {
      val oldVs = formatValue(oldVsOpt)
      val newVs = formatValue(newVsOpt)
      cells = tableRows.item(tableRowIndex).childNodes
      editCell = cells.item(editColIndex).asInstanceOf[TableCell]
      editCell.innerHTML = ""
      newVs match {
        // Can be Seq or Map
        case vs if vs.asInstanceOf[Iterable[_]].isEmpty && oldVs != newVs =>
          editCell.className = s"$cellBaseClass retract"

        case vs if vs.asInstanceOf[Iterable[_]].isEmpty =>
          editCell.className = cellBaseClass

        case vs if oldVs.asInstanceOf[Iterable[_]].isEmpty =>
          editCell.className = s"$cellBaseClass assert"
          editCell.appendChild(ul(colValueToItems(vs)).render)

        case vs if oldVs != newVs =>
          editCell.className = s"$cellBaseClass update"
          editCell.appendChild(ul(colValueToItems(vs)).render)

        case vs =>
          editCell.className = cellBaseClass
          editCell.appendChild(ul(colValueToItems(vs)).render)
      }
    }
  }
}
