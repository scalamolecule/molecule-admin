package moleculeadmin.client.app.domain.query.data.groupEdit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.Indexes
import moleculeadmin.client.app.domain.query.keyEvents.Paging
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ast.schema.Attr
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{LI, TableCell, TableRow}
import org.scalajs.dom.{Node, NodeList, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class GroupJoin(colIndex: Int, baseNs: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with BodyElements with Paging {

  type keepBooPickleImport_GroupSave = PickleState

  val attrs: Seq[(String, String, Int, String, Seq[(String, String, Boolean, String)])] = {
    if (metaSchema.parts.head.name == "db.part/user") {
      val nss = metaSchema.parts.head.nss
      val ns  = nss.find(_.nameFull == baseNs).get
      ns.attrs.collect {
        case Attr(_, refAttr, refCard, _, _, Some(refNs), _, _, _, _, _, _, _) =>
          val valueAttrs = nss.find(_.nameFull == refNs).get.attrs.map { at =>
            val opt = at.options$ match {
              case None       => ""
              case Some(opts) =>
                if (opts.contains("uniqueIdentity"))
                  "uniqueIdentity"
                else if (opts.contains("uniqueValue"))
                  "uniqueValue"
                else
                  ""
            }
            (at.name, at.tpe, at.enums$.isDefined, opt)
          }
          (ns.name, refAttr, refCard, refNs, valueAttrs)
      }
    } else {
      Nil
    }
  }

  def create(
    ns: String,
    refAttr: String,
    refCard: Int,
    refNs: String,
    valueAttr: String,
    attrType: String,
    isEnum: Boolean,
    value: String
  ): Unit = {
    val qr          = queryCache.queryResult
    val sortCols    = columns.now.filter(_.sortDir.nonEmpty)
    val unfiltered  = filters.now.isEmpty
    val indexBridge = Indexes(qr, sortCols, unfiltered).getIndexBridge
    val eidArray    = qr.num(qr.arrayIndexes(colIndex))
    val eids        = new ListBuffer[Long]
    val lastRow     = actualRowCount
    var i           = 0
    while (i < lastRow) {
      eids += eidArray(indexBridge(i)).get.toLong
      i += 1
    }
    queryWire().createJoins(
      db,
      eids,
      ns,
      refAttr,
      refCard,
      refNs,
      valueAttr,
      attrType,
      isEnum,
      value
    ).call().map {
      case Right(count) =>
        val queryHasRefAttr = columns.now.exists {
          case Col(_, _, `ns`, _, attr, "ref", _, _, _, _, _, _, _, _, _)
            if clean(attr) == refAttr => true
          case _                      => false
        }
        val msg             =
          s"Successfully created $count " +
            s"joins to `:$refNs/$valueAttr` with value `$value`." +
            (if (queryHasRefAttr) "" else
              "\nPlease note that new joins won't show " +
                "since ref attribute is not in current query.")
        println(msg)
        window.alert(msg)
        modelElements.recalc()

      case Left(err) =>
        val msg = s"Error creating joins to $valueAttr:\n$err"
        println(msg)
        window.alert(msg)
    }
  }
}
