package moleculeadmin.client.app.logic.query.data.groupEdit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.Indexes
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.client.app.html.query.datatable.HeadElements
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ast.schema.{Attr, Part}
import org.scalajs.dom.window
import rx.Ctx
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global

case class GroupJoin(colIndex: Int, nsFull: String)(implicit val ctx: Ctx.Owner)
  extends HeadElements with Paging {

  val attrs: Seq[NsData] = {
    nsMap(nsFull).attrs.collect {
      case Attr(_, refAttr, refCard, _, _, Some(refNs), _, _, _, _, _, _, _) =>
        val valueAttrs = nsMap(refNs).attrs.map { at =>
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
        (nsFull, refAttr, refCard, refNs, valueAttrs)
    }
  }

  def create(
    nsFull: String,
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
    queryWireAjax().createJoins(
      db,
      eids,
      nsFull,
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
          case Col(_, _, `nsFull`, _, attr, "ref", _, _, _, _, _, _, _, _, _)
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
