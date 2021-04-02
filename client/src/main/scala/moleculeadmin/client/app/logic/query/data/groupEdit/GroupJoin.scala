package moleculeadmin.client.app.logic.query.data.groupEdit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.query.datatable.HeadElements
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ast.metaSchema.MetaAttr
import org.scalajs.dom.window
import rx.Ctx
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global

case class GroupJoin(colIndex: Int, nsFull: String)(implicit val ctx: Ctx.Owner)
  extends HeadElements with Paging {

  val attrs: Seq[NsData] = {
    nsMap(nsFull).attrs.collect {
      case MetaAttr(_, refAttr, refCard, _, _, Some(refNs), _, _, _, _, _, _, _) =>
        val valueAttrs = nsMap(refNs).attrs.map { at =>
          val opt = if(at.options.contains("uniqueIdentity"))
            "uniqueIdentity"
          else if(at.options.contains("uniqueValue"))
                "uniqueValue"
          else ""
          (at.name, at.tpe, at.enums.nonEmpty, opt)
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
    val qr          = cachedQueryResult
    val indexBridge = cachedIndexBridge
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
      eids.toSeq,
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
          case Col(_, _, `nsFull`, _, attr, "ref", _, _, _, _, _, _, _, _, _, _)
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
