package moleculeadmin.client.app.logic.query.data.groupEdit

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.window
import rx.Ctx
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class GroupRetract(col: Col)(implicit val ctx: Ctx.Owner)
  extends Paging with ColOps {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, _,
  opt, enums, _, _, _, _, _) = col

  val qr          = cachedQueryResult
  val indexBridge = cachedIndexBridge
  val valueIndex  = qr.arrayIndexes(colIndex)
  val rowCount    = actualRowCount
  var i           = 0
  var j           = 0

  def entities(): Unit = {
    val eidArray = qr.num(valueIndex)
    val eids     = new Array[Long](rowCount)
    while (i < rowCount) {
      eids(i) = eidArray(indexBridge(i)).get.toLong
      i += 1
    }
    queryWireAjax().retractEntities(db, eids).call().foreach {
      case Right(tx) =>
        println(s"Retracted ${eids.length} entities " +
          s"(see Undo for details of transaction)")
        modelElements.recalc()

      case Left(err) =>
        val msg = s"Error retracting entities:\n" + err
        println(msg)
        window.alert(msg)
    }
  }


  def values(): Unit = {
    val attrFull = s":$nsFull/${clean(attr)}"
    val eidIndex = getEidColIndex(columns.now, colIndex, nsAlias, nsFull)
    val eidArray = qr.num(qr.arrayIndexes(eidIndex))


    def string(): (Int, Future[Either[String, (Long, Long, String)]]) = {
      val enumPrefix = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
      val valueArray = qr.str(valueIndex)
      val data       = new ListBuffer[(Long, Seq[String], Seq[String])]
      while (i < rowCount) {
        j = indexBridge(i)
        valueArray(j).foreach(v =>
          data.+=((eidArray(j).get.toLong, Seq(v), Nil))
        )
        i += 1
      }
      (
        data.length,
        queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call
      )
    }

    def double(): (Int, Future[Either[String, (Long, Long, String)]]) = {
      val valueArray = qr.num(valueIndex)
      val data       = new ListBuffer[(Long, Seq[Double], Seq[Double])]
      while (i < rowCount) {
        j = indexBridge(i)
        valueArray(j).foreach(v =>
          data.+=((eidArray(j).get.toLong, Seq(v), Nil))
        )
        i += 1
      }
      (
        data.length,
        queryWireAjax().updateNum(db, attrFull, attrType, data).call
      )
    }

    def listString(): (Int, Future[Either[String, (Long, Long, String)]]) = {
      val enumPrefix = if (enums.isEmpty) "" else s":$nsAlias.${clean(attr)}/"
      val valueArray = qr.listStr(valueIndex)
      val data       = new ListBuffer[(Long, Seq[String], Seq[String])]
      while (i < rowCount) {
        j = indexBridge(i)
        valueArray(j).foreach(vs =>
          data.+=((eidArray(j).get.toLong, vs, Nil))
        )
        i += 1
      }
      (
        data.length,
        queryWireAjax().updateStr(db, attrFull, attrType, enumPrefix, data).call
      )
    }

    def listDouble(): (Int, Future[Either[String, (Long, Long, String)]]) = {
      val valueArray = qr.listNum(valueIndex)
      val data       = new ListBuffer[(Long, Seq[Double], Seq[Double])]
      while (i < rowCount) {
        j = indexBridge(i)
        valueArray(j).foreach(vs =>
          data.+=((eidArray(j).get.toLong, vs, Nil))
        )
        i += 1
      }
      (
        data.length,
        queryWireAjax().updateNum(db, attrFull, attrType, data).call
      )
    }

    def mapValue[T](valueArray: Array[Option[Map[String, T]]])
    : (Int, Future[Either[String, (Long, Long, String)]]) = {
      val data = new ListBuffer[(Long, Seq[String], Seq[String])]
      while (i < rowCount) {
        j = indexBridge(i)
        valueArray(j).foreach(vs =>
          data.+=(
            (
              eidArray(j).get.toLong,
              vs.map { case (k, v) => k + "@" + v }.toSeq,
              Nil
            )
          )
        )
        i += 1
      }
      (
        data.length,
        queryWireAjax().updateStr(db, attrFull, "String", "", data).call
      )
    }

    val (count, call) = colType match {
      case "string"     => string()
      case "double"     => double()
      case "listString" => listString()
      case "listDouble" => listDouble()
      case "mapString"  => mapValue(qr.mapStr(valueIndex))
      case "mapDouble"  => mapValue(qr.mapNum(valueIndex))
    }

    call.foreach {
      case Right(_) =>
        println(s"Retracted `$attrFull` values for $count entities")
        modelElements.recalc()

      case Left(err) =>
        val msg = s"Error retracting `$attrFull` values:\n" + err
        println(msg)
        window.alert(msg)
    }
  }
}
