package moleculeadmin.client.app.logic.query.data.edit

import autowire._
import boopickle.Default._
import molecule.util.DateHandling
import moleculeadmin.client.app.logic.query.QueryState.{curT, curTx, curTxInstant, db}
import moleculeadmin.client.app.logic.query.views.Base
import moleculeadmin.client.app.html.query.datatable.BodyElements
import moleculeadmin.client.queryWireAjax
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import org.scalajs.dom.html.TableCell
import rx.Ctx
import scalatags.JsDom
import scala.concurrent.ExecutionContext.Implicits.global


abstract class TxLambdas(
  cols: Seq[Col],
  qr: QueryResult,
)(implicit ctx: Ctx.Owner)
  extends TxLambdasGrid(cols, qr)
    with BodyElements with DateHandling {


  def tLambda(
    cellIdMaker: Int => String,
    arrayIndex: Int,
    colIndex: Int,
  )(implicit ctx: Ctx.Owner): Int => JsDom.TypedTag[TableCell] = {
    val arrayT = qr.num(arrayIndex)
    val tx_    = cols.isDefinedAt(colIndex + 1) && cols(colIndex + 1).kind == "tx"
    val j      = if (tx_) 2 else 1
    val txI_   = cols.isDefinedAt(colIndex + j) && cols(colIndex + j).kind == "txInstant"
    (tx_, txI_) match {
      // t
      case (false, false) =>
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex))) { t =>
            _tdOneT(
              cellIdMaker(rowIndex),
              t.toLong,
              curT,
              { () =>
                val t1 = arrayT(rowIndex).get.toLong
                curT() = t1
                queryWireAjax().getTxFromT(t1).call().foreach(curTx() = _)
              }
            )
          }

      // t + tx
      case (true, false) =>
        val arrayTx = qr.num(arrayIndex + 1) // tx is next
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(t =>
            _tdOneT_tx(
              cellIdMaker(rowIndex),
              t.toLong,
              arrayTx(rowIndex).get.toLong,
              curT,
              { () =>
                curT() = arrayT(rowIndex).get.toLong
                curTx() = arrayTx(rowIndex).get.toLong
              }))

      // t + txInstant
      case (false, true) =>
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + j)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(t =>
            _tdOneT_inst(
              cellIdMaker(rowIndex),
              t.toLong,
              arrayTxInstant(rowIndex).get,
              curT,
              { () =>
                val t1 = arrayT(rowIndex).get.toLong
                curT() = t1
                curTxInstant() = arrayTxInstant(rowIndex).get
                queryWireAjax().getTxFromT(t1).call().foreach(curTx() = _)
              }))

      // t + tx + txInstant
      case (true, true) =>
        val arrayTx             = qr.num(arrayIndex + 1) // tx is next
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + j)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(t1 =>
            _tdOneT_tx_inst(
              cellIdMaker(rowIndex),
              t1.toLong,
              arrayTx(rowIndex).get.toLong,
              arrayTxInstant(rowIndex).get,
              curT,
              { () =>
                curT() = arrayT(rowIndex).get.toLong
                curTx() = arrayTx(rowIndex).get.toLong
                curTxInstant() = arrayTxInstant(rowIndex).get
              }))
    }
  }


  def txLambda(
    cellIdMaker: Int => String,
    arrayIndex: Int,
    colIndex: Int
  )(implicit ctx: Ctx.Owner): Int => JsDom.TypedTag[TableCell] = {
    val arrayTx = qr.num(arrayIndex)
    val t_      = cols.isDefinedAt(colIndex - 1) && cols(colIndex - 1).kind == "t"
    val txI_    = cols.isDefinedAt(colIndex + 1) && cols(colIndex + 1).kind == "txInstant"
    (t_, txI_) match {
      // tx
      case (false, false) =>
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(tx =>
            _tdOneTx(
              cellIdMaker(rowIndex),
              tx.toLong,
              curTx,
              { () =>
                val tx1 = arrayTx(rowIndex).get.toLong
                curTx() = tx1
                queryWireAjax().getTFromTx(tx1).call().foreach(curT() = _)
              }))

      // tx + t
      case (true, false) =>
        val arrayT = qr.num(arrayIndex - 1)
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(tx1 =>
            _tdOneTx_t(
              cellIdMaker(rowIndex),
              tx1.toLong,
              arrayT(rowIndex).get.toLong,
              curTx,
              { () =>
                curTx() = arrayTx(rowIndex).get.toLong
                curT() = arrayT(rowIndex).get.toLong
              }))

      // tx + txInstant
      case (false, true) =>
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + 1)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(tx =>
            _tdOneTx_inst(
              cellIdMaker(rowIndex),
              tx.toLong,
              arrayTxInstant(rowIndex).get,
              curTx,
              { () =>
                val tx1 = arrayTx(rowIndex).get.toLong
                curTx() = tx1
                curTxInstant() = arrayTxInstant(rowIndex).get
                queryWireAjax().getTFromTx(tx1).call().foreach(curT() = _)
              }))

      // tx + t + txInstant
      case (true, true) =>
        val arrayT              = qr.num(arrayIndex - 1)
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + 1)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(tx =>
            _tdOneTx_t_inst(
              cellIdMaker(rowIndex),
              tx.toLong,
              arrayT(rowIndex).get.toLong,
              arrayTxInstant(rowIndex).get,
              curTx,
              { () =>
                curTx() = arrayTx(rowIndex).get.toLong
                curT() = arrayT(rowIndex).get.toLong
                curTxInstant() = arrayTxInstant(rowIndex).get
              }))
    }
  }


  def txInstantLambda(
    cellIdMaker: Int => String,
    arrayIndex: Int,
    colIndex: Int
  )(implicit ctx: Ctx.Owner): Int => JsDom.TypedTag[TableCell] = {
    val arrayTxInstant = qr.str(arrayIndex)
    val tx_            = cols.isDefinedAt(colIndex - 1) &&
      cols(colIndex - 1).kind == "tx"
    val j              = if (tx_) 2 else 1
    val t_             = cols.isDefinedAt(colIndex - j) &&
      cols(colIndex - j).kind == "t"
    (t_, tx_) match {
      // txInstant
      case (false, false) =>
        (rowIndex: Int) =>
          arrayTxInstant(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(d =>
            _tdOneTxInstant(
              cellIdMaker(rowIndex),
              d,
              curTxInstant,
              { () =>
                val txInstant = arrayTxInstant(rowIndex).get
                curTxInstant() = txInstant
                queryWireAjax().getTTxFromTxInstant(db, txInstant).call().foreach {
                  case (t, tx) =>
                    curT() = t
                    curTx() = tx
                }
              }))

      // txInstant + t
      case (true, false) =>
        val arrayIndexT = qr.arrayIndexes(colIndex - j)
        val arrayT      = qr.num(arrayIndexT)
        (rowIndex: Int) =>
          arrayTxInstant(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(d =>
            _tdOneTxInstant_t(
              cellIdMaker(rowIndex),
              d,
              arrayT(rowIndex).get.toLong,
              curTxInstant,
              { () =>
                val txInstant = arrayTxInstant(rowIndex).get
                curTxInstant() = txInstant
                curT() = arrayT(rowIndex).get.toLong
                queryWireAjax().getTTxFromTxInstant(db, txInstant).call().foreach {
                  case (_, tx) => curTx() = tx
                }
              }))

      // txInstant + tx
      case (false, true) =>
        val arrayIndexTx = qr.arrayIndexes(colIndex - 1)
        val arrayTx      = qr.num(arrayIndexTx)
        (rowIndex: Int) =>
          arrayTxInstant(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(d =>
            _tdOneTxInstant_tx(
              cellIdMaker(rowIndex),
              d,
              arrayTx(rowIndex).get.toLong,
              curTxInstant,
              { () =>
                val txInstant = arrayTxInstant(rowIndex).get
                curTxInstant() = txInstant
                curTx() = arrayTx(rowIndex).get.toLong
                queryWireAjax().getTTxFromTxInstant(db, txInstant).call().foreach {
                  case (t, _) => curT() = t
                }
              }))

      // txInstant + t + tx
      case (true, true) =>
        val arrayIndexT  = qr.arrayIndexes(colIndex - j)
        val arrayIndexTx = qr.arrayIndexes(colIndex - 1)
        val arrayT       = qr.num(arrayIndexT)
        val arrayTx      = qr.num(arrayIndexTx)
        (rowIndex: Int) =>
          arrayTxInstant(rowIndex).fold(_tdOneStr(cellIdMaker(rowIndex)))(d =>
            _tdOneTxInstant_t_tx(
              cellIdMaker(rowIndex),
              d,
              arrayT(rowIndex).get.toLong,
              arrayTx(rowIndex).get.toLong,
              curTxInstant,
              { () =>
                curTxInstant() = arrayTxInstant(rowIndex).get
                curT() = arrayT(rowIndex).get.toLong
                curTx() = arrayTx(rowIndex).get.toLong
              }))
    }
  }
}
