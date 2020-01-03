package moleculeadmin.client.app.domain.query.data.edit
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.app.domain.query.QueryState.{db, curT, curTx, curTxInstant}
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import molecule.util.DateHandling
import org.scalajs.dom.html.TableCell
import rx.Ctx
import scalatags.JsDom
import scala.concurrent.ExecutionContext.Implicits.global


abstract class TxLambdas(implicit val ctx: Ctx.Owner)
  extends BodyElements with DateHandling {

  type keepBooPickleImport_TxLambdas = PickleState


  def tLambda(qr: QueryResult,
              arrayIndex: Int,
              cols: Seq[Col],
              colIndex: Int
             )(implicit ctx: Ctx.Owner): Int => JsDom.TypedTag[TableCell] = {
    val arrayT = qr.num(arrayIndex)
    val tx_    = cols.isDefinedAt(colIndex + 1) && cols(colIndex + 1).attrExpr == "tx"
    val j      = if (tx_) 2 else 1
    val txI_   = cols.isDefinedAt(colIndex + j) && cols(colIndex + j).attrExpr == "txInstant"
    (tx_, txI_) match {
      // t
      case (false, false) =>
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdNoEdit)(t1 =>
            _tdOneT(
              t1.toLong,
              curT,
              (t2: Long) => { () =>
                curT() = t2
                queryWire().getTxFromT(t2).call().foreach {
                  tx => curTx() = tx
                }
              }))

      // t + tx
      case (true, false) =>
        val arrayTx = qr.num(arrayIndex + 1) // tx is next
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdNoEdit)(t1 =>
            _tdOneT_tx(
              t1.toLong,
              arrayTx(rowIndex).get.toLong,
              curT,
              (t2: Long, tx2: Long) => { () =>
                curT() = t2
                curTx() = tx2
              }))

      // t + txInstant
      case (false, true) =>
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + j)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdNoEdit)(t1 =>
            _tdOneT_inst(
              t1.toLong,
              arrayTxInstant(rowIndex).get,
              curT,
              (t2: Long, txInstant2: String) => { () =>
                curT() = t2
                curTxInstant() = truncateDateStr(txInstant2)
                queryWire().getTxFromT(t2).call().foreach {
                  tx => curTx() = tx
                }
              }))

      // t + tx + txInstant
      case (true, true) =>
        val arrayTx             = qr.num(arrayIndex + 1) // tx is next
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + j)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayT(rowIndex).fold(_tdNoEdit)(t1 =>
            _tdOneT_tx_inst(
              t1.toLong,
              arrayTx(rowIndex).get.toLong,
              arrayTxInstant(rowIndex).get,
              curT,
              (t2: Long, tx2: Long, txInstant2: String) => { () =>
                curT() = t2
                curTx() = tx2
                curTxInstant() = truncateDateStr(txInstant2)
              }))
    }
  }


  def txLambda(qr: QueryResult,
               arrayIndex: Int,
               cols: Seq[Col],
               colIndex: Int
              )(implicit ctx: Ctx.Owner): Int => JsDom.TypedTag[TableCell] = {
    val arrayTx = qr.num(arrayIndex)
    val t_      = cols.isDefinedAt(colIndex - 1) && cols(colIndex - 1).attrExpr == "t"
    val txI_    = cols.isDefinedAt(colIndex + 1) && cols(colIndex + 1).attrExpr == "txInstant"
    (t_, txI_) match {
      // tx
      case (false, false) =>
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdNoEdit)(tx1 =>
            _tdOneTx(
              tx1.toLong,
              curTx,
              (tx2: Long) => { () =>
                curTx() = tx2
                queryWire().getTFromTx(tx2).call().foreach {
                  t => curT() = t
                }
              }))

      // tx + t
      case (true, false) =>
        val arrayT = qr.num(arrayIndex - 1)
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdNoEdit)(tx1 =>
            _tdOneTx_t(
              tx1.toLong,
              arrayT(rowIndex).get.toLong,
              curTx,
              (tx2: Long, t2: Long) => { () =>
                curTx() = tx2
                curT() = t2
              }))

      // tx + txInstant
      case (false, true) =>
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + 1)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdNoEdit)(tx1 =>
            _tdOneTx_inst(
              tx1.toLong,
              arrayTxInstant(rowIndex).get,
              curTx,
              (tx2: Long, txInstant2: String) => { () =>
                curTx() = tx2
                curTxInstant() = truncateDateStr(txInstant2)
                queryWire().getTFromTx(tx2).call().foreach {
                  t => curT() = t
                }
              }))

      // tx + t + txInstant
      case (true, true) =>
        val arrayT              = qr.num(arrayIndex - 1)
        val arrayIndexTxInstant = qr.arrayIndexes(colIndex + 1)
        val arrayTxInstant      = qr.str(arrayIndexTxInstant)
        (rowIndex: Int) =>
          arrayTx(rowIndex).fold(_tdNoEdit)(tx1 =>
            _tdOneTx_t_inst(
              tx1.toLong,
              arrayT(rowIndex).get.toLong,
              arrayTxInstant(rowIndex).get,
              curTx,
              (tx2: Long, t2: Long, txInstant2: String) => { () =>
                curTx() = tx2
                curT() = t2
                curTxInstant() = truncateDateStr(txInstant2)
              }))
    }
  }


  def txInstantLambda(qr: QueryResult,
                      arrayIndex: Int,
                      cols: Seq[Col],
                      colIndex: Int
                     )(implicit ctx: Ctx.Owner): Int => JsDom.TypedTag[TableCell] = {
    val arrayTxInstant = qr.str(arrayIndex)
    val tx_            = cols.isDefinedAt(colIndex - 1) && cols(colIndex - 1).attrExpr == "tx"
    val j              = if (tx_) 2 else 1
    val t_             = cols.isDefinedAt(colIndex - j) && cols(colIndex - j).attrExpr == "t"
    (t_, tx_) match {
      // txInstant
      case (false, false) =>
        (rowIndex: Int) =>
          arrayTxInstant(rowIndex).fold(_tdNoEdit)(d =>
            _tdOneTxInstant(
              truncateDateStr(d),
              curTxInstant,
              (txInstant2: String) => { () =>
                curTxInstant() = txInstant2
                queryWire().getTTxFromTxInstant(db, txInstant2).call().foreach {
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
          arrayTxInstant(rowIndex).fold(_tdNoEdit)(d =>
            _tdOneTxInstant_t(
              truncateDateStr(d),
              arrayT(rowIndex).get.toLong,
              curTxInstant,
              (txInstant2: String, t2: Long) => { () =>
                curTxInstant() = txInstant2
                curT() = t2
                queryWire().getTTxFromTxInstant(db, txInstant2).call().foreach {
                  case (_, tx) => curTx() = tx
                }
              }))

      // txInstant + tx
      case (false, true) =>
        val arrayIndexTx = qr.arrayIndexes(colIndex - 1)
        val arrayTx      = qr.num(arrayIndexTx)
        (rowIndex: Int) =>
          arrayTxInstant(rowIndex).fold(_tdNoEdit)(d =>
            _tdOneTxInstant_tx(
              truncateDateStr(d),
              arrayTx(rowIndex).get.toLong,
              curTxInstant,
              (txInstant2: String, tx2: Long) => { () =>
                curTxInstant() = txInstant2
                curTx() = tx2
                queryWire().getTTxFromTxInstant(db, txInstant2).call().foreach {
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
          arrayTxInstant(rowIndex).fold(_tdNoEdit)(d =>
            _tdOneTxInstant_t_tx(
              truncateDateStr(d),
              arrayT(rowIndex).get.toLong,
              arrayTx(rowIndex).get.toLong,
              curTxInstant,
              (txInstant2: String, t2: Long, tx2: Long) => { () =>
                curTxInstant() = txInstant2
                curT() = t2
                curTx() = tx2
              }))
    }
  }

}
