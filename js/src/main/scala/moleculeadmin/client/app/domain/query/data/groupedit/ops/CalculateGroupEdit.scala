package moleculeadmin.client.app.domain.query.data.groupedit.ops
import moleculeadmin.client.scalafiddle.ScalaFiddle
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

object CalculateGroupEdit {

  def apply[TransferType](
    colIndexes: Seq[Int],
    toTransferValueLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    var i = 0
    colIndexes.length match {
      case 2 =>
        val Seq(v1, v2) = toTransferValueLambdas
        scalafiddle.lambda2.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 3 =>
        val Seq(v1, v2, v3) = toTransferValueLambdas
        scalafiddle.lambda3.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 4 =>
        val Seq(v1, v2, v3, v4) = toTransferValueLambdas
        scalafiddle.lambda4.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 5 =>
        val Seq(v1, v2, v3, v4, v5) = toTransferValueLambdas
        scalafiddle.lambda5.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 6 =>
        val Seq(v1, v2, v3, v4, v5, v6) = toTransferValueLambdas
        scalafiddle.lambda6.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 7 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7) = toTransferValueLambdas
        scalafiddle.lambda7.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 8 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8) = toTransferValueLambdas
        scalafiddle.lambda8.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 9 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9) = toTransferValueLambdas
        scalafiddle.lambda9.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 10 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = toTransferValueLambdas
        scalafiddle.lambda10.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 11 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = toTransferValueLambdas
        scalafiddle.lambda11.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 12 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = toTransferValueLambdas
        scalafiddle.lambda12.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 13 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = toTransferValueLambdas
        scalafiddle.lambda13.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 14 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = toTransferValueLambdas
        scalafiddle.lambda14.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 15 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = toTransferValueLambdas
        scalafiddle.lambda15.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 16 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = toTransferValueLambdas
        scalafiddle.lambda16.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 17 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) = toTransferValueLambdas
        scalafiddle.lambda17.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 18 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) = toTransferValueLambdas
        scalafiddle.lambda18.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 19 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) = toTransferValueLambdas
        scalafiddle.lambda19.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 20 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) = toTransferValueLambdas
        scalafiddle.lambda20.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 21 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) = toTransferValueLambdas
        scalafiddle.lambda21.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j), v21(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 22 =>
        val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) = toTransferValueLambdas
        scalafiddle.lambda22.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j), v21(j), v22(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }
    }
  }
}
