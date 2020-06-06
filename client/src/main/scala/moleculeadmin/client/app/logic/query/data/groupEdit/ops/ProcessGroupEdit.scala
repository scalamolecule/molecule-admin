package moleculeadmin.client.app.logic.query.data.groupEdit.ops

import moleculeadmin.client.app.logic.query.QueryState.groupEditId
import moleculeadmin.client.scalafiddle.ScalaFiddle
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

object ProcessGroupEdit {
  var i = 0

  def apply[TransferType](
    colIndexes: Seq[Int],
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    colIndexes.length match {
      case 2  => resolve2(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 3  => resolve3(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 4  => resolve4(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 5  => resolve5(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 6  => resolve6(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 7  => resolve7(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 8  => resolve8(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 9  => resolve9(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 10 => resolve10(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 11 => resolve11(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 12 => resolve12(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 13 => resolve13(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 14 => resolve14(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 15 => resolve15(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 16 => resolve16(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 17 => resolve17(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 18 => resolve18(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 19 => resolve19(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 20 => resolve20(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 21 => resolve21(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 22 => resolve22(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 23 => resolve23(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 24 => resolve24(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 25 => resolve25(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 26 => resolve26(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 27 => resolve27(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 28 => resolve28(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 29 => resolve29(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 30 => resolve30(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 31 => resolve31(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 32 => resolve32(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 33 => resolve33(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 34 => resolve34(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 35 => resolve35(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 36 => resolve36(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 37 => resolve37(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 38 => resolve38(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 39 => resolve39(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 40 => resolve40(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 41 => resolve41(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 42 => resolve42(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 43 => resolve43(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 44 => resolve44(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 45 => resolve45(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 46 => resolve46(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 47 => resolve47(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 48 => resolve48(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 49 => resolve49(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 50 => resolve50(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 51 => resolve51(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 52 => resolve52(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 53 => resolve53(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 54 => resolve54(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 55 => resolve55(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 56 => resolve56(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 57 => resolve57(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 58 => resolve58(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 59 => resolve59(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 60 => resolve60(colType2StringLambdas, scalafiddle, lastRow, resolve)
    }
  }

  def resolve2[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2) = colType2StringLambdas
    scalafiddle.lambda2.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple2(v1(j), v2(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve3[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3) = colType2StringLambdas
    scalafiddle.lambda3.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple3(v1(j), v2(j), v3(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve4[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4) = colType2StringLambdas
    scalafiddle.lambda4.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple4(v1(j), v2(j), v3(j), v4(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve5[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5) = colType2StringLambdas
    scalafiddle.lambda5.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple5(v1(j), v2(j), v3(j), v4(j), v5(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }

  }

  def resolve6[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6) = colType2StringLambdas
    scalafiddle.lambda6.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple6(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve7[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7) = colType2StringLambdas
    scalafiddle.lambda7.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple7(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve8[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8) = colType2StringLambdas
    scalafiddle.lambda8.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple8(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve9[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9) = colType2StringLambdas
    scalafiddle.lambda9.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple9(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve10[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = colType2StringLambdas
    scalafiddle.lambda10.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple10(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve11[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = colType2StringLambdas
    scalafiddle.lambda11.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple11(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve12[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = colType2StringLambdas
    scalafiddle.lambda12.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple12(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve13[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = colType2StringLambdas
    scalafiddle.lambda13.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple13(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve14[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = colType2StringLambdas
    scalafiddle.lambda14.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple14(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve15[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = colType2StringLambdas
    scalafiddle.lambda15.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple15(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve16[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = colType2StringLambdas
    scalafiddle.lambda16.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple16(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve17[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) = colType2StringLambdas
    scalafiddle.lambda17.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple17(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve18[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) = colType2StringLambdas
    scalafiddle.lambda18.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple18(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve19[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) = colType2StringLambdas
    scalafiddle.lambda19.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple19(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve20[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) = colType2StringLambdas
    scalafiddle.lambda20.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve21[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) = colType2StringLambdas
    scalafiddle.lambda21.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple21(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j), v21(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve22[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val Seq(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) = colType2StringLambdas
    scalafiddle.lambda22.foreach { fn =>
      val applyFn = (j: Int) => fn(js.Tuple22(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j), v21(j), v22(j)))
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve23[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    scalafiddle.lambda23.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple3(v21(j), v22(j), v23(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve24[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    scalafiddle.lambda24.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple4(v21(j), v22(j), v23(j), v24(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve25[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    scalafiddle.lambda25.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple5(v21(j), v22(j), v23(j), v24(j), v25(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve26[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    scalafiddle.lambda26.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple6(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve27[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    scalafiddle.lambda27.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple7(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve28[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    scalafiddle.lambda28.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple8(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve29[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    scalafiddle.lambda29.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple9(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve30[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    scalafiddle.lambda30.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple10(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve31[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    scalafiddle.lambda31.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple11(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve32[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    scalafiddle.lambda32.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple12(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve33[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    scalafiddle.lambda33.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple13(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve34[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    scalafiddle.lambda34.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple14(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve35[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    scalafiddle.lambda35.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple15(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve36[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    scalafiddle.lambda36.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple16(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve37[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    scalafiddle.lambda37.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple17(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve38[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    scalafiddle.lambda38.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple18(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve39[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    scalafiddle.lambda39.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple19(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
    }
  }

  def resolve40[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    scalafiddle.lambda40.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve41[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    scalafiddle.lambda41.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple21(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j), v41(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve42[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    scalafiddle.lambda42.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple2(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple22(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j), v41(j), v42(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve43[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    scalafiddle.lambda43.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple3(v41(j), v42(j), v43(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve44[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    scalafiddle.lambda44.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple4(v41(j), v42(j), v43(j), v44(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve45[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    scalafiddle.lambda45.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple5(v41(j), v42(j), v43(j), v44(j), v45(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve46[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    scalafiddle.lambda46.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple6(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve47[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    scalafiddle.lambda47.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple7(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve48[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    scalafiddle.lambda48.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple8(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve49[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    scalafiddle.lambda49.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple9(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve50[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    scalafiddle.lambda50.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple10(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve51[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    scalafiddle.lambda51.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple11(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve52[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    scalafiddle.lambda52.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple12(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve53[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    scalafiddle.lambda53.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple13(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve54[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    scalafiddle.lambda54.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple14(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve55[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    val v55 = colType2StringLambdas(54)
    scalafiddle.lambda55.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple15(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve56[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    val v55 = colType2StringLambdas(54)
    val v56 = colType2StringLambdas(55)
    scalafiddle.lambda56.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple16(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve57[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    val v55 = colType2StringLambdas(54)
    val v56 = colType2StringLambdas(55)
    val v57 = colType2StringLambdas(56)
    scalafiddle.lambda57.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple17(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve58[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    val v55 = colType2StringLambdas(54)
    val v56 = colType2StringLambdas(55)
    val v57 = colType2StringLambdas(56)
    val v58 = colType2StringLambdas(57)
    scalafiddle.lambda58.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple18(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve59[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    val v55 = colType2StringLambdas(54)
    val v56 = colType2StringLambdas(55)
    val v57 = colType2StringLambdas(56)
    val v58 = colType2StringLambdas(57)
    val v59 = colType2StringLambdas(58)
    scalafiddle.lambda59.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple19(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve60[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1  = colType2StringLambdas.head
    val v2  = colType2StringLambdas(1)
    val v3  = colType2StringLambdas(2)
    val v4  = colType2StringLambdas(3)
    val v5  = colType2StringLambdas(4)
    val v6  = colType2StringLambdas(5)
    val v7  = colType2StringLambdas(6)
    val v8  = colType2StringLambdas(7)
    val v9  = colType2StringLambdas(8)
    val v10 = colType2StringLambdas(9)
    val v11 = colType2StringLambdas(10)
    val v12 = colType2StringLambdas(11)
    val v13 = colType2StringLambdas(12)
    val v14 = colType2StringLambdas(13)
    val v15 = colType2StringLambdas(14)
    val v16 = colType2StringLambdas(15)
    val v17 = colType2StringLambdas(16)
    val v18 = colType2StringLambdas(17)
    val v19 = colType2StringLambdas(18)
    val v20 = colType2StringLambdas(19)
    val v21 = colType2StringLambdas(20)
    val v22 = colType2StringLambdas(21)
    val v23 = colType2StringLambdas(22)
    val v24 = colType2StringLambdas(23)
    val v25 = colType2StringLambdas(24)
    val v26 = colType2StringLambdas(25)
    val v27 = colType2StringLambdas(26)
    val v28 = colType2StringLambdas(27)
    val v29 = colType2StringLambdas(28)
    val v30 = colType2StringLambdas(29)
    val v31 = colType2StringLambdas(30)
    val v32 = colType2StringLambdas(31)
    val v33 = colType2StringLambdas(32)
    val v34 = colType2StringLambdas(33)
    val v35 = colType2StringLambdas(34)
    val v36 = colType2StringLambdas(35)
    val v37 = colType2StringLambdas(36)
    val v38 = colType2StringLambdas(37)
    val v39 = colType2StringLambdas(38)
    val v40 = colType2StringLambdas(39)
    val v41 = colType2StringLambdas(40)
    val v42 = colType2StringLambdas(41)
    val v43 = colType2StringLambdas(42)
    val v44 = colType2StringLambdas(43)
    val v45 = colType2StringLambdas(44)
    val v46 = colType2StringLambdas(45)
    val v47 = colType2StringLambdas(46)
    val v48 = colType2StringLambdas(47)
    val v49 = colType2StringLambdas(48)
    val v50 = colType2StringLambdas(49)
    val v51 = colType2StringLambdas(50)
    val v52 = colType2StringLambdas(51)
    val v53 = colType2StringLambdas(52)
    val v54 = colType2StringLambdas(53)
    val v55 = colType2StringLambdas(54)
    val v56 = colType2StringLambdas(55)
    val v57 = colType2StringLambdas(56)
    val v58 = colType2StringLambdas(57)
    val v59 = colType2StringLambdas(58)
    val v60 = colType2StringLambdas(59)
    scalafiddle.lambda60.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j))
        )
      )
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve61[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve62[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve63[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve64[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve65[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve66[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve67[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve68[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve69[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve70[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve71[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve72[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve73[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve74[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve75[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve76[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve77[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve78[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve79[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve80[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve81[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve82[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve83[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve84[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve85[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve86[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve87[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve88[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve89[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve90[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve91[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve92[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve93[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve94[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve95[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve96[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve97[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve98[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve99[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }

  def resolve100[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
  }
}
