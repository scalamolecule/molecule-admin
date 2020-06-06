package moleculeadmin.client.app.logic.query.data.groupEdit.ops

import moleculeadmin.client.app.logic.query.QueryState.groupEditId
import moleculeadmin.client.scalafiddle.ScalaFiddle
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

object ProcessGroupEdit {

  def apply[TransferType](
    colIndexes: Seq[Int],
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    colIndexes.length match {
      case 2   => resolve2(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 3   => resolve3(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 4   => resolve4(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 5   => resolve5(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 6   => resolve6(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 7   => resolve7(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 8   => resolve8(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 9   => resolve9(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 10  => resolve10(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 11  => resolve11(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 12  => resolve12(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 13  => resolve13(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 14  => resolve14(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 15  => resolve15(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 16  => resolve16(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 17  => resolve17(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 18  => resolve18(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 19  => resolve19(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 20  => resolve20(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 21  => resolve21(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 22  => resolve22(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 23  => resolve23(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 24  => resolve24(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 25  => resolve25(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 26  => resolve26(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 27  => resolve27(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 28  => resolve28(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 29  => resolve29(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 30  => resolve30(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 31  => resolve31(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 32  => resolve32(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 33  => resolve33(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 34  => resolve34(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 35  => resolve35(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 36  => resolve36(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 37  => resolve37(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 38  => resolve38(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 39  => resolve39(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 40  => resolve40(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 41  => resolve41(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 42  => resolve42(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 43  => resolve43(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 44  => resolve44(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 45  => resolve45(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 46  => resolve46(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 47  => resolve47(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 48  => resolve48(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 49  => resolve49(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 50  => resolve50(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 51  => resolve51(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 52  => resolve52(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 53  => resolve53(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 54  => resolve54(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 55  => resolve55(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 56  => resolve56(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 57  => resolve57(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 58  => resolve58(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 59  => resolve59(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 60  => resolve60(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 61  => resolve61(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 62  => resolve62(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 63  => resolve63(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 64  => resolve64(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 65  => resolve65(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 66  => resolve66(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 67  => resolve67(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 68  => resolve68(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 69  => resolve69(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 70  => resolve70(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 71  => resolve71(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 72  => resolve72(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 73  => resolve73(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 74  => resolve74(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 75  => resolve75(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 76  => resolve76(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 77  => resolve77(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 78  => resolve78(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 79  => resolve79(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 80  => resolve80(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 81  => resolve81(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 82  => resolve82(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 83  => resolve83(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 84  => resolve84(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 85  => resolve85(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 86  => resolve86(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 87  => resolve87(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 88  => resolve88(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 89  => resolve89(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 90  => resolve90(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 91  => resolve91(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 92  => resolve92(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 93  => resolve93(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 94  => resolve94(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 95  => resolve95(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 96  => resolve96(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 97  => resolve97(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 98  => resolve98(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 99  => resolve99(colType2StringLambdas, scalafiddle, lastRow, resolve)
      case 100 => resolve100(colType2StringLambdas, scalafiddle, lastRow, resolve)
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
      var i = 0
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
    val v61 = colType2StringLambdas(60)
    scalafiddle.lambda61.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple21(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j), v61(j)),
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve62[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    scalafiddle.lambda62.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple3(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple22(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j), v61(j), v62(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve63[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    scalafiddle.lambda63.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple3(v61(j), v62(j), v63(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve64[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    scalafiddle.lambda64.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple4(v61(j), v62(j), v63(j), v64(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve65[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    scalafiddle.lambda65.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple5(v61(j), v62(j), v63(j), v64(j), v65(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve66[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    scalafiddle.lambda66.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple6(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve67[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    scalafiddle.lambda67.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple7(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve68[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    scalafiddle.lambda68.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple8(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve69[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    scalafiddle.lambda69.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple9(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve70[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    scalafiddle.lambda70.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple10(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve71[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    scalafiddle.lambda71.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple11(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve72[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    scalafiddle.lambda72.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple12(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve73[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    scalafiddle.lambda73.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple13(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve74[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    scalafiddle.lambda74.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple14(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve75[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    scalafiddle.lambda75.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple15(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve76[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    scalafiddle.lambda76.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple16(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve77[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    scalafiddle.lambda77.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple17(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve78[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    scalafiddle.lambda78.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple18(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve79[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    scalafiddle.lambda79.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple19(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve80[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    scalafiddle.lambda80.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve81[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    scalafiddle.lambda81.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple21(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j), v81(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve82[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    scalafiddle.lambda82.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple4(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple22(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j), v81(j), v82(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve83[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    scalafiddle.lambda83.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple3(v81(j), v82(j), v83(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve84[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    scalafiddle.lambda84.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple4(v81(j), v82(j), v83(j), v84(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve85[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    scalafiddle.lambda85.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple5(v81(j), v82(j), v83(j), v84(j), v85(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve86[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    scalafiddle.lambda86.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple6(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve87[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    scalafiddle.lambda87.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple7(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve88[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    scalafiddle.lambda88.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple8(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve89[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    scalafiddle.lambda89.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple9(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve90[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    scalafiddle.lambda90.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple10(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve91[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    scalafiddle.lambda91.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple11(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve92[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    scalafiddle.lambda92.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple12(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve93[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    scalafiddle.lambda93.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple13(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve94[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    val v94 = colType2StringLambdas(93)
    scalafiddle.lambda94.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple14(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve95[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    val v94 = colType2StringLambdas(93)
    val v95 = colType2StringLambdas(94)
    scalafiddle.lambda95.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple15(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j), v95(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve96[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    val v94 = colType2StringLambdas(93)
    val v95 = colType2StringLambdas(94)
    val v96 = colType2StringLambdas(95)
    scalafiddle.lambda96.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple16(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j), v95(j), v96(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve97[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    val v94 = colType2StringLambdas(93)
    val v95 = colType2StringLambdas(94)
    val v96 = colType2StringLambdas(95)
    val v97 = colType2StringLambdas(96)
    scalafiddle.lambda97.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple17(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j), v95(j), v96(j), v97(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve98[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    val v94 = colType2StringLambdas(93)
    val v95 = colType2StringLambdas(94)
    val v96 = colType2StringLambdas(95)
    val v97 = colType2StringLambdas(96)
    val v98 = colType2StringLambdas(97)
    scalafiddle.lambda98.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple18(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j), v95(j), v96(j), v97(j), v98(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve99[TransferType](
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
    val v61 = colType2StringLambdas(60)
    val v62 = colType2StringLambdas(61)
    val v63 = colType2StringLambdas(62)
    val v64 = colType2StringLambdas(63)
    val v65 = colType2StringLambdas(64)
    val v66 = colType2StringLambdas(65)
    val v67 = colType2StringLambdas(66)
    val v68 = colType2StringLambdas(67)
    val v69 = colType2StringLambdas(68)
    val v70 = colType2StringLambdas(69)
    val v71 = colType2StringLambdas(70)
    val v72 = colType2StringLambdas(71)
    val v73 = colType2StringLambdas(72)
    val v74 = colType2StringLambdas(73)
    val v75 = colType2StringLambdas(74)
    val v76 = colType2StringLambdas(75)
    val v77 = colType2StringLambdas(76)
    val v78 = colType2StringLambdas(77)
    val v79 = colType2StringLambdas(78)
    val v80 = colType2StringLambdas(79)
    val v81 = colType2StringLambdas(80)
    val v82 = colType2StringLambdas(81)
    val v83 = colType2StringLambdas(82)
    val v84 = colType2StringLambdas(83)
    val v85 = colType2StringLambdas(84)
    val v86 = colType2StringLambdas(85)
    val v87 = colType2StringLambdas(86)
    val v88 = colType2StringLambdas(87)
    val v89 = colType2StringLambdas(88)
    val v90 = colType2StringLambdas(89)
    val v91 = colType2StringLambdas(90)
    val v92 = colType2StringLambdas(91)
    val v93 = colType2StringLambdas(92)
    val v94 = colType2StringLambdas(93)
    val v95 = colType2StringLambdas(94)
    val v96 = colType2StringLambdas(95)
    val v97 = colType2StringLambdas(96)
    val v98 = colType2StringLambdas(97)
    val v99 = colType2StringLambdas(98)
    scalafiddle.lambda99.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple19(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j), v95(j), v96(j), v97(j), v98(j), v99(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }

  def resolve100[TransferType](
    colType2StringLambdas: Seq[Int => Any],
    scalafiddle: ScalaFiddle[TransferType],
    lastRow: Int,
    resolve: (Int, Int => js.Tuple2[TransferType, String]) => Unit
  ): Unit = {
    val v1   = colType2StringLambdas.head
    val v2   = colType2StringLambdas(1)
    val v3   = colType2StringLambdas(2)
    val v4   = colType2StringLambdas(3)
    val v5   = colType2StringLambdas(4)
    val v6   = colType2StringLambdas(5)
    val v7   = colType2StringLambdas(6)
    val v8   = colType2StringLambdas(7)
    val v9   = colType2StringLambdas(8)
    val v10  = colType2StringLambdas(9)
    val v11  = colType2StringLambdas(10)
    val v12  = colType2StringLambdas(11)
    val v13  = colType2StringLambdas(12)
    val v14  = colType2StringLambdas(13)
    val v15  = colType2StringLambdas(14)
    val v16  = colType2StringLambdas(15)
    val v17  = colType2StringLambdas(16)
    val v18  = colType2StringLambdas(17)
    val v19  = colType2StringLambdas(18)
    val v20  = colType2StringLambdas(19)
    val v21  = colType2StringLambdas(20)
    val v22  = colType2StringLambdas(21)
    val v23  = colType2StringLambdas(22)
    val v24  = colType2StringLambdas(23)
    val v25  = colType2StringLambdas(24)
    val v26  = colType2StringLambdas(25)
    val v27  = colType2StringLambdas(26)
    val v28  = colType2StringLambdas(27)
    val v29  = colType2StringLambdas(28)
    val v30  = colType2StringLambdas(29)
    val v31  = colType2StringLambdas(30)
    val v32  = colType2StringLambdas(31)
    val v33  = colType2StringLambdas(32)
    val v34  = colType2StringLambdas(33)
    val v35  = colType2StringLambdas(34)
    val v36  = colType2StringLambdas(35)
    val v37  = colType2StringLambdas(36)
    val v38  = colType2StringLambdas(37)
    val v39  = colType2StringLambdas(38)
    val v40  = colType2StringLambdas(39)
    val v41  = colType2StringLambdas(40)
    val v42  = colType2StringLambdas(41)
    val v43  = colType2StringLambdas(42)
    val v44  = colType2StringLambdas(43)
    val v45  = colType2StringLambdas(44)
    val v46  = colType2StringLambdas(45)
    val v47  = colType2StringLambdas(46)
    val v48  = colType2StringLambdas(47)
    val v49  = colType2StringLambdas(48)
    val v50  = colType2StringLambdas(49)
    val v51  = colType2StringLambdas(50)
    val v52  = colType2StringLambdas(51)
    val v53  = colType2StringLambdas(52)
    val v54  = colType2StringLambdas(53)
    val v55  = colType2StringLambdas(54)
    val v56  = colType2StringLambdas(55)
    val v57  = colType2StringLambdas(56)
    val v58  = colType2StringLambdas(57)
    val v59  = colType2StringLambdas(58)
    val v60  = colType2StringLambdas(59)
    val v61  = colType2StringLambdas(60)
    val v62  = colType2StringLambdas(61)
    val v63  = colType2StringLambdas(62)
    val v64  = colType2StringLambdas(63)
    val v65  = colType2StringLambdas(64)
    val v66  = colType2StringLambdas(65)
    val v67  = colType2StringLambdas(66)
    val v68  = colType2StringLambdas(67)
    val v69  = colType2StringLambdas(68)
    val v70  = colType2StringLambdas(69)
    val v71  = colType2StringLambdas(70)
    val v72  = colType2StringLambdas(71)
    val v73  = colType2StringLambdas(72)
    val v74  = colType2StringLambdas(73)
    val v75  = colType2StringLambdas(74)
    val v76  = colType2StringLambdas(75)
    val v77  = colType2StringLambdas(76)
    val v78  = colType2StringLambdas(77)
    val v79  = colType2StringLambdas(78)
    val v80  = colType2StringLambdas(79)
    val v81  = colType2StringLambdas(80)
    val v82  = colType2StringLambdas(81)
    val v83  = colType2StringLambdas(82)
    val v84  = colType2StringLambdas(83)
    val v85  = colType2StringLambdas(84)
    val v86  = colType2StringLambdas(85)
    val v87  = colType2StringLambdas(86)
    val v88  = colType2StringLambdas(87)
    val v89  = colType2StringLambdas(88)
    val v90  = colType2StringLambdas(89)
    val v91  = colType2StringLambdas(90)
    val v92  = colType2StringLambdas(91)
    val v93  = colType2StringLambdas(92)
    val v94  = colType2StringLambdas(93)
    val v95  = colType2StringLambdas(94)
    val v96  = colType2StringLambdas(95)
    val v97  = colType2StringLambdas(96)
    val v98  = colType2StringLambdas(97)
    val v99  = colType2StringLambdas(98)
    val v100 = colType2StringLambdas(99)
    scalafiddle.lambda100.foreach { fn =>
      val applyFn = (j: Int) => fn(
        js.Tuple5(
          js.Tuple20(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j)),
          js.Tuple20(v21(j), v22(j), v23(j), v24(j), v25(j), v26(j), v27(j), v28(j), v29(j), v30(j), v31(j), v32(j), v33(j), v34(j), v35(j), v36(j), v37(j), v38(j), v39(j), v40(j)),
          js.Tuple20(v41(j), v42(j), v43(j), v44(j), v45(j), v46(j), v47(j), v48(j), v49(j), v50(j), v51(j), v52(j), v53(j), v54(j), v55(j), v56(j), v57(j), v58(j), v59(j), v60(j)),
          js.Tuple20(v61(j), v62(j), v63(j), v64(j), v65(j), v66(j), v67(j), v68(j), v69(j), v70(j), v71(j), v72(j), v73(j), v74(j), v75(j), v76(j), v77(j), v78(j), v79(j), v80(j)),
          js.Tuple20(v81(j), v82(j), v83(j), v84(j), v85(j), v86(j), v87(j), v88(j), v89(j), v90(j), v91(j), v92(j), v93(j), v94(j), v95(j), v96(j), v97(j), v98(j), v99(j), v100(j))
        )
      )
      var i = 0
      while (i < lastRow) {
        resolve(i, applyFn)
        i += 1
      }
      groupEditId() = ""
    }
  }
}
