package moleculeadmin.client.app.logic.query.data.row

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.cell.CellMakerClean
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.shared.ast.query.{QueryResult, _}
import org.scalajs.dom.html.TableSection
import rx.Ctx


case class RowBuilderClean(
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult
)(implicit ctx: Ctx.Owner)
  extends CellMakerClean(tableBody, cols, qr) with Paging with RowBuilder {

  def appendRows(): Unit = appender(offset.now, curLastRow, cachedIndexBridge)

  var rowIndex    = 0
  var bridgeIndex = 0

  val appender = cols.size match {
    case 1   => append1
    case 2   => append2
    case 3   => append3
    case 4   => append4
    case 5   => append5
    case 6   => append6
    case 7   => append7
    case 8   => append8
    case 9   => append9
    case 10  => append10
    case 11  => append11
    case 12  => append12
    case 13  => append13
    case 14  => append14
    case 15  => append15
    case 16  => append16
    case 17  => append17
    case 18  => append18
    case 19  => append19
    case 20  => append20
    case 21  => append21
    case 22  => append22
    case 23  => append23
    case 24  => append24
    case 25  => append25
    case 26  => append26
    case 27  => append27
    case 28  => append28
    case 29  => append29
    case 30  => append30
    case 31  => append31
    case 32  => append32
    case 33  => append33
    case 34  => append34
    case 35  => append35
    case 36  => append36
    case 37  => append37
    case 38  => append38
    case 39  => append39
    case 40  => append40
    case 41  => append41
    case 42  => append42
    case 43  => append43
    case 44  => append44
    case 45  => append45
    case 46  => append46
    case 47  => append47
    case 48  => append48
    case 49  => append49
    case 50  => append50
    case 51  => append51
    case 52  => append52
    case 53  => append53
    case 54  => append54
    case 55  => append55
    case 56  => append56
    case 57  => append57
    case 58  => append58
    case 59  => append59
    case 60  => append60
    case 61  => append61
    case 62  => append62
    case 63  => append63
    case 64  => append64
    case 65  => append65
    case 66  => append66
    case 67  => append67
    case 68  => append68
    case 69  => append69
    case 70  => append70
    case 71  => append71
    case 72  => append72
    case 73  => append73
    case 74  => append74
    case 75  => append75
    case 76  => append76
    case 77  => append77
    case 78  => append78
    case 79  => append79
    case 80  => append80
    case 81  => append81
    case 82  => append82
    case 83  => append83
    case 84  => append84
    case 85  => append85
    case 86  => append86
    case 87  => append87
    case 88  => append88
    case 89  => append89
    case 90  => append90
    case 91  => append91
    case 92  => append92
    case 93  => append93
    case 94  => append94
    case 95  => append95
    case 96  => append96
    case 97  => append97
    case 98  => append98
    case 99  => append99
    case 100 => append100
  }

  def append1: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append2: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append3: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append4: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    val cellLambda4 = cellLambda(3)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append5: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    val cellLambda4 = cellLambda(3)
    val cellLambda5 = cellLambda(4)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append6: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    val cellLambda4 = cellLambda(3)
    val cellLambda5 = cellLambda(4)
    val cellLambda6 = cellLambda(5)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append7: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    val cellLambda4 = cellLambda(3)
    val cellLambda5 = cellLambda(4)
    val cellLambda6 = cellLambda(5)
    val cellLambda7 = cellLambda(6)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append8: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    val cellLambda4 = cellLambda(3)
    val cellLambda5 = cellLambda(4)
    val cellLambda6 = cellLambda(5)
    val cellLambda7 = cellLambda(6)
    val cellLambda8 = cellLambda(7)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append9: (Int, Int, Int => Int) => Unit = {
    val cellLambda1 = cellLambda(0)
    val cellLambda2 = cellLambda(1)
    val cellLambda3 = cellLambda(2)
    val cellLambda4 = cellLambda(3)
    val cellLambda5 = cellLambda(4)
    val cellLambda6 = cellLambda(5)
    val cellLambda7 = cellLambda(6)
    val cellLambda8 = cellLambda(7)
    val cellLambda9 = cellLambda(8)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append10: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append11: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append12: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append13: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append14: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append15: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append16: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append17: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append18: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append19: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append20: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append21: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append22: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append23: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex)
          )
        )
        rowIndex += 1
      }
  }

  def append24: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append25: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append26: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append27: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append28: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append29: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append30: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append31: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append32: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append33: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append34: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append35: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append36: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append37: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append38: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append39: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append40: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append41: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append42: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append43: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append44: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append45: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append46: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append47: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append48: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append49: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append50: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append51: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append52: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append53: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append54: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append55: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append56: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append57: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append58: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append59: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append60: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append61: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append62: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append63: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append64: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append65: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append66: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append67: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append68: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append69: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append70: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append71: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append72: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append73: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append74: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append75: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append76: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append77: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append78: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append79: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append80: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append81: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append82: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append83: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append84: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append85: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append86: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append87: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append88: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append89: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append90: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append91: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append92: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append93: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append94: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    val cellLambda94 = cellLambda(93)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append95: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    val cellLambda94 = cellLambda(93)
    val cellLambda95 = cellLambda(94)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
            cellLambda95(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append96: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    val cellLambda94 = cellLambda(93)
    val cellLambda95 = cellLambda(94)
    val cellLambda96 = cellLambda(95)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
            cellLambda95(bridgeIndex),
            cellLambda96(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append97: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    val cellLambda94 = cellLambda(93)
    val cellLambda95 = cellLambda(94)
    val cellLambda96 = cellLambda(95)
    val cellLambda97 = cellLambda(96)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
            cellLambda95(bridgeIndex),
            cellLambda96(bridgeIndex),
            cellLambda97(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append98: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    val cellLambda94 = cellLambda(93)
    val cellLambda95 = cellLambda(94)
    val cellLambda96 = cellLambda(95)
    val cellLambda97 = cellLambda(96)
    val cellLambda98 = cellLambda(97)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
            cellLambda95(bridgeIndex),
            cellLambda96(bridgeIndex),
            cellLambda97(bridgeIndex),
            cellLambda98(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append99: (Int, Int, Int => Int) => Unit = {
    val cellLambda1  = cellLambda(0)
    val cellLambda2  = cellLambda(1)
    val cellLambda3  = cellLambda(2)
    val cellLambda4  = cellLambda(3)
    val cellLambda5  = cellLambda(4)
    val cellLambda6  = cellLambda(5)
    val cellLambda7  = cellLambda(6)
    val cellLambda8  = cellLambda(7)
    val cellLambda9  = cellLambda(8)
    val cellLambda10 = cellLambda(9)
    val cellLambda11 = cellLambda(10)
    val cellLambda12 = cellLambda(11)
    val cellLambda13 = cellLambda(12)
    val cellLambda14 = cellLambda(13)
    val cellLambda15 = cellLambda(14)
    val cellLambda16 = cellLambda(15)
    val cellLambda17 = cellLambda(16)
    val cellLambda18 = cellLambda(17)
    val cellLambda19 = cellLambda(18)
    val cellLambda20 = cellLambda(19)
    val cellLambda21 = cellLambda(20)
    val cellLambda22 = cellLambda(21)
    val cellLambda23 = cellLambda(22)
    val cellLambda24 = cellLambda(23)
    val cellLambda25 = cellLambda(24)
    val cellLambda26 = cellLambda(25)
    val cellLambda27 = cellLambda(26)
    val cellLambda28 = cellLambda(27)
    val cellLambda29 = cellLambda(28)
    val cellLambda30 = cellLambda(29)
    val cellLambda31 = cellLambda(30)
    val cellLambda32 = cellLambda(31)
    val cellLambda33 = cellLambda(32)
    val cellLambda34 = cellLambda(33)
    val cellLambda35 = cellLambda(34)
    val cellLambda36 = cellLambda(35)
    val cellLambda37 = cellLambda(36)
    val cellLambda38 = cellLambda(37)
    val cellLambda39 = cellLambda(38)
    val cellLambda40 = cellLambda(39)
    val cellLambda41 = cellLambda(40)
    val cellLambda42 = cellLambda(41)
    val cellLambda43 = cellLambda(42)
    val cellLambda44 = cellLambda(43)
    val cellLambda45 = cellLambda(44)
    val cellLambda46 = cellLambda(45)
    val cellLambda47 = cellLambda(46)
    val cellLambda48 = cellLambda(47)
    val cellLambda49 = cellLambda(48)
    val cellLambda50 = cellLambda(49)
    val cellLambda51 = cellLambda(50)
    val cellLambda52 = cellLambda(51)
    val cellLambda53 = cellLambda(52)
    val cellLambda54 = cellLambda(53)
    val cellLambda55 = cellLambda(54)
    val cellLambda56 = cellLambda(55)
    val cellLambda57 = cellLambda(56)
    val cellLambda58 = cellLambda(57)
    val cellLambda59 = cellLambda(58)
    val cellLambda60 = cellLambda(59)
    val cellLambda61 = cellLambda(60)
    val cellLambda62 = cellLambda(61)
    val cellLambda63 = cellLambda(62)
    val cellLambda64 = cellLambda(63)
    val cellLambda65 = cellLambda(64)
    val cellLambda66 = cellLambda(65)
    val cellLambda67 = cellLambda(66)
    val cellLambda68 = cellLambda(67)
    val cellLambda69 = cellLambda(68)
    val cellLambda70 = cellLambda(69)
    val cellLambda71 = cellLambda(70)
    val cellLambda72 = cellLambda(71)
    val cellLambda73 = cellLambda(72)
    val cellLambda74 = cellLambda(73)
    val cellLambda75 = cellLambda(74)
    val cellLambda76 = cellLambda(75)
    val cellLambda77 = cellLambda(76)
    val cellLambda78 = cellLambda(77)
    val cellLambda79 = cellLambda(78)
    val cellLambda80 = cellLambda(79)
    val cellLambda81 = cellLambda(80)
    val cellLambda82 = cellLambda(81)
    val cellLambda83 = cellLambda(82)
    val cellLambda84 = cellLambda(83)
    val cellLambda85 = cellLambda(84)
    val cellLambda86 = cellLambda(85)
    val cellLambda87 = cellLambda(86)
    val cellLambda88 = cellLambda(87)
    val cellLambda89 = cellLambda(88)
    val cellLambda90 = cellLambda(89)
    val cellLambda91 = cellLambda(90)
    val cellLambda92 = cellLambda(91)
    val cellLambda93 = cellLambda(92)
    val cellLambda94 = cellLambda(93)
    val cellLambda95 = cellLambda(94)
    val cellLambda96 = cellLambda(95)
    val cellLambda97 = cellLambda(96)
    val cellLambda98 = cellLambda(97)
    val cellLambda99 = cellLambda(98)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
            cellLambda95(bridgeIndex),
            cellLambda96(bridgeIndex),
            cellLambda97(bridgeIndex),
            cellLambda98(bridgeIndex),
            cellLambda99(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }

  def append100: (Int, Int, Int => Int) => Unit = {
    val cellLambda1   = cellLambda(0)
    val cellLambda2   = cellLambda(1)
    val cellLambda3   = cellLambda(2)
    val cellLambda4   = cellLambda(3)
    val cellLambda5   = cellLambda(4)
    val cellLambda6   = cellLambda(5)
    val cellLambda7   = cellLambda(6)
    val cellLambda8   = cellLambda(7)
    val cellLambda9   = cellLambda(8)
    val cellLambda10  = cellLambda(9)
    val cellLambda11  = cellLambda(10)
    val cellLambda12  = cellLambda(11)
    val cellLambda13  = cellLambda(12)
    val cellLambda14  = cellLambda(13)
    val cellLambda15  = cellLambda(14)
    val cellLambda16  = cellLambda(15)
    val cellLambda17  = cellLambda(16)
    val cellLambda18  = cellLambda(17)
    val cellLambda19  = cellLambda(18)
    val cellLambda20  = cellLambda(19)
    val cellLambda21  = cellLambda(20)
    val cellLambda22  = cellLambda(21)
    val cellLambda23  = cellLambda(22)
    val cellLambda24  = cellLambda(23)
    val cellLambda25  = cellLambda(24)
    val cellLambda26  = cellLambda(25)
    val cellLambda27  = cellLambda(26)
    val cellLambda28  = cellLambda(27)
    val cellLambda29  = cellLambda(28)
    val cellLambda30  = cellLambda(29)
    val cellLambda31  = cellLambda(30)
    val cellLambda32  = cellLambda(31)
    val cellLambda33  = cellLambda(32)
    val cellLambda34  = cellLambda(33)
    val cellLambda35  = cellLambda(34)
    val cellLambda36  = cellLambda(35)
    val cellLambda37  = cellLambda(36)
    val cellLambda38  = cellLambda(37)
    val cellLambda39  = cellLambda(38)
    val cellLambda40  = cellLambda(39)
    val cellLambda41  = cellLambda(40)
    val cellLambda42  = cellLambda(41)
    val cellLambda43  = cellLambda(42)
    val cellLambda44  = cellLambda(43)
    val cellLambda45  = cellLambda(44)
    val cellLambda46  = cellLambda(45)
    val cellLambda47  = cellLambda(46)
    val cellLambda48  = cellLambda(47)
    val cellLambda49  = cellLambda(48)
    val cellLambda50  = cellLambda(49)
    val cellLambda51  = cellLambda(50)
    val cellLambda52  = cellLambda(51)
    val cellLambda53  = cellLambda(52)
    val cellLambda54  = cellLambda(53)
    val cellLambda55  = cellLambda(54)
    val cellLambda56  = cellLambda(55)
    val cellLambda57  = cellLambda(56)
    val cellLambda58  = cellLambda(57)
    val cellLambda59  = cellLambda(58)
    val cellLambda60  = cellLambda(59)
    val cellLambda61  = cellLambda(60)
    val cellLambda62  = cellLambda(61)
    val cellLambda63  = cellLambda(62)
    val cellLambda64  = cellLambda(63)
    val cellLambda65  = cellLambda(64)
    val cellLambda66  = cellLambda(65)
    val cellLambda67  = cellLambda(66)
    val cellLambda68  = cellLambda(67)
    val cellLambda69  = cellLambda(68)
    val cellLambda70  = cellLambda(69)
    val cellLambda71  = cellLambda(70)
    val cellLambda72  = cellLambda(71)
    val cellLambda73  = cellLambda(72)
    val cellLambda74  = cellLambda(73)
    val cellLambda75  = cellLambda(74)
    val cellLambda76  = cellLambda(75)
    val cellLambda77  = cellLambda(76)
    val cellLambda78  = cellLambda(77)
    val cellLambda79  = cellLambda(78)
    val cellLambda80  = cellLambda(79)
    val cellLambda81  = cellLambda(80)
    val cellLambda82  = cellLambda(81)
    val cellLambda83  = cellLambda(82)
    val cellLambda84  = cellLambda(83)
    val cellLambda85  = cellLambda(84)
    val cellLambda86  = cellLambda(85)
    val cellLambda87  = cellLambda(86)
    val cellLambda88  = cellLambda(87)
    val cellLambda89  = cellLambda(88)
    val cellLambda90  = cellLambda(89)
    val cellLambda91  = cellLambda(90)
    val cellLambda92  = cellLambda(91)
    val cellLambda93  = cellLambda(92)
    val cellLambda94  = cellLambda(93)
    val cellLambda95  = cellLambda(94)
    val cellLambda96  = cellLambda(95)
    val cellLambda97  = cellLambda(96)
    val cellLambda98  = cellLambda(97)
    val cellLambda99  = cellLambda(98)
    val cellLambda100 = cellLambda(99)
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex),
            cellLambda2(bridgeIndex),
            cellLambda3(bridgeIndex),
            cellLambda4(bridgeIndex),
            cellLambda5(bridgeIndex),
            cellLambda6(bridgeIndex),
            cellLambda7(bridgeIndex),
            cellLambda8(bridgeIndex),
            cellLambda9(bridgeIndex),
            cellLambda10(bridgeIndex),
            cellLambda11(bridgeIndex),
            cellLambda12(bridgeIndex),
            cellLambda13(bridgeIndex),
            cellLambda14(bridgeIndex),
            cellLambda15(bridgeIndex),
            cellLambda16(bridgeIndex),
            cellLambda17(bridgeIndex),
            cellLambda18(bridgeIndex),
            cellLambda19(bridgeIndex),
            cellLambda20(bridgeIndex),
            cellLambda21(bridgeIndex),
            cellLambda22(bridgeIndex),
            cellLambda23(bridgeIndex),
            cellLambda24(bridgeIndex),
            cellLambda25(bridgeIndex),
            cellLambda26(bridgeIndex),
            cellLambda27(bridgeIndex),
            cellLambda28(bridgeIndex),
            cellLambda29(bridgeIndex),
            cellLambda30(bridgeIndex),
            cellLambda31(bridgeIndex),
            cellLambda32(bridgeIndex),
            cellLambda33(bridgeIndex),
            cellLambda34(bridgeIndex),
            cellLambda35(bridgeIndex),
            cellLambda36(bridgeIndex),
            cellLambda37(bridgeIndex),
            cellLambda38(bridgeIndex),
            cellLambda39(bridgeIndex),
            cellLambda40(bridgeIndex),
            cellLambda41(bridgeIndex),
            cellLambda42(bridgeIndex),
            cellLambda43(bridgeIndex),
            cellLambda44(bridgeIndex),
            cellLambda45(bridgeIndex),
            cellLambda46(bridgeIndex),
            cellLambda47(bridgeIndex),
            cellLambda48(bridgeIndex),
            cellLambda49(bridgeIndex),
            cellLambda50(bridgeIndex),
            cellLambda51(bridgeIndex),
            cellLambda52(bridgeIndex),
            cellLambda53(bridgeIndex),
            cellLambda54(bridgeIndex),
            cellLambda55(bridgeIndex),
            cellLambda56(bridgeIndex),
            cellLambda57(bridgeIndex),
            cellLambda58(bridgeIndex),
            cellLambda59(bridgeIndex),
            cellLambda60(bridgeIndex),
            cellLambda61(bridgeIndex),
            cellLambda62(bridgeIndex),
            cellLambda63(bridgeIndex),
            cellLambda64(bridgeIndex),
            cellLambda65(bridgeIndex),
            cellLambda66(bridgeIndex),
            cellLambda67(bridgeIndex),
            cellLambda68(bridgeIndex),
            cellLambda69(bridgeIndex),
            cellLambda70(bridgeIndex),
            cellLambda71(bridgeIndex),
            cellLambda72(bridgeIndex),
            cellLambda73(bridgeIndex),
            cellLambda74(bridgeIndex),
            cellLambda75(bridgeIndex),
            cellLambda76(bridgeIndex),
            cellLambda77(bridgeIndex),
            cellLambda78(bridgeIndex),
            cellLambda79(bridgeIndex),
            cellLambda80(bridgeIndex),
            cellLambda81(bridgeIndex),
            cellLambda82(bridgeIndex),
            cellLambda83(bridgeIndex),
            cellLambda84(bridgeIndex),
            cellLambda85(bridgeIndex),
            cellLambda86(bridgeIndex),
            cellLambda87(bridgeIndex),
            cellLambda88(bridgeIndex),
            cellLambda89(bridgeIndex),
            cellLambda90(bridgeIndex),
            cellLambda91(bridgeIndex),
            cellLambda92(bridgeIndex),
            cellLambda93(bridgeIndex),
            cellLambda94(bridgeIndex),
            cellLambda95(bridgeIndex),
            cellLambda96(bridgeIndex),
            cellLambda97(bridgeIndex),
            cellLambda98(bridgeIndex),
            cellLambda99(bridgeIndex),
            cellLambda100(bridgeIndex),
          )
        )
        rowIndex += 1
      }
  }
}
