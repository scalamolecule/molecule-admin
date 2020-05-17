package moleculeadmin.client.app.logic.query.data

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.shared.ast.query.{QueryResult, _}
import org.scalajs.dom.html.TableSection
import rx.Ctx


case class RowBuilder(
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult
)(implicit ctx: Ctx.Owner)
  extends Cell(tableBody, cols, qr) with Paging {

  def appendRows(): Unit = appender(offset.now, curLastRow, cachedIndexBridge)

  var rowIndex    = 0
  var bridgeIndex = 0

  val appender = cols.size match {
    case 1 =>
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

    case 2 =>
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

    case 3 =>
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

    case 4 =>
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

    case 5 =>
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

    case 6 =>
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

    case 7 =>
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

    case 8 =>
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

    case 9 =>
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

    case 10 =>
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

    case 11 =>
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

    case 12 =>
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

    case 13 =>
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

    case 14 =>
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

    case 15 =>
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

    case 16 =>
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

    case 17 =>
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

    case 18 =>
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

    case 19 =>
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

    case 20 =>
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

    case 21 =>
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

    case 22 =>
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

    case 23 =>
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

    case 24 =>
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

    case 25 =>
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

    case 26 =>
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

    case 27 =>
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

    case 28 =>
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

    case 29 =>
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

    case 30 =>
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

    case 31 =>
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

    case 32 =>
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

    case 33 =>
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

    case 34 =>
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

    case 35 =>
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

    case 36 =>
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

    case 37 =>
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

    case 38 =>
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

    case 39 =>
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

    case 40 =>
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

    case 41 =>
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

    case 42 =>
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

    case 43 =>
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

    case 44 =>
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

    case 45 =>
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

    case 46 =>
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

    case 47 =>
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

    case 48 =>
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

    case 49 =>
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

    case 50 =>
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

    case 51 =>
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

    case 52 =>
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

    case 53 =>
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

    case 54 =>
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

    case 55 =>
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

    case 56 =>
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

    case 57 =>
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

    case 58 =>
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

    case 59 =>
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

    case 60 =>
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
}
