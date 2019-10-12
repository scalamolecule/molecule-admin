package moleculeadmin.client.app.domain.query.data

import moleculeadmin.client.app.domain.query.QueryState.offset
import moleculeadmin.shared.ast.query.{QueryResult, _}
import org.scalajs.dom.html.TableSection
import rx.Ctx


case class RowBuilder(db: String,
                      tableBody: TableSection,
                      cols: Seq[Col],
                      qr: QueryResult,
                      sortIndex: Array[Int],
                      filterIndex: Array[Int]
                      )(implicit ctx: Ctx.Owner)
  extends Cell(db, tableBody, cols, qr) {

  def append(): Unit = {
    val lastRow                 = curLastRow
    var rowIndex                = offset.now
    val indexBridge: Int => Int = {
      if (filterIndex.nonEmpty)
        (i: Int) => filterIndex(i)
      else if (sortIndex.nonEmpty)
        (i: Int) => sortIndex(i)
      else
        (i: Int) => i
    }

    cols.size match {
      case 1 =>
        val cellLambda1 = cellLambda(0)
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex))
            )
          )
          rowIndex += 1
        }

      case 2 =>
        val cellLambda1 = cellLambda(0)
        val cellLambda2 = cellLambda(1)
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex))
            )
          )
          rowIndex += 1
        }

      case 3 =>
        val cellLambda1 = cellLambda(0)
        val cellLambda2 = cellLambda(1)
        val cellLambda3 = cellLambda(2)
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex))
            )
          )
          rowIndex += 1
        }

      case 4 =>
        val cellLambda1 = cellLambda(0)
        val cellLambda2 = cellLambda(1)
        val cellLambda3 = cellLambda(2)
        val cellLambda4 = cellLambda(3)
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex)),
              cellLambda17(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex)),
              cellLambda17(indexBridge(rowIndex)),
              cellLambda18(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex)),
              cellLambda17(indexBridge(rowIndex)),
              cellLambda18(indexBridge(rowIndex)),
              cellLambda19(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex)),
              cellLambda17(indexBridge(rowIndex)),
              cellLambda18(indexBridge(rowIndex)),
              cellLambda19(indexBridge(rowIndex)),
              cellLambda20(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex)),
              cellLambda17(indexBridge(rowIndex)),
              cellLambda18(indexBridge(rowIndex)),
              cellLambda19(indexBridge(rowIndex)),
              cellLambda20(indexBridge(rowIndex)),
              cellLambda21(indexBridge(rowIndex))
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
        while (rowIndex < lastRow) {
          e = 0
          tableBody.appendChild(
            _mkRow(
              rowIndex,
              cellLambda1(indexBridge(rowIndex)),
              cellLambda2(indexBridge(rowIndex)),
              cellLambda3(indexBridge(rowIndex)),
              cellLambda4(indexBridge(rowIndex)),
              cellLambda5(indexBridge(rowIndex)),
              cellLambda6(indexBridge(rowIndex)),
              cellLambda7(indexBridge(rowIndex)),
              cellLambda8(indexBridge(rowIndex)),
              cellLambda9(indexBridge(rowIndex)),
              cellLambda10(indexBridge(rowIndex)),
              cellLambda11(indexBridge(rowIndex)),
              cellLambda12(indexBridge(rowIndex)),
              cellLambda13(indexBridge(rowIndex)),
              cellLambda14(indexBridge(rowIndex)),
              cellLambda15(indexBridge(rowIndex)),
              cellLambda16(indexBridge(rowIndex)),
              cellLambda17(indexBridge(rowIndex)),
              cellLambda18(indexBridge(rowIndex)),
              cellLambda19(indexBridge(rowIndex)),
              cellLambda20(indexBridge(rowIndex)),
              cellLambda21(indexBridge(rowIndex)),
              cellLambda22(indexBridge(rowIndex))
            )
          )
          rowIndex += 1
        }
    }
  }

}
