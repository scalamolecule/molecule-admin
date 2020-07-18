package moleculeadmin.client.app.logic.query.data.row

import moleculeadmin.client.app.logic.query.QueryState._
import moleculeadmin.client.app.logic.query.data.cell.CellMakerGrid
import moleculeadmin.client.app.logic.query.keyEvents.Paging
import moleculeadmin.shared.ast.query.{QueryResult, _}
import org.scalajs.dom.html.TableSection
import rx.Ctx


case class RowBuilderGrid(
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult,
  gridType: Int
)(implicit ctx: Ctx.Owner)
  extends CellMakerGrid(tableBody, cols, qr) with Paging with RowBuilder {

  def appendRows(): Unit = appender(offset.now, curLastRow, cachedIndexBridge)

  var rowIndex    = 0
  var bridgeIndex = 0
  var priorities  = Seq.empty[String]

  // Compare sort column values of this and previous row
  var prev1 = ""
  var prev2 = ""
  var prev3 = ""
  var prev4 = ""
  var prev5 = ""
  var v1    = ""
  var v2    = ""
  var v3    = ""
  var v4    = ""
  var v5    = ""

  /**
   * Priority resolver
   *
   * A `gridType` Int value denotes various grids that can be applied
   * rendering a visualization of nested data structures when one or more rows
   * are sorted.
   *
   * Cells that contain a changed value compared to the previous row are marked
   * with a line (border) between the cells. This line extends to the following
   * cells depending on which grid type is chosen.
   *
   * Levels can be muted and will thus not be marked. Muting one or more levels
   * will re-align remaining marked levels evenly from top priority and down.
   *
   * Note that col.sortPos 1 is "highest level/priority", col.sortPos 2 a
   * "lower level/priority" and so on. So col.sortPos and "level"/"priority" are
   * inversely proportional to each other.
   *
   * First priority markings always span from first to last cell.
   *
   * The grid types are:
   *
   * 0 - No marking (RowBuilderFlat will be used instead)
   * 1 - Mark from change until next higher priority and continue new priority if changed
   * 2 - Highest changed priority marks all columns from start to end
   *
   * @param valueLambdas Seq[Int => String] Value retrievers for each column
   * @return (Int, Boolean) => Seq[String] Css priority retrievers for all
   *         columns. Given a colIndex and a isFirstRow boolean a Seq of css
   *         priority class names for all columns is produced. Non-sorted
   *         columns simply have an empty string.
   */
  def getPriorityResolver(
    valueLambdas: Seq[Int => String]
  ): (Int, Boolean) => Seq[String] = {
    val levelCount = gridColIndexes.now.length
    val activeCols = {
      val nci = gridColIndexes.now
      cols.sortBy(_.sortPos).map {
        case c if c.sortDir.nonEmpty =>
          // Mute sort columns
          if (nci.contains(c.colIndex)) c else c.copy(sortDir = "", sortPos = 0)
        case c                       => c
      }.foldLeft(1, Seq.empty[Col]) {
        case ((l, cols), c) if c.sortDir.nonEmpty =>
          // Re-align sort position
          (l + 1, cols :+ c.copy(sortPos = l))
        case ((l, cols), c)                       => (l, cols :+ c)
      }._2.sortBy(_.colIndex) // Restore original column order
    }
    val depth      = activeCols.map(_.sortPos).max

    def cssPrioC(c: Col): String = {
      cssPrio(activeCols(c.colIndex).sortPos)
    }
    def cssPrio(level: Int): String = {
      if (level == 0) "" else " prio" + (5 - depth + level)
    }

    if (levelCount == 1) {
      val valueLambda = activeCols.collect {
        case c if c.sortDir.nonEmpty => valueLambdas(c.colIndex)
      }.head

      (bridgeIndex: Int, first: Boolean) => {
        v1 = valueLambda(bridgeIndex)
        val changed = v1 != prev1
        prev1 = v1
        val prio = if (changed) cssPrio(1) else ""
        // Same level for all columns
        if (first)
          Seq.fill(activeCols.length)("")
        else
          Seq.fill(activeCols.length)(prio)
      }

    } else {

      (bridgeIndex: Int, first: Boolean) => {
        if (first) {
          // Set values that second row can compare to
          activeCols.collect {
            case c if c.sortPos == 1 => prev1 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 2 => prev2 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 3 => prev3 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 4 => prev4 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 5 => prev5 = valueLambdas(c.colIndex)(bridgeIndex)
          }
          Seq.fill(activeCols.length)("")
        } else {
          // Determine changed columns first
          val (changedLevels, changed) = activeCols.foldLeft(
            Seq.empty[Int], Seq.empty[Boolean]
          ) {
            case ((cl, changedCols), c) if c.sortPos == 0 =>
              (cl, changedCols :+ false)

            case ((cl, changedCols), c) =>
              c.sortPos match {
                case 1 =>
                  v1 = valueLambdas(c.colIndex)(bridgeIndex)
                  val changed = v1 != prev1
                  prev1 = v1
                  (if (changed) cl :+ 1 else cl, changedCols :+ changed)

                case 2 =>
                  v2 = valueLambdas(c.colIndex)(bridgeIndex)
                  val changed = v2 != prev2
                  prev2 = v2
                  (if (changed) cl :+ 2 else cl, changedCols :+ changed)

                case 3 =>
                  v3 = valueLambdas(c.colIndex)(bridgeIndex)
                  val changed = v3 != prev3
                  prev3 = v3
                  (if (changed) cl :+ 3 else cl, changedCols :+ changed)

                case 4 =>
                  v4 = valueLambdas(c.colIndex)(bridgeIndex)
                  val changed = v4 != prev4
                  prev4 = v4
                  (if (changed) cl :+ 4 else cl, changedCols :+ changed)

                case 5 =>
                  v5 = valueLambdas(c.colIndex)(bridgeIndex)
                  val changed = v5 != prev5
                  prev5 = v5
                  (if (changed) cl :+ 5 else cl, changedCols :+ changed)
              }
          }


          changedLevels.minOption.getOrElse(0) match {
            case 0              =>
              // No change, no marking
              Seq.fill(cols.length)("")
            case 1              =>
              // Top level changed, mark all
              Seq.fill(cols.length)(cssPrio(1))
            case topChangeLevel =>
              gridType match {
                case 1 =>
                  cols.foldLeft(0, Seq.empty[String]) {
                    case ((0, acc), c) if changed(c.colIndex) =>
                      // This or lower level marked
                      (activeCols(c.colIndex).sortPos, acc :+ cssPrioC(c))

                    case ((0, acc), _) =>
                      // Not yet changed
                      (0, acc :+ "")

                    case ((l, acc), c)
                      if changed(c.colIndex) && activeCols(c.colIndex).sortPos < l =>
                      // Higher priority takes new precedence
                      (activeCols(c.colIndex).sortPos, acc :+ cssPrioC(c))

                    case ((l, acc), c) =>
                      // Continue on same level
                      (l, acc :+ cssPrio(l))
                  }._2

                case 2 =>
                  // Sub level changed, mark all with highest sub level
                  Seq.fill(cols.length)(cssPrio(topChangeLevel))
              }
          }
        }
      }
    }
  }

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
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val priorityResolver  = getPriorityResolver(
      Seq(t1))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0))
          )
        )
        rowIndex += 1
      }
  }

  def append2: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1))
          )
        )
        rowIndex += 1
      }
  }

  def append3: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2))
          )
        )
        rowIndex += 1
      }
  }

  def append4: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val (t4, cellLambda4) = cellLambdaGrid(3)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3, t4))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3))
          )
        )
        rowIndex += 1
      }
  }

  def append5: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val (t4, cellLambda4) = cellLambdaGrid(3)
    val (t5, cellLambda5) = cellLambdaGrid(4)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4))
          )
        )
        rowIndex += 1
      }
  }

  def append6: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val (t4, cellLambda4) = cellLambdaGrid(3)
    val (t5, cellLambda5) = cellLambdaGrid(4)
    val (t6, cellLambda6) = cellLambdaGrid(5)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5))
          )
        )
        rowIndex += 1
      }
  }

  def append7: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val (t4, cellLambda4) = cellLambdaGrid(3)
    val (t5, cellLambda5) = cellLambdaGrid(4)
    val (t6, cellLambda6) = cellLambdaGrid(5)
    val (t7, cellLambda7) = cellLambdaGrid(6)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6))
          )
        )
        rowIndex += 1
      }
  }

  def append8: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val (t4, cellLambda4) = cellLambdaGrid(3)
    val (t5, cellLambda5) = cellLambdaGrid(4)
    val (t6, cellLambda6) = cellLambdaGrid(5)
    val (t7, cellLambda7) = cellLambdaGrid(6)
    val (t8, cellLambda8) = cellLambdaGrid(7)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7))
          )
        )
        rowIndex += 1
      }
  }

  def append9: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1) = cellLambdaGrid(0)
    val (t2, cellLambda2) = cellLambdaGrid(1)
    val (t3, cellLambda3) = cellLambdaGrid(2)
    val (t4, cellLambda4) = cellLambdaGrid(3)
    val (t5, cellLambda5) = cellLambdaGrid(4)
    val (t6, cellLambda6) = cellLambdaGrid(5)
    val (t7, cellLambda7) = cellLambdaGrid(6)
    val (t8, cellLambda8) = cellLambdaGrid(7)
    val (t9, cellLambda9) = cellLambdaGrid(8)
    val priorityResolver  = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8))
          )
        )
        rowIndex += 1
      }
  }

  def append10: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9))
          )
        )
        rowIndex += 1
      }
  }

  def append11: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10))
          )
        )
        rowIndex += 1
      }
  }

  def append12: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11))
          )
        )
        rowIndex += 1
      }
  }

  def append13: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12))
          )
        )
        rowIndex += 1
      }
  }

  def append14: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13))
          )
        )
        rowIndex += 1
      }
  }

  def append15: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14))
          )
        )
        rowIndex += 1
      }
  }

  def append16: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15))
          )
        )
        rowIndex += 1
      }
  }

  def append17: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16))
          )
        )
        rowIndex += 1
      }
  }

  def append18: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17))
          )
        )
        rowIndex += 1
      }
  }

  def append19: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18))
          )
        )
        rowIndex += 1
      }
  }

  def append20: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19))
          )
        )
        rowIndex += 1
      }
  }

  def append21: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20))
          )
        )
        rowIndex += 1
      }
  }

  def append22: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21))
          )
        )
        rowIndex += 1
      }
  }

  def append23: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22))
          )
        )
        rowIndex += 1
      }
  }

  def append24: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
          )
        )
        rowIndex += 1
      }
  }

  def append25: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
          )
        )
        rowIndex += 1
      }
  }

  def append26: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
          )
        )
        rowIndex += 1
      }
  }

  def append27: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
          )
        )
        rowIndex += 1
      }
  }

  def append28: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
          )
        )
        rowIndex += 1
      }
  }

  def append29: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
          )
        )
        rowIndex += 1
      }
  }

  def append30: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
          )
        )
        rowIndex += 1
      }
  }

  def append31: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
          )
        )
        rowIndex += 1
      }
  }

  def append32: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
          )
        )
        rowIndex += 1
      }
  }

  def append33: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
          )
        )
        rowIndex += 1
      }
  }

  def append34: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
          )
        )
        rowIndex += 1
      }
  }

  def append35: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
          )
        )
        rowIndex += 1
      }
  }

  def append36: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
          )
        )
        rowIndex += 1
      }
  }

  def append37: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
          )
        )
        rowIndex += 1
      }
  }

  def append38: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
          )
        )
        rowIndex += 1
      }
  }

  def append39: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
          )
        )
        rowIndex += 1
      }
  }

  def append40: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
          )
        )
        rowIndex += 1
      }
  }

  def append41: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
          )
        )
        rowIndex += 1
      }
  }

  def append42: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
          )
        )
        rowIndex += 1
      }
  }

  def append43: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
          )
        )
        rowIndex += 1
      }
  }

  def append44: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
          )
        )
        rowIndex += 1
      }
  }

  def append45: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
          )
        )
        rowIndex += 1
      }
  }

  def append46: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
          )
        )
        rowIndex += 1
      }
  }

  def append47: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
          )
        )
        rowIndex += 1
      }
  }

  def append48: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
          )
        )
        rowIndex += 1
      }
  }

  def append49: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
          )
        )
        rowIndex += 1
      }
  }

  def append50: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
          )
        )
        rowIndex += 1
      }
  }

  def append51: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
          )
        )
        rowIndex += 1
      }
  }

  def append52: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
          )
        )
        rowIndex += 1
      }
  }

  def append53: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
          )
        )
        rowIndex += 1
      }
  }

  def append54: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
          )
        )
        rowIndex += 1
      }
  }

  def append55: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
          )
        )
        rowIndex += 1
      }
  }

  def append56: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
          )
        )
        rowIndex += 1
      }
  }

  def append57: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
          )
        )
        rowIndex += 1
      }
  }

  def append58: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
          )
        )
        rowIndex += 1
      }
  }

  def append59: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
          )
        )
        rowIndex += 1
      }
  }

  def append60: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
          )
        )
        rowIndex += 1
      }
  }

  def append61: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
          )
        )
        rowIndex += 1
      }
  }

  def append62: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
          )
        )
        rowIndex += 1
      }
  }

  def append63: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
          )
        )
        rowIndex += 1
      }
  }

  def append64: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
          )
        )
        rowIndex += 1
      }
  }

  def append65: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
          )
        )
        rowIndex += 1
      }
  }

  def append66: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
          )
        )
        rowIndex += 1
      }
  }

  def append67: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
          )
        )
        rowIndex += 1
      }
  }

  def append68: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
          )
        )
        rowIndex += 1
      }
  }

  def append69: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
          )
        )
        rowIndex += 1
      }
  }

  def append70: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
          )
        )
        rowIndex += 1
      }
  }

  def append71: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
          )
        )
        rowIndex += 1
      }
  }

  def append72: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
          )
        )
        rowIndex += 1
      }
  }

  def append73: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
          )
        )
        rowIndex += 1
      }
  }

  def append74: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
          )
        )
        rowIndex += 1
      }
  }

  def append75: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
          )
        )
        rowIndex += 1
      }
  }

  def append76: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
          )
        )
        rowIndex += 1
      }
  }

  def append77: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
          )
        )
        rowIndex += 1
      }
  }

  def append78: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
          )
        )
        rowIndex += 1
      }
  }

  def append79: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
          )
        )
        rowIndex += 1
      }
  }

  def append80: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
          )
        )
        rowIndex += 1
      }
  }

  def append81: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
          )
        )
        rowIndex += 1
      }
  }

  def append82: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
          )
        )
        rowIndex += 1
      }
  }

  def append83: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
          )
        )
        rowIndex += 1
      }
  }

  def append84: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
          )
        )
        rowIndex += 1
      }
  }

  def append85: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
          )
        )
        rowIndex += 1
      }
  }

  def append86: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
          )
        )
        rowIndex += 1
      }
  }

  def append87: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
          )
        )
        rowIndex += 1
      }
  }

  def append88: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
          )
        )
        rowIndex += 1
      }
  }

  def append89: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
          )
        )
        rowIndex += 1
      }
  }

  def append90: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
          )
        )
        rowIndex += 1
      }
  }

  def append91: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
          )
        )
        rowIndex += 1
      }
  }

  def append92: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
          )
        )
        rowIndex += 1
      }
  }

  def append93: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
          )
        )
        rowIndex += 1
      }
  }

  def append94: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val (t94, cellLambda94) = cellLambdaGrid(93)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
          )
        )
        rowIndex += 1
      }
  }

  def append95: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val (t94, cellLambda94) = cellLambdaGrid(93)
    val (t95, cellLambda95) = cellLambdaGrid(94)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
            cellLambda95(bridgeIndex, priorities(94)),
          )
        )
        rowIndex += 1
      }
  }

  def append96: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val (t94, cellLambda94) = cellLambdaGrid(93)
    val (t95, cellLambda95) = cellLambdaGrid(94)
    val (t96, cellLambda96) = cellLambdaGrid(95)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
            cellLambda95(bridgeIndex, priorities(94)),
            cellLambda96(bridgeIndex, priorities(95)),
          )
        )
        rowIndex += 1
      }
  }

  def append97: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val (t94, cellLambda94) = cellLambdaGrid(93)
    val (t95, cellLambda95) = cellLambdaGrid(94)
    val (t96, cellLambda96) = cellLambdaGrid(95)
    val (t97, cellLambda97) = cellLambdaGrid(96)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96, t97
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
            cellLambda95(bridgeIndex, priorities(94)),
            cellLambda96(bridgeIndex, priorities(95)),
            cellLambda97(bridgeIndex, priorities(96)),
          )
        )
        rowIndex += 1
      }
  }

  def append98: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val (t94, cellLambda94) = cellLambdaGrid(93)
    val (t95, cellLambda95) = cellLambdaGrid(94)
    val (t96, cellLambda96) = cellLambdaGrid(95)
    val (t97, cellLambda97) = cellLambdaGrid(96)
    val (t98, cellLambda98) = cellLambdaGrid(97)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96, t97, t98
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
            cellLambda95(bridgeIndex, priorities(94)),
            cellLambda96(bridgeIndex, priorities(95)),
            cellLambda97(bridgeIndex, priorities(96)),
            cellLambda98(bridgeIndex, priorities(97)),
          )
        )
        rowIndex += 1
      }
  }

  def append99: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)   = cellLambdaGrid(0)
    val (t2, cellLambda2)   = cellLambdaGrid(1)
    val (t3, cellLambda3)   = cellLambdaGrid(2)
    val (t4, cellLambda4)   = cellLambdaGrid(3)
    val (t5, cellLambda5)   = cellLambdaGrid(4)
    val (t6, cellLambda6)   = cellLambdaGrid(5)
    val (t7, cellLambda7)   = cellLambdaGrid(6)
    val (t8, cellLambda8)   = cellLambdaGrid(7)
    val (t9, cellLambda9)   = cellLambdaGrid(8)
    val (t10, cellLambda10) = cellLambdaGrid(9)
    val (t11, cellLambda11) = cellLambdaGrid(10)
    val (t12, cellLambda12) = cellLambdaGrid(11)
    val (t13, cellLambda13) = cellLambdaGrid(12)
    val (t14, cellLambda14) = cellLambdaGrid(13)
    val (t15, cellLambda15) = cellLambdaGrid(14)
    val (t16, cellLambda16) = cellLambdaGrid(15)
    val (t17, cellLambda17) = cellLambdaGrid(16)
    val (t18, cellLambda18) = cellLambdaGrid(17)
    val (t19, cellLambda19) = cellLambdaGrid(18)
    val (t20, cellLambda20) = cellLambdaGrid(19)
    val (t21, cellLambda21) = cellLambdaGrid(20)
    val (t22, cellLambda22) = cellLambdaGrid(21)
    val (t23, cellLambda23) = cellLambdaGrid(22)
    val (t24, cellLambda24) = cellLambdaGrid(23)
    val (t25, cellLambda25) = cellLambdaGrid(24)
    val (t26, cellLambda26) = cellLambdaGrid(25)
    val (t27, cellLambda27) = cellLambdaGrid(26)
    val (t28, cellLambda28) = cellLambdaGrid(27)
    val (t29, cellLambda29) = cellLambdaGrid(28)
    val (t30, cellLambda30) = cellLambdaGrid(29)
    val (t31, cellLambda31) = cellLambdaGrid(30)
    val (t32, cellLambda32) = cellLambdaGrid(31)
    val (t33, cellLambda33) = cellLambdaGrid(32)
    val (t34, cellLambda34) = cellLambdaGrid(33)
    val (t35, cellLambda35) = cellLambdaGrid(34)
    val (t36, cellLambda36) = cellLambdaGrid(35)
    val (t37, cellLambda37) = cellLambdaGrid(36)
    val (t38, cellLambda38) = cellLambdaGrid(37)
    val (t39, cellLambda39) = cellLambdaGrid(38)
    val (t40, cellLambda40) = cellLambdaGrid(39)
    val (t41, cellLambda41) = cellLambdaGrid(40)
    val (t42, cellLambda42) = cellLambdaGrid(41)
    val (t43, cellLambda43) = cellLambdaGrid(42)
    val (t44, cellLambda44) = cellLambdaGrid(43)
    val (t45, cellLambda45) = cellLambdaGrid(44)
    val (t46, cellLambda46) = cellLambdaGrid(45)
    val (t47, cellLambda47) = cellLambdaGrid(46)
    val (t48, cellLambda48) = cellLambdaGrid(47)
    val (t49, cellLambda49) = cellLambdaGrid(48)
    val (t50, cellLambda50) = cellLambdaGrid(49)
    val (t51, cellLambda51) = cellLambdaGrid(50)
    val (t52, cellLambda52) = cellLambdaGrid(51)
    val (t53, cellLambda53) = cellLambdaGrid(52)
    val (t54, cellLambda54) = cellLambdaGrid(53)
    val (t55, cellLambda55) = cellLambdaGrid(54)
    val (t56, cellLambda56) = cellLambdaGrid(55)
    val (t57, cellLambda57) = cellLambdaGrid(56)
    val (t58, cellLambda58) = cellLambdaGrid(57)
    val (t59, cellLambda59) = cellLambdaGrid(58)
    val (t60, cellLambda60) = cellLambdaGrid(59)
    val (t61, cellLambda61) = cellLambdaGrid(60)
    val (t62, cellLambda62) = cellLambdaGrid(61)
    val (t63, cellLambda63) = cellLambdaGrid(62)
    val (t64, cellLambda64) = cellLambdaGrid(63)
    val (t65, cellLambda65) = cellLambdaGrid(64)
    val (t66, cellLambda66) = cellLambdaGrid(65)
    val (t67, cellLambda67) = cellLambdaGrid(66)
    val (t68, cellLambda68) = cellLambdaGrid(67)
    val (t69, cellLambda69) = cellLambdaGrid(68)
    val (t70, cellLambda70) = cellLambdaGrid(69)
    val (t71, cellLambda71) = cellLambdaGrid(70)
    val (t72, cellLambda72) = cellLambdaGrid(71)
    val (t73, cellLambda73) = cellLambdaGrid(72)
    val (t74, cellLambda74) = cellLambdaGrid(73)
    val (t75, cellLambda75) = cellLambdaGrid(74)
    val (t76, cellLambda76) = cellLambdaGrid(75)
    val (t77, cellLambda77) = cellLambdaGrid(76)
    val (t78, cellLambda78) = cellLambdaGrid(77)
    val (t79, cellLambda79) = cellLambdaGrid(78)
    val (t80, cellLambda80) = cellLambdaGrid(79)
    val (t81, cellLambda81) = cellLambdaGrid(80)
    val (t82, cellLambda82) = cellLambdaGrid(81)
    val (t83, cellLambda83) = cellLambdaGrid(82)
    val (t84, cellLambda84) = cellLambdaGrid(83)
    val (t85, cellLambda85) = cellLambdaGrid(84)
    val (t86, cellLambda86) = cellLambdaGrid(85)
    val (t87, cellLambda87) = cellLambdaGrid(86)
    val (t88, cellLambda88) = cellLambdaGrid(87)
    val (t89, cellLambda89) = cellLambdaGrid(88)
    val (t90, cellLambda90) = cellLambdaGrid(89)
    val (t91, cellLambda91) = cellLambdaGrid(90)
    val (t92, cellLambda92) = cellLambdaGrid(91)
    val (t93, cellLambda93) = cellLambdaGrid(92)
    val (t94, cellLambda94) = cellLambdaGrid(93)
    val (t95, cellLambda95) = cellLambdaGrid(94)
    val (t96, cellLambda96) = cellLambdaGrid(95)
    val (t97, cellLambda97) = cellLambdaGrid(96)
    val (t98, cellLambda98) = cellLambdaGrid(97)
    val (t99, cellLambda99) = cellLambdaGrid(98)
    val priorityResolver    = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96, t97, t98, t99
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
            cellLambda95(bridgeIndex, priorities(94)),
            cellLambda96(bridgeIndex, priorities(95)),
            cellLambda97(bridgeIndex, priorities(96)),
            cellLambda98(bridgeIndex, priorities(97)),
            cellLambda99(bridgeIndex, priorities(98)),
          )
        )
        rowIndex += 1
      }
  }

  def append100: (Int, Int, Int => Int) => Unit = {
    val (t1, cellLambda1)     = cellLambdaGrid(0)
    val (t2, cellLambda2)     = cellLambdaGrid(1)
    val (t3, cellLambda3)     = cellLambdaGrid(2)
    val (t4, cellLambda4)     = cellLambdaGrid(3)
    val (t5, cellLambda5)     = cellLambdaGrid(4)
    val (t6, cellLambda6)     = cellLambdaGrid(5)
    val (t7, cellLambda7)     = cellLambdaGrid(6)
    val (t8, cellLambda8)     = cellLambdaGrid(7)
    val (t9, cellLambda9)     = cellLambdaGrid(8)
    val (t10, cellLambda10)   = cellLambdaGrid(9)
    val (t11, cellLambda11)   = cellLambdaGrid(10)
    val (t12, cellLambda12)   = cellLambdaGrid(11)
    val (t13, cellLambda13)   = cellLambdaGrid(12)
    val (t14, cellLambda14)   = cellLambdaGrid(13)
    val (t15, cellLambda15)   = cellLambdaGrid(14)
    val (t16, cellLambda16)   = cellLambdaGrid(15)
    val (t17, cellLambda17)   = cellLambdaGrid(16)
    val (t18, cellLambda18)   = cellLambdaGrid(17)
    val (t19, cellLambda19)   = cellLambdaGrid(18)
    val (t20, cellLambda20)   = cellLambdaGrid(19)
    val (t21, cellLambda21)   = cellLambdaGrid(20)
    val (t22, cellLambda22)   = cellLambdaGrid(21)
    val (t23, cellLambda23)   = cellLambdaGrid(22)
    val (t24, cellLambda24)   = cellLambdaGrid(23)
    val (t25, cellLambda25)   = cellLambdaGrid(24)
    val (t26, cellLambda26)   = cellLambdaGrid(25)
    val (t27, cellLambda27)   = cellLambdaGrid(26)
    val (t28, cellLambda28)   = cellLambdaGrid(27)
    val (t29, cellLambda29)   = cellLambdaGrid(28)
    val (t30, cellLambda30)   = cellLambdaGrid(29)
    val (t31, cellLambda31)   = cellLambdaGrid(30)
    val (t32, cellLambda32)   = cellLambdaGrid(31)
    val (t33, cellLambda33)   = cellLambdaGrid(32)
    val (t34, cellLambda34)   = cellLambdaGrid(33)
    val (t35, cellLambda35)   = cellLambdaGrid(34)
    val (t36, cellLambda36)   = cellLambdaGrid(35)
    val (t37, cellLambda37)   = cellLambdaGrid(36)
    val (t38, cellLambda38)   = cellLambdaGrid(37)
    val (t39, cellLambda39)   = cellLambdaGrid(38)
    val (t40, cellLambda40)   = cellLambdaGrid(39)
    val (t41, cellLambda41)   = cellLambdaGrid(40)
    val (t42, cellLambda42)   = cellLambdaGrid(41)
    val (t43, cellLambda43)   = cellLambdaGrid(42)
    val (t44, cellLambda44)   = cellLambdaGrid(43)
    val (t45, cellLambda45)   = cellLambdaGrid(44)
    val (t46, cellLambda46)   = cellLambdaGrid(45)
    val (t47, cellLambda47)   = cellLambdaGrid(46)
    val (t48, cellLambda48)   = cellLambdaGrid(47)
    val (t49, cellLambda49)   = cellLambdaGrid(48)
    val (t50, cellLambda50)   = cellLambdaGrid(49)
    val (t51, cellLambda51)   = cellLambdaGrid(50)
    val (t52, cellLambda52)   = cellLambdaGrid(51)
    val (t53, cellLambda53)   = cellLambdaGrid(52)
    val (t54, cellLambda54)   = cellLambdaGrid(53)
    val (t55, cellLambda55)   = cellLambdaGrid(54)
    val (t56, cellLambda56)   = cellLambdaGrid(55)
    val (t57, cellLambda57)   = cellLambdaGrid(56)
    val (t58, cellLambda58)   = cellLambdaGrid(57)
    val (t59, cellLambda59)   = cellLambdaGrid(58)
    val (t60, cellLambda60)   = cellLambdaGrid(59)
    val (t61, cellLambda61)   = cellLambdaGrid(60)
    val (t62, cellLambda62)   = cellLambdaGrid(61)
    val (t63, cellLambda63)   = cellLambdaGrid(62)
    val (t64, cellLambda64)   = cellLambdaGrid(63)
    val (t65, cellLambda65)   = cellLambdaGrid(64)
    val (t66, cellLambda66)   = cellLambdaGrid(65)
    val (t67, cellLambda67)   = cellLambdaGrid(66)
    val (t68, cellLambda68)   = cellLambdaGrid(67)
    val (t69, cellLambda69)   = cellLambdaGrid(68)
    val (t70, cellLambda70)   = cellLambdaGrid(69)
    val (t71, cellLambda71)   = cellLambdaGrid(70)
    val (t72, cellLambda72)   = cellLambdaGrid(71)
    val (t73, cellLambda73)   = cellLambdaGrid(72)
    val (t74, cellLambda74)   = cellLambdaGrid(73)
    val (t75, cellLambda75)   = cellLambdaGrid(74)
    val (t76, cellLambda76)   = cellLambdaGrid(75)
    val (t77, cellLambda77)   = cellLambdaGrid(76)
    val (t78, cellLambda78)   = cellLambdaGrid(77)
    val (t79, cellLambda79)   = cellLambdaGrid(78)
    val (t80, cellLambda80)   = cellLambdaGrid(79)
    val (t81, cellLambda81)   = cellLambdaGrid(80)
    val (t82, cellLambda82)   = cellLambdaGrid(81)
    val (t83, cellLambda83)   = cellLambdaGrid(82)
    val (t84, cellLambda84)   = cellLambdaGrid(83)
    val (t85, cellLambda85)   = cellLambdaGrid(84)
    val (t86, cellLambda86)   = cellLambdaGrid(85)
    val (t87, cellLambda87)   = cellLambdaGrid(86)
    val (t88, cellLambda88)   = cellLambdaGrid(87)
    val (t89, cellLambda89)   = cellLambdaGrid(88)
    val (t90, cellLambda90)   = cellLambdaGrid(89)
    val (t91, cellLambda91)   = cellLambdaGrid(90)
    val (t92, cellLambda92)   = cellLambdaGrid(91)
    val (t93, cellLambda93)   = cellLambdaGrid(92)
    val (t94, cellLambda94)   = cellLambdaGrid(93)
    val (t95, cellLambda95)   = cellLambdaGrid(94)
    val (t96, cellLambda96)   = cellLambdaGrid(95)
    val (t97, cellLambda97)   = cellLambdaGrid(96)
    val (t98, cellLambda98)   = cellLambdaGrid(97)
    val (t99, cellLambda99)   = cellLambdaGrid(98)
    val (t100, cellLambda100) = cellLambdaGrid(99)
    val priorityResolver      = getPriorityResolver(
      Seq(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50,
        t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96, t97, t98, t99, t100
      ))
    (rowIndex0: Int, lastRow: Int, indexBridge: Int => Int) =>
      rowIndex = rowIndex0
      var first = true
      while (rowIndex < lastRow) {
        e = 0
        bridgeIndex = indexBridge(rowIndex)
        priorities = priorityResolver(bridgeIndex, first)
        first = false
        tableBody.appendChild(
          _mkRow(
            rowIndex,
            cellLambda1(bridgeIndex, priorities(0)),
            cellLambda2(bridgeIndex, priorities(1)),
            cellLambda3(bridgeIndex, priorities(2)),
            cellLambda4(bridgeIndex, priorities(3)),
            cellLambda5(bridgeIndex, priorities(4)),
            cellLambda6(bridgeIndex, priorities(5)),
            cellLambda7(bridgeIndex, priorities(6)),
            cellLambda8(bridgeIndex, priorities(7)),
            cellLambda9(bridgeIndex, priorities(8)),
            cellLambda10(bridgeIndex, priorities(9)),
            cellLambda11(bridgeIndex, priorities(10)),
            cellLambda12(bridgeIndex, priorities(11)),
            cellLambda13(bridgeIndex, priorities(12)),
            cellLambda14(bridgeIndex, priorities(13)),
            cellLambda15(bridgeIndex, priorities(14)),
            cellLambda16(bridgeIndex, priorities(15)),
            cellLambda17(bridgeIndex, priorities(16)),
            cellLambda18(bridgeIndex, priorities(17)),
            cellLambda19(bridgeIndex, priorities(18)),
            cellLambda20(bridgeIndex, priorities(19)),
            cellLambda21(bridgeIndex, priorities(20)),
            cellLambda22(bridgeIndex, priorities(21)),
            cellLambda23(bridgeIndex, priorities(22)),
            cellLambda24(bridgeIndex, priorities(23)),
            cellLambda25(bridgeIndex, priorities(24)),
            cellLambda26(bridgeIndex, priorities(25)),
            cellLambda27(bridgeIndex, priorities(26)),
            cellLambda28(bridgeIndex, priorities(27)),
            cellLambda29(bridgeIndex, priorities(28)),
            cellLambda30(bridgeIndex, priorities(29)),
            cellLambda31(bridgeIndex, priorities(30)),
            cellLambda32(bridgeIndex, priorities(31)),
            cellLambda33(bridgeIndex, priorities(32)),
            cellLambda34(bridgeIndex, priorities(33)),
            cellLambda35(bridgeIndex, priorities(34)),
            cellLambda36(bridgeIndex, priorities(35)),
            cellLambda37(bridgeIndex, priorities(36)),
            cellLambda38(bridgeIndex, priorities(37)),
            cellLambda39(bridgeIndex, priorities(38)),
            cellLambda40(bridgeIndex, priorities(39)),
            cellLambda41(bridgeIndex, priorities(40)),
            cellLambda42(bridgeIndex, priorities(41)),
            cellLambda43(bridgeIndex, priorities(42)),
            cellLambda44(bridgeIndex, priorities(43)),
            cellLambda45(bridgeIndex, priorities(44)),
            cellLambda46(bridgeIndex, priorities(45)),
            cellLambda47(bridgeIndex, priorities(46)),
            cellLambda48(bridgeIndex, priorities(47)),
            cellLambda49(bridgeIndex, priorities(48)),
            cellLambda50(bridgeIndex, priorities(49)),
            cellLambda51(bridgeIndex, priorities(50)),
            cellLambda52(bridgeIndex, priorities(51)),
            cellLambda53(bridgeIndex, priorities(52)),
            cellLambda54(bridgeIndex, priorities(53)),
            cellLambda55(bridgeIndex, priorities(54)),
            cellLambda56(bridgeIndex, priorities(55)),
            cellLambda57(bridgeIndex, priorities(56)),
            cellLambda58(bridgeIndex, priorities(57)),
            cellLambda59(bridgeIndex, priorities(58)),
            cellLambda60(bridgeIndex, priorities(59)),
            cellLambda61(bridgeIndex, priorities(60)),
            cellLambda62(bridgeIndex, priorities(61)),
            cellLambda63(bridgeIndex, priorities(62)),
            cellLambda64(bridgeIndex, priorities(63)),
            cellLambda65(bridgeIndex, priorities(64)),
            cellLambda66(bridgeIndex, priorities(65)),
            cellLambda67(bridgeIndex, priorities(66)),
            cellLambda68(bridgeIndex, priorities(67)),
            cellLambda69(bridgeIndex, priorities(68)),
            cellLambda70(bridgeIndex, priorities(69)),
            cellLambda71(bridgeIndex, priorities(70)),
            cellLambda72(bridgeIndex, priorities(71)),
            cellLambda73(bridgeIndex, priorities(72)),
            cellLambda74(bridgeIndex, priorities(73)),
            cellLambda75(bridgeIndex, priorities(74)),
            cellLambda76(bridgeIndex, priorities(75)),
            cellLambda77(bridgeIndex, priorities(76)),
            cellLambda78(bridgeIndex, priorities(77)),
            cellLambda79(bridgeIndex, priorities(78)),
            cellLambda80(bridgeIndex, priorities(79)),
            cellLambda81(bridgeIndex, priorities(80)),
            cellLambda82(bridgeIndex, priorities(81)),
            cellLambda83(bridgeIndex, priorities(82)),
            cellLambda84(bridgeIndex, priorities(83)),
            cellLambda85(bridgeIndex, priorities(84)),
            cellLambda86(bridgeIndex, priorities(85)),
            cellLambda87(bridgeIndex, priorities(86)),
            cellLambda88(bridgeIndex, priorities(87)),
            cellLambda89(bridgeIndex, priorities(88)),
            cellLambda90(bridgeIndex, priorities(89)),
            cellLambda91(bridgeIndex, priorities(90)),
            cellLambda92(bridgeIndex, priorities(91)),
            cellLambda93(bridgeIndex, priorities(92)),
            cellLambda94(bridgeIndex, priorities(93)),
            cellLambda95(bridgeIndex, priorities(94)),
            cellLambda96(bridgeIndex, priorities(95)),
            cellLambda97(bridgeIndex, priorities(96)),
            cellLambda98(bridgeIndex, priorities(97)),
            cellLambda99(bridgeIndex, priorities(98)),
            cellLambda100(bridgeIndex, priorities(99)),
          )
        )
        rowIndex += 1
      }
  }
}
