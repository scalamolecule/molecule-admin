package moleculeadmin.clienttest.query.grid

import moleculeadmin.shared.ast.query.Col
import utest._

trait Base extends TestSuite {

  var cols        = Seq.empty[Col]
  var rowIndex    = 0
  var bridgeIndex = 0
  var priorities  = Seq.empty[String]
  var prev1       = ""
  var prev2       = ""
  var prev3       = ""
  var prev4       = ""
  var prev5       = ""
  var v1          = ""
  var v2          = ""
  var v3          = ""
  var v4          = ""
  var v5          = ""
  var pr          = (i: Int, first: Boolean) => Seq.empty[String]

  //  def cssPrio(level: Int) = " prio" + (6 - level)
  def cssPrio(depth: Int, level: Int) =
    if (level == 0) "" else "p" + (5 - depth + level)

  def init() = {
    rowIndex = 0
    bridgeIndex = 0
    priorities = Seq.empty[String]
    prev1 = ""
    prev2 = ""
    prev3 = ""
    prev4 = ""
    prev5 = ""
    v1 = ""
    v2 = ""
    v3 = ""
    v4 = ""
    v5 = ""
  }

  /*
  Note that col.sortPos 1 is "highest level/priority", col.sortPos 2 a
  "lower level/priority" and so on. So col.sortPos and "level"/"priority" are
  inversely proportional to each other.
   */

  def getPriorityResolver(
    gridType: Int,
    valueLambdas: Seq[Int => String]
  ): (Int, Boolean) => Seq[String] = {
    init()
    val levelCount = {
      val max = cols.map(_.sortPos).max
      if (max == 0) 1 else max
    }

    if (levelCount == 1) {
      val valueLambda = cols.collect {
        case c if c.sortDir.nonEmpty => valueLambdas(c.colIndex)
      }.head

      (bridgeIndex: Int, first: Boolean) => {
        v1 = valueLambda(bridgeIndex)
        val changed = v1 != prev1
        prev1 = v1
        val prio = if (changed) cssPrio(1, 1) else ""
        // Same level for all columns
        if (first)
          Seq.fill(cols.length)("")
        else
          Seq.fill(cols.length)(prio)
      }

    } else {
      val depth = cols.map(_.sortPos).max

      (bridgeIndex: Int, first: Boolean) => {
        if (first) {
          // Set values that second row can compare to
          cols.collect {
            case c if c.sortPos == 1 => prev1 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 2 => prev2 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 3 => prev3 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 4 => prev4 = valueLambdas(c.colIndex)(bridgeIndex)
            case c if c.sortPos == 5 => prev5 = valueLambdas(c.colIndex)(bridgeIndex)
          }
          Seq.fill(cols.length)("")
        } else {
          // Determine changed columns first
          val (changedLevels, changed) = cols.foldLeft(
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
              Seq.fill(cols.length)(cssPrio(depth, 1))
            case topChangeLevel =>
              gridType match {
                case 1 =>
                  cols.foldLeft(0, Seq.empty[String]) {
                    case ((0, acc), c) if changed(c.colIndex) =>
                      // This or lower level marked
                      (c.sortPos, acc :+ cssPrio(depth, c.sortPos))

                    case ((0, acc), _) =>
                      // Not yet changed
                      (0, acc :+ "")

                    case ((l, acc), c) if changed(c.colIndex) && c.sortPos < l =>
                      // Higher priority takes new precedence
                      (c.sortPos, acc :+ cssPrio(depth, c.sortPos))

                    case ((l, acc), _) =>
                      // Continue on same level
                      (l, acc :+ cssPrio(depth, l))
                  }._2

                case 2 =>
                  // Sub level changed, mark all with highest sub level
                  Seq.fill(cols.length)(cssPrio(depth, topChangeLevel))
              }
          }
        }
      }
    }
  }

  val __ = ""
  val p1 = "p1"
  val p2 = "p2"
  val p3 = "p3"
  val p4 = "p4"
  val p5 = "p5"

  def mkCols(pos: Int*) = pos.zipWithIndex.map {
    case (6, i) => Col(i, 0, "Ns", "Ns", "int", "Int", "double", 1, false, Seq(), "", "", "asc", 0, "", "")
    case (0, i) => Col(i, 0, "Ns", "Ns", "int", "Int", "double", 1, false, Seq(), "", "", "", 0, "", "")
    case (p, i) => Col(i, 0, "Ns", "Ns", "int", "Int", "double", 1, false, Seq(), "", "", "asc", p, "", "")
  }

  def priorityResolver(
    data: Seq[Seq[Int]],
    gridType: Int
  ): (Int, Boolean) => Seq[String]

  def resolve(gridType: Int, data: Seq[Int]*): Seq[Seq[String]] = {
    pr = priorityResolver(data, gridType)
    pr(0, true) +: data.indices.tail.map(i => pr(i, false))
  }
}
