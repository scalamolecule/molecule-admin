package moleculeadmin.client.app.domain.query.marker
import moleculeadmin.client.app.domain.query.QueryState.curStars
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.{Element, TableSection}

case class Star(
  tableBody: TableSection,
  eColIndexes: Seq[Int],
  eid: Long
) {

  val onCls  = "fas fa-star starOn"
  val offCls = "far fa-star starOff"

  val curCls = if (curStars.contains(eid)) onCls else offCls

  val eidStr = eid.toString

  def toggle: Unit = {
    val isOn = curStars.contains(eid)
    val newCls = if (curStars.contains(eid)) offCls else onCls

    if (isOn) {
      curStars = curStars.filterNot(_ == eid)
    } else {
      curStars = eid :: curStars
    }

    val rows = tableBody.children
    var i    = 0

    eColIndexes.length match {
      case 1 =>
        val c1 = eColIndexes.head
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)
          i += 1
        }

      case 2 =>
        val Seq(c1, c2) = eColIndexes
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)

          val cell2 = rows(i).children(c2)
          if (cell2.innerText == eidStr)
            cell2.children(0).setAttribute("class", newCls)

          i += 1
        }

      case 3 =>
        val Seq(c1, c2, c3) = eColIndexes
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)

          val cell2 = rows(i).children(c2)
          if (cell2.innerText == eidStr)
            cell2.children(0).setAttribute("class", newCls)

          val cell3 = rows(i).children(c3)
          if (cell3.innerText == eidStr)
            cell3.children(0).setAttribute("class", newCls)

          i += 1
        }

      case 4 =>
        val Seq(c1, c2, c3, c4) = eColIndexes
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)

          val cell2 = rows(i).children(c2)
          if (cell2.innerText == eidStr)
            cell2.children(0).setAttribute("class", newCls)

          val cell3 = rows(i).children(c3)
          if (cell3.innerText == eidStr)
            cell3.children(0).setAttribute("class", newCls)

          val cell4 = rows(i).children(c4)
          if (cell4.innerText == eidStr)
            cell4.children(0).setAttribute("class", newCls)

          i += 1
        }

      case 5 =>
        val Seq(c1, c2, c3, c4, c5) = eColIndexes
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)

          val cell2 = rows(i).children(c2)
          if (cell2.innerText == eidStr)
            cell2.children(0).setAttribute("class", newCls)

          val cell3 = rows(i).children(c3)
          if (cell3.innerText == eidStr)
            cell3.children(0).setAttribute("class", newCls)

          val cell4 = rows(i).children(c4)
          if (cell4.innerText == eidStr)
            cell4.children(0).setAttribute("class", newCls)

          val cell5 = rows(i).children(c5)
          if (cell5.innerText == eidStr)
            cell5.children(0).setAttribute("class", newCls)

          i += 1
        }

      case 6 =>
        val Seq(c1, c2, c3, c4, c5, c6) = eColIndexes
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)

          val cell2 = rows(i).children(c2)
          if (cell2.innerText == eidStr)
            cell2.children(0).setAttribute("class", newCls)

          val cell3 = rows(i).children(c3)
          if (cell3.innerText == eidStr)
            cell3.children(0).setAttribute("class", newCls)

          val cell4 = rows(i).children(c4)
          if (cell4.innerText == eidStr)
            cell4.children(0).setAttribute("class", newCls)

          val cell5 = rows(i).children(c5)
          if (cell5.innerText == eidStr)
            cell5.children(0).setAttribute("class", newCls)

          val cell6 = rows(i).children(c6)
          if (cell6.innerText == eidStr)
            cell6.children(0).setAttribute("class", newCls)

          i += 1
        }

      case 7 =>
        val Seq(c1, c2, c3, c4, c5, c6, c7) = eColIndexes
        while (i < rows.length) {
          val cell1 = rows(i).children(c1)
          if (cell1.innerText == eidStr)
            cell1.children(0).setAttribute("class", newCls)

          val cell2 = rows(i).children(c2)
          if (cell2.innerText == eidStr)
            cell2.children(0).setAttribute("class", newCls)

          val cell3 = rows(i).children(c3)
          if (cell3.innerText == eidStr)
            cell3.children(0).setAttribute("class", newCls)

          val cell4 = rows(i).children(c4)
          if (cell4.innerText == eidStr)
            cell4.children(0).setAttribute("class", newCls)

          val cell5 = rows(i).children(c5)
          if (cell5.innerText == eidStr)
            cell5.children(0).setAttribute("class", newCls)

          val cell6 = rows(i).children(c6)
          if (cell6.innerText == eidStr)
            cell6.children(0).setAttribute("class", newCls)

          val cell7 = rows(i).children(c7)
          if (cell7.innerText == eidStr)
            cell7.children(0).setAttribute("class", newCls)

          i += 1
        }

      case n =>
        throw new IllegalArgumentException(
          "Can only mark up to 7 entity id columns. Found " + n
        )
    }




  }
}
