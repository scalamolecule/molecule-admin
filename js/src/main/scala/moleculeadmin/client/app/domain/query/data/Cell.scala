package moleculeadmin.client.app.domain.query.data

import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.edit.{TxLambdas, Update, UpdateCardMany, UpdateCardMap, UpdateCardOne}
import moleculeadmin.client.app.domain.query.marker
import moleculeadmin.client.app.domain.query.marker.Star
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.shared.ast.query.{QueryResult, _}
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{Element, TableCell, TableRow, TableSection}
import org.scalajs.dom.document
import rx.Ctx
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.{JsDom, generic}
import scala.collection.immutable


abstract class Cell(
  db: String,
  tableBody: TableSection,
  cols: Seq[Col],
  qr: QueryResult
)(implicit ctx: Ctx.Owner)
  extends TxLambdas(db) with Update with BodyElements with ColOps {

  // current entity id for updates of subsequent attributes on each row
  var e = 0L

  protected def cellLambda(colIndex: Int): Int => JsDom.TypedTag[TableCell] = {
    val arrayIndex = qr.arrayIndexes(colIndex)

    val Col(_, related, nsAlias, nsFull, attr, attrType, colType,
    card, _, enums, _, expr, _, _) = cols(colIndex)

    lazy val noCount    = expr != "count" && expr != "count-distinct"
    lazy val aggregates = Seq(
      "count", "count-distinct", "sum", "avg", "median", "variance", "stddev"
    )

    val cellType = "" match {
      case _ if aggregates.contains(expr)             => "aggr"
      case _ if attr == "e" && noCount                => "eid"
      case _ if attrType == "ref" && noCount          => "ref"
      case _ if expr == "t"                           => "t"
      case _ if expr == "tx"                          => "tx"
      case _ if expr == "txInstant"                   => "txI"
      case _ if attrType == "String" && enums.isEmpty => "str"
      case _ if attrType == "Date"                    => "date"
      case _ if attrType.startsWith("Big")            => "big"
      case _                                          => ""
    }

    // e has to be first within namespace to allow editing
    val groupEdit = expr == "edit"
    val editable  = groupEdit || isEditable(cols, colIndex, nsAlias, nsFull)
    lazy val showAll   = expr == "orig" || expr == "edit"

    def id: Int => String = idBase(colIndex)

    /**
     * @tparam T cardinality 1: String / Double
     *           cardinality 2: List[String] / List[Double]
     *           cardinality 3: Map[String, String] / Map[String, Double]
     **/
    def save[T](
      origArray: Array[Option[T]],
      valueArray: Array[Option[T]],
      rowIndex: Int,
      baseClass: String
    ): () => Unit = {
      () => {
        val cellId : String    = idBase(colIndex)(rowIndex)
        val oldVOpt: Option[T] = valueArray(rowIndex)
        val isNum  : Boolean   = Seq("Int", "Long", "ref", "Float", "Double").contains(attrType)
        val cell   : TableCell = document.getElementById(cellId).asInstanceOf[TableCell]
        val row    : TableRow  = cell.parentNode.asInstanceOf[TableRow]
        val eid    : Long      = cell.getAttribute("eid").toLong
        val updater            = card match {
          case 1 => UpdateCardOne(
            db, cols, qr, origArray, valueArray, baseClass,
            colType, rowIndex, colIndex, related,
            nsAlias, nsFull, attr, attrType, card, enums, expr
          )
          case 2 => UpdateCardMany(
            db, cols, qr, origArray, valueArray, baseClass,
            colType, rowIndex, colIndex, related,
            nsAlias, nsFull, attr, attrType, card, enums, cellType, expr
          )
          case 3 => UpdateCardMap(
            db, cols, qr, origArray, valueArray, baseClass,
            colType, rowIndex, colIndex, related,
            nsAlias, nsFull, attr, attrType, card, enums, expr
          )
        }
        updater.update(cellId, cell, row, eid, oldVOpt, isNum)
      }
    }

    def getOrigArray[T](arrays: List[Array[Option[T]]]): Array[Option[T]] = {
      if (groupEdit)
        arrays(arrayIndex - 1)
      else
        Array.empty[Option[T]]
    }

    def getClassLambda[T](
      origArray: Array[Option[T]],
      valueArray: Array[Option[T]]
    ): (String, Int) => String = {
      if (groupEdit)
        (baseClass: String, rowIndex: Int) => {
          val oldV = origArray(rowIndex)
          val newV = valueArray(rowIndex)
          if (oldV == newV)
            baseClass
          else
            newV match {
              case None                    => s"$baseClass retract"
              case Some(_) if oldV.isEmpty => s"$baseClass assert"
              case Some(_)                 => s"$baseClass update"
            }
        }
      else
        (baseClass: String, _: Int) => baseClass
    }

    lazy val eColIndexes = cols.collect {
      case Col(colIndex, _, _, _, "e", _, _, _, _, _, _, _, _, _) => colIndex + 1
    }

    colType match {

      // card one --------------------------------------------------------------

      case "string" =>
        val origArray  = getOrigArray(qr.str)
        val valueArray = qr.str(arrayIndex)
        val getCls     = getClassLambda(origArray, valueArray)
        cellType match {
          case "str" if editable =>
            (rowIndex: Int) =>
              _tdOneStrEdit(
                getCls("str", rowIndex),
                id(rowIndex), e, valueArray(rowIndex),
                save(origArray, valueArray, rowIndex, "str")
              )

          case "date" if editable =>
            (rowIndex: Int) =>
              _tdOneDateEdit(
                getCls("date", rowIndex),
                id(rowIndex), e, valueArray(rowIndex),
                save(origArray, valueArray, rowIndex, "date")
              )

          case "big" if editable =>
            (rowIndex: Int) =>
              _tdOneNumEdit(
                getCls("num", rowIndex),
                id(rowIndex), e, valueArray(rowIndex),
                save(origArray, valueArray, rowIndex, "num")
              )

          case _ if editable =>
            (rowIndex: Int) =>
              _tdOneEdit(
                getCls("", rowIndex),
                id(rowIndex), e, valueArray(rowIndex),
                save(origArray, valueArray, rowIndex, "")
              )

          case "str" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(s =>
                _tdNoEdit(_str2frags(s)))

          case "aggr" =>
            val array = qr.num(arrayIndex)
            (rowIndex: Int) =>
              array(rowIndex).fold(_tdNoAggrEdit)(
                _tdOneNumNoAggrEdit(_))


          case "date" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit) { d =>


                _tdOneDate(truncateDateStr(d))
              }

          case "big" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdOneNumNoEdit(_))

          case "txI" =>
            txInstantLambda(qr, arrayIndex, cols, colIndex)

          case _ =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdNoEdit(_))
        }


      case "double" =>


        def toggleFlag(eid: Long, elem: Element): Unit = {
          val on = curFlags.contains(eid)
          if (on) {
            elem.className = "far fa-star starOff"
            curFlags = curFlags.filterNot(_ == eid)
          } else {
            elem.className = "fas fa-star starOn"
            curFlags = eid :: curFlags
          }
        }
        def toggleCheck(eid: Long, elem: Element): Unit = {
          val on = curFlags.contains(eid)
          if (on) {
            elem.className = "far fa-star starOff"
            curFlags = curFlags.filterNot(_ == eid)
          } else {
            elem.className = "fas fa-star starOn"
            curFlags = eid :: curFlags
          }
        }

        val origArray  = getOrigArray(qr.num)
        val valueArray = qr.num(arrayIndex)
        val getCls     = getClassLambda(origArray, valueArray)
        cellType match {
          case "eid" =>


            (rowIndex: Int) =>
              // Set entity id for updates of subsequent attribute values
              e = valueArray(rowIndex).fold(0L)(_.toLong)
              val eid     = e
              val star = Star(tableBody, eColIndexes, eid)

//              val starred = if (curStars.contains(e))
//                "fas fa-star starOn" else "far fa-star starOff"
//              val flagged = if (curFlags.contains(e))
//                "fas fa-flag flagOn" else "far fa-flag flagOff"
//              val checked = if (curChecks.contains(e))
//                "oi oi-check checkOn" else "fas fa-check checkOff"
              _tdOneEid(
                eid,
                curEntity,
                () => curEntity() = eid,
//                star.curCls,
//                () => star.toggle,
//
//                "far fa-flag flagOff",
////                flagged,
//                (elem: Element) => toggleFlag(eid, elem),
//
//                "fas fa-check checkOff",
////                checked,
//                (elem: Element) => toggleCheck(eid, elem),
              )


          case "ref" if groupEdit =>
            (rowIndex: Int) =>
              _tdOneRefEdit2(
                id(rowIndex),
                e,
                valueArray(rowIndex),
                save(origArray, valueArray, rowIndex, "num")
              )

          case "ref" if editable =>
            (rowIndex: Int) =>
              _tdOneRefEdit(
                id(rowIndex),
                e,
                valueArray(rowIndex),
                curEntity,
                (ref: Long) => () => curEntity() = ref,
                save(origArray, valueArray, rowIndex, "num")
              )

          case _ if editable =>
            (rowIndex: Int) =>
              _tdOneNumEdit(
                getCls("num", rowIndex),
                id(rowIndex), e, valueArray(rowIndex),
                save(origArray, valueArray, rowIndex, "num")
              )

          case "ref" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(v =>
                _tdOneRef(
                  v.toLong,
                  curEntity,
                  (eid: Long) => () => curEntity() = eid
                )
              )

          case "aggr" => (rowIndex: Int) =>
            valueArray(rowIndex).fold(_tdNoAggrEdit)(_tdOneNumNoAggrEdit(_))

          case "t"  => tLambda(qr, arrayIndex, cols, colIndex)
          case "tx" => txLambda(qr, arrayIndex, cols, colIndex)

          case _ =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(_tdOneNumNoEdit(_))
        }


      // card many -------------------------------------------------------------

      case "listString" =>
        val origArray  = getOrigArray(qr.listStr)
        val valueArray = qr.listStr(arrayIndex)
        val getCls     = getClassLambda(origArray, valueArray)
        cellType match {
          case "str" if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyStringEdit(
                  vs, getCls("items", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "items")
                )
              )

          case "date" if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyDateEdit(
                  vs, getCls("str", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "str")
                )
              )

          case "big" if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyStringBigEdit(
                  vs, getCls("num", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "num")
                )
              )

          case _ if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyStringOtherEdit(
                  vs, getCls("str", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "str")
                )
              )

          case "str" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdManyString(_, "items", showAll))

          case "date" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdManyDate(_, showAll))

          case "big" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdManyString(_, "num", showAll))

          case _ =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdManyString(_, "str", showAll))
        }

      case "listDouble" =>
        val origArray  = getOrigArray(qr.listNum)
        val valueArray = qr.listNum(arrayIndex)
        val getCls     = getClassLambda(origArray, valueArray)
        cellType match {
          case "eid" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyRef(vs, curEntity,
                  (eid: Long) => () => curEntity() = eid))

          case "ref" if groupEdit =>
            println("_tdManyRefEdit2")
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyRefEdit2(
                  vs,
                  getCls("num", rowIndex),
                  id(rowIndex),
                  e,
                  curEntity,
                  (ref: Long) => () => curEntity() = ref,
                  save(origArray, valueArray, rowIndex, "")
                )
              )
          case "ref" if editable  =>
            println("_tdManyRefEdit")
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyRefEdit(
                  vs,
                  id(rowIndex),
                  e,
                  curEntity,
                  (ref: Long) => () => curEntity() = ref,
                  save(origArray, valueArray, rowIndex, "")
                )
              )

          case "ref" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyRef(vs, curEntity,
                  (eid: Long) => () => curEntity() = eid, true))

          case _ if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdManyDoubleEdit(
                  vs, getCls("num", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "num")
                )
              )

          case _ =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(
                _tdManyDouble(_, showAll))
        }


      // map -------------------------------------------------------------------

      case "mapString" =>
        val origArray  = getOrigArray(qr.mapStr)
        val valueArray = qr.mapStr(arrayIndex)
        val getCls     = getClassLambda(origArray, valueArray)
        cellType match {
          case "str" if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdMapStrEdit(
                  vs, getCls("items", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "items")
                )
              )

          case "date" if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdMapDateEdit(
                  vs, getCls("str", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "str")
                )
              )

          case _ if editable =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(vs =>
                _tdMapStrOtherEdit(
                  vs, getCls("str", rowIndex), id(rowIndex), e,
                  save(origArray, valueArray, rowIndex, "str")
                )
              )

          case "date" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(_tdMapDate)

          case "str" =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(_tdMapStr)

          case _ =>
            (rowIndex: Int) =>
              valueArray(rowIndex).fold(_tdNoEdit)(_tdMapStrOther)
        }

      case "mapDouble" =>
        val origArray  = getOrigArray(qr.mapNum)
        val valueArray = qr.mapNum(arrayIndex)
        val getCls     = getClassLambda(origArray, valueArray)
        if (editable) {
          rowIndex: Int =>
            valueArray(rowIndex).fold(_tdNoEdit)(vs =>
              _tdMapDoubleEdit(
                vs, getCls("str", rowIndex), id(rowIndex), e,
                save(origArray, valueArray, rowIndex, "str")
              )
            )
        } else {
          rowIndex: Int =>
            valueArray(rowIndex).fold(_tdNoEdit)(_tdMapDouble)
        }
    }
  }
}
