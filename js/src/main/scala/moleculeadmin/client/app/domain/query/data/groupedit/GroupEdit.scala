package moleculeadmin.client.app.domain.query.data.groupedit
import java.net.URI
import java.util.{Date, UUID}
import moleculeadmin.client.app.domain.query.KeyEvents
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.domain.query.data.Indexes
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps
import org.scalajs.dom.html.{LI, TableCell}
import org.scalajs.dom.{Node, NodeList, document}
import rx.Ctx
import scalafiddle.ScalafiddleApi
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.collection.{GenMap, mutable}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.{Dictionary, WrappedDictionary}

/*
  Float is treated as Double to avoid precision problems from ScalaJS

  Long is treated as Double since it is opaque in ScalaJS
  see: https://stackoverflow.com/a/27823467/1211032


  To get precision correct, use BigDecimal and cast to Double (used for
  all number types except Int):
  floats.map(v => (v + BigDecimal(0.1)).toDouble)
 */
case class GroupEdit(col: Col,
                     filterId: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with ColOps with BodyElements with KeyEvents {

  val Col(colIndex, _, nsAlias, nsFull, attr, attrType, colType, card, _, _,
  aggrType, expr, sortDir, sortPos) = col

  val qr = queryCache.now.find(_.modelElements == modelElements.now).get.queryResult

  val arrayIndexes = qr.arrayIndexes


  def getAttrLambda(colType: String,
                    colIndex: Int,
                    attrType: String,
                    opt: Boolean): Int => Any = colType match {
    case "string" =>
      val array = qr.str(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"     => (j: Int) => array(j)
          case "Boolean"    => (j: Int) => array(j).fold(Option.empty[Boolean])(v => Some(v.toBoolean))
          case "Date"       => (j: Int) => array(j).fold(Option.empty[Date])(v => Some(str2date(v)))
          case "UUID"       => (j: Int) => array(j).fold(Option.empty[UUID])(v => Some(UUID.fromString(v)))
          case "URI"        => (j: Int) => array(j).fold(Option.empty[URI])(v => Some(new URI(v)))
          case "BigInt"     => (j: Int) => array(j)
          case "BigDecimal" => (j: Int) => array(j)
        }
      else
        attrType match {
          case "String"     => (j: Int) => array(j).get
          case "Boolean"    => (j: Int) => array(j).get.toBoolean
          case "Date"       => (j: Int) => str2date(array(j).get)
          case "UUID"       => (j: Int) => UUID.fromString(array(j).get)
          case "URI"        => (j: Int) => new URI(array(j).get)
          case "BigInt"     => (j: Int) => array(j).get
          case "BigDecimal" => (j: Int) => array(j).get
        }

    case "double" =>
      val array = qr.num(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) => array(j).fold(Option.empty[Int])(v => Some(v.toInt))
          case _     => (j: Int) => array(j).fold(Option.empty[String])(v => Some(v.toString))
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.toInt
          case _     => (j: Int) => array(j).get.toString
        }

    case "listString" =>
      val array = qr.listStr(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"     => (j: Int) => array(j)
          case "Boolean"    => (j: Int) => array(j).fold(Option.empty[List[Boolean]])(vs => Some(vs.map(_.toBoolean)))
          case "Date"       => (j: Int) => array(j).fold(Option.empty[List[Date]])(vs => Some(vs.map(v => str2date(v))))
          case "UUID"       => (j: Int) => array(j).fold(Option.empty[List[UUID]])(vs => Some(vs.map(v => UUID.fromString(v))))
          case "URI"        => (j: Int) => array(j).fold(Option.empty[List[URI]])(vs => Some(vs.map(v => new URI(v))))
          case "BigInt"     => (j: Int) => array(j)
          case "BigDecimal" => (j: Int) => array(j)
        }
      else
        attrType match {
          case "String"     => (j: Int) => array(j).get
          case "Boolean"    => (j: Int) => array(j).get.map(_.toBoolean)
          case "Date"       => (j: Int) => array(j).get.map(v => str2date(v))
          case "UUID"       => (j: Int) => array(j).get.map(v => UUID.fromString(v))
          case "URI"        => (j: Int) => array(j).get.map(v => new URI(v))
          case "BigInt"     => (j: Int) => array(j).get
          case "BigDecimal" => (j: Int) => array(j).get
        }

    case "listDouble" =>
      val array = qr.listNum(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) => array(j).fold(Option.empty[List[Int]])(vs => Some(vs.map(_.toInt)))
          case _     => (j: Int) => array(j).fold(Option.empty[List[String]])(vs => Some(vs.map(_.toString)))
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.map(_.toInt)
          case _     => (j: Int) => array(j).get.map(_.toString)
        }

    case "mapString" =>
      val array = qr.mapStr(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "String"     => (j: Int) => array(j)
          case "Boolean"    => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Boolean]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toBoolean }))
          case "Date"       => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Date]])(pairs =>
                Some(pairs.map { case (k, v) => k -> str2date(v) }))
          case "UUID"       => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, UUID]])(pairs =>
                Some(pairs.map { case (k, v) => k -> UUID.fromString(v) }))
          case "URI"        => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, URI]])(pairs =>
                Some(pairs.map { case (k, v) => k -> new URI(v) }))
          case "BigInt"     => (j: Int) => array(j)
          case "BigDecimal" => (j: Int) => array(j)
        }
      else
        attrType match {
          case "String"     => (j: Int) => array(j).get
          case "Boolean"    => (j: Int) => array(j).get.map { case (k, v) => k -> v.toBoolean }
          case "Date"       => (j: Int) => array(j).get.map { case (k, v) => k -> str2date(v) }
          case "UUID"       => (j: Int) => array(j).get.map { case (k, v) => k -> UUID.fromString(v) }
          case "URI"        => (j: Int) => array(j).get.map { case (k, v) => k -> new URI(v) }
          case "BigInt"     => (j: Int) => array(j).get
          case "BigDecimal" => (j: Int) => array(j).get
        }

    case "mapDouble" =>
      val array = qr.mapNum(arrayIndexes(colIndex))
      if (opt)
        attrType match {
          case "Int" => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, Int]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toInt }))
          case _     => (j: Int) =>
            array(j)
              .fold(Option.empty[Map[String, String]])(pairs =>
                Some(pairs.map { case (k, v) => k -> v.toString }))
        }
      else
        attrType match {
          case "Int" => (j: Int) => array(j).get.map { case (k, v) => k -> v.toInt }
          case _     => (j: Int) => array(j).get.map { case (k, v) => k -> v.toString }
        }
  }

  val lhsTypes       = new ListBuffer[String]
  val lhsParamsTypes = new ListBuffer[String]
  val lhsParams      = new ListBuffer[String]
  val colTypes       = new ListBuffer[String]
  val vars           = new ListBuffer[String]
  val varAssignments = new ListBuffer[String]
  val colIndexes     = new ListBuffer[Int]
  val attrLambdas    = new ListBuffer[Int => Any]

  def attrTokens(attr: String, tpe: String, card: Int)
  : (String, String, String) = card match {
    case 1 if attr.last == '$' =>
      tpe match {
        case "Long" | "datom" | "ref" => (
          "Option[String)",
          "Option[BigInt] = Option.empty[BigInt]",
          s"_$attr.fold(Option.empty[BigInt])(v => Some(BigInt(v))"
        )

        case "Float" | "Double" => (
          "Option[String]",
          "Option[BigDecimal] = Option.empty[BigInt]",
          s"_$attr.fold(Option.empty[BigDecimal])(v => Some(BigDecimal(v))"
        )

        // Int + String types
        case _ => (
          s"Option[$tpe]",
          s"Option[$tpe] = Option.empty[$tpe]",
          s"_$attr"
        )
      }

    case 1 =>
      tpe match {
        case "Long" | "datom" | "ref" =>
          ("String", "BigInt = BigInt(0)", s"BigInt(_$attr)")

        case "Float" | "Double" =>
          ("String", "BigDecimal = BigDecimal(0)", s"BigDecimal(_$attr)")

        // Int + String types
        case "Int"        => (tpe, "Int = 0", s"_$attr")
        case "String"     => (tpe, """String = """"", s"_$attr")
        case "Boolean"    => (tpe, "Boolean = false", s"_$attr")
        case "Date"       => (tpe, "Date = new Date()", s"_$attr")
        case "UUID"       => (tpe, "UUID = UUID.randomUUID()", s"_$attr")
        case "URI"        => (tpe, """URI = new URI("")""", s"_$attr")
        case "BigInt"     => ("String", "BigInt = BigInt(0)", s"BigInt(_$attr)")
        case "BigDecimal" => ("String", "BigDecimal = BigDecimal(0)", s"BigDecimal(_$attr)")
      }

    case 2 =>
      tpe match {
        case "Int"                    => (
          "List[Int]",
          "js.Array[Int] = new js.Array()",
          s"_$attr.toJSArray"
        )
        case "Long" | "datom" | "ref" => (
          "List[String]",
          "js.Array[BigInt] = new js.Array()",
          s"_$attr.toJSArray.map(v => BigInt(v))"
        )
        case "Float" | "Double"       => (
          "List[String]",
          "js.Array[BigDecimal] = new js.Array()",
          s"_$attr.toJSArray.map(v => BigDecimal(v))"
        )
        case "BigInt"                 => (
          "List[String]",
          "js.Array[BigInt] = new js.Array()",
          s"_$attr.toJSArray.map(v => BigInt(v))"
        )
        case "BigDecimal"             => (
          "List[String]",
          "js.Array[BigDecimal] = new js.Array()",
          s"_$attr.toJSArray.map(v => BigDecimal(v))"
        )
        case _                        => (
          s"List[$tpe]",
          s"js.Array[$tpe] = new js.Array()",
          s"_$attr.toJSArray",
        )
      }

    case 3 =>
      tpe match {
        case "Int"                    => (
          "Map[String, Int]",
          "js.Dictionary[Int] = js.Dictionary.empty[Int]",
          s"_$attr.toJSDictionary"
        )
        case "Long" | "datom" | "ref" => (
          "Map[String, String]",
          "js.Dictionary[BigInt] = js.Dictionary.empty[BigInt]",
          s"_$attr.toJSDictionary.map { case (k, v) => k -> BigInt(v) }"
        )
        case "Float" | "Double"       => (
          "Map[String, String]",
          "js.Dictionary[BigDecimal] = js.Dictionary.empty[BigDecimal]",
          s"_$attr.toJSDictionary.map { case (k, v) => k -> BigDecimal(v) }"
        )
        case "BigInt"                 => (
          "Map[String, String]",
          "js.Dictionary[BigInt] = js.Dictionary.empty[BigInt]",
          s"_$attr.toJSDictionary.map { case (k, v) => k -> BigInt(v) }"
        )
        case "BigDecimal"             => (
          "Map[String, String]",
          "js.Dictionary[BigDecimal] = js.Dictionary.empty[BigDecimal]",
          s"_$attr.toJSDictionary.map { case (k, v) => k -> BigDecimal(v) }"
        )
        case _                        => (
          s"Map[String, $tpe]",
          s"""js.Dictionary[$tpe] = js.Dictionary.empty[$tpe]""",
          s"_$attr.toJSDictionary",
        )
      }
  }

  columns.now.foreach {
    case Col(colIndex, _, `nsAlias`, `nsFull`, attr, tpe, colType, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>
      val (tpe1, tpe2, assignment) = attrTokens(attr, tpe, card)
      lhsTypes += tpe1
      lhsParamsTypes += s"_$attr: $tpe1"
      lhsParams += "_" + attr
      vars += s"var $attr: $tpe2"
      varAssignments += s"$attr = $assignment"
      colTypes += colType
      colIndexes += colIndex
      attrLambdas += getAttrLambda(colType, colIndex, tpe, attr.last == '$')

    case Col(colIndex, _, nsAlias, _, attr, tpe, colType, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>
      val Ns_attr                  = nsAlias + "_" + attr
      val (tpe1, tpe2, assignment) = attrTokens(Ns_attr, tpe, card)
      lhsTypes += tpe1
      lhsParamsTypes += s"_$Ns_attr: $tpe1"
      lhsParams += "_" + Ns_attr
      vars += s"var $Ns_attr: $tpe2"
      varAssignments += s"$Ns_attr = $assignment"
      colTypes += colType
      colIndexes += colIndex
      attrLambdas += getAttrLambda(colType, colIndex, tpe, attr.last == '$')

    case other =>
    //      throw new RuntimeException("Unexpected col: " + col)
  }

  // Get entered scala right hand side code (before processing adds spinner!)
  val rhs = _html2str(document.getElementById(filterId).innerHTML)
    .trim.replaceAllLiterally("\n", "\n      ")

  // Start spinner since compilation can take some seconds
  processing() = filterId

  val origIndex = arrayIndexes(colIndex - 1)
  val editIndex = arrayIndexes(colIndex)

  val tableRows           = document.getElementById("tableBody").childNodes
  var tableRowIndexOffset = offset.now
  var tableRowIndexMax    = curLastRow

  val sortCols                 = columns.now.filter(_.sortDir.nonEmpty)
  val unfiltered               = filters.now.isEmpty
  val (sortIndex, filterIndex) = Indexes(qr, sortCols, unfiltered).get
  val lastRow                  = actualRowCount

  val indexBridge: Int => Int = {
    if (filterIndex.nonEmpty)
      (i: Int) => filterIndex(i)
    else if (sortIndex.nonEmpty)
      (i: Int) => sortIndex(i)
    else
      (i: Int) => i
  }

  // Update visible cell values and mark edge color
  def updateCellCardOne[ColType](cellBaseClass: String,
                                 colValueToNode: ColType => Node
                                ): (Int, Option[ColType], Option[ColType]) => Unit = {
    var cells   : NodeList  = null
    var editCell: TableCell = null
    val editColIndex        = colIndex + 1
    val formatValue         = attrType match {
      case "Date" => (optV: Option[ColType]) =>
        optV.fold(Option.empty[ColType])(v =>
          Some(truncateDateStr(v.toString).asInstanceOf[ColType])
        )
      case _      => (optV: Option[ColType]) => optV
    }
    (tableRowIndex: Int,
     oldVopt0: Option[ColType],
     newVopt: Option[ColType]
    ) => {
      val oldVopt = formatValue(oldVopt0)
      cells = tableRows.item(tableRowIndex).childNodes
      editCell = cells.item(editColIndex).asInstanceOf[TableCell]
      editCell.innerHTML = ""
      newVopt match {
        case None if oldVopt != newVopt =>
          editCell.className = s"$cellBaseClass retract"
        case None                       =>
          editCell.className = cellBaseClass
        case Some(v) if oldVopt.isEmpty =>
          editCell.className = s"$cellBaseClass assert"
          editCell.appendChild(colValueToNode(v))

        case Some(v) if oldVopt != newVopt =>
          editCell.className = s"$cellBaseClass update"
          editCell.appendChild(colValueToNode(v))
        case Some(v)                       =>
          editCell.className = cellBaseClass
          editCell.appendChild(colValueToNode(v))
      }
    }
  }

  def updateCellCardMany[ColType](cellBaseClass: String,
                                  colValueToItems: ColType => Seq[TypedTag[LI]]
                                 ): (Int, Option[ColType], Option[ColType]) => Unit = {
    var cells   : NodeList  = null
    var editCell: TableCell = null
    val editColIndex        = colIndex + 1
    val formatValue         = attrType match {
      case "Date" if card == 3 => (optV: Option[ColType]) =>
        optV.fold(Option.empty[ColType])(pairs =>
          Some(
            pairs.asInstanceOf[Map[String, String]]
              .map { case (k, v) => {

                println("--------------")
                println("cell date 3: " + v)
                println("cell date 3: " + truncateDateStr(v))
                k -> truncateDateStr(v)
              }
              }
          ).asInstanceOf[Option[ColType]]
        )
      case "Date"              => (optV: Option[ColType]) =>
        optV.fold(Option.empty[ColType])(vs =>
          Some(
            vs.asInstanceOf[List[String]].map(v => {

              println("--------------")
              println("cell date 2: " + v)
              println("cell date 2: " + truncateDateStr(v))
              truncateDateStr(v)
            })
          ).asInstanceOf[Option[ColType]]
        )
      case _                   => (optV: Option[ColType]) => optV
    }

    (tableRowIndex: Int,
     oldVopt0: Option[ColType],
     newVopt: Option[ColType]
    ) => {
      val oldVopt = formatValue(oldVopt0)

      println("oldVopt0: " + oldVopt0)
      println("oldVopt : " + oldVopt)
      println("newVopt : " + newVopt)
      println("newVopt_: " + colValueToItems(newVopt.get))

      cells = tableRows.item(tableRowIndex).childNodes
      editCell = cells.item(editColIndex).asInstanceOf[TableCell]
      editCell.innerHTML = ""
      newVopt match {
        case None if oldVopt != newVopt    =>
          editCell.className = s"$cellBaseClass retract"
        case None                          =>
          editCell.className = cellBaseClass
        case Some(v) if oldVopt.isEmpty    =>
          editCell.className = s"$cellBaseClass assert"
          editCell.appendChild(ul(colValueToItems(v)).render)
        case Some(v) if oldVopt != newVopt =>
          editCell.className = s"$cellBaseClass update"
          editCell.appendChild(ul(colValueToItems(v)).render)
        case Some(v)                       =>
          editCell.className = cellBaseClass
          editCell.appendChild(ul(colValueToItems(v)).render)
      }
    }
  }

  def transformValues[ColType, Ret](arrays: List[Array[Option[ColType]]],
                                    toColType: Ret => ColType,
                                    updateCell: (Int, Option[ColType], Option[ColType]) => Unit
                                   ): Unit = {
    val origArray     = arrays(origIndex)
    val editArray     = arrays(editIndex)
    val processType   = attrType match {
      case "Int"                    => "Int"
      case "Long" | "datom" | "ref" => "BigInt"
      case "Float" | "Double"       => "BigDecimal"
      case _                        => attrType
    }
    val processed     = Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")
    val transferType  = {
      if (processed.contains(attrType))
        "String"
      else
        attrType
    }
    val compiler      = ScalafiddleApi[Ret](
      card,
      lhsTypes.mkString(", "),
      lhsParamsTypes.mkString(", "),
      vars.mkString("\n  "),
      varAssignments.mkString("\n      "),
      lhsParams.mkString(", "),
      attrType,
      processType,
      transferType,
      rhs
    )
    var oldVopt       = Option.empty[ColType]
    var newVopt       = Option.empty[ColType]
    var i             = 0
    var j             = 0
    var tableRowIndex = 0

    val resolve = {
      if (card == 1) {
        (i: Int, applyFn: Int => Ret) => {
          j = indexBridge(i)
          oldVopt = origArray(j)
          newVopt = applyFn(j) match {
            case "__None__" => None
            case s          => Some(toColType(s))
          }
          if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
            updateCell(tableRowIndex, oldVopt, newVopt)
            tableRowIndex += 1
          }
          editArray(j) = newVopt
        }
      } else {
        (i: Int, applyFn: Int => Ret) => {
          j = indexBridge(i)
          oldVopt = origArray(j)
          newVopt = applyFn(j) match {
            case Nil => None
            case vs  => Some(toColType(vs))
          }
          if (i >= tableRowIndexOffset && i < tableRowIndexMax) {
            updateCell(tableRowIndex, oldVopt, newVopt)
            tableRowIndex += 1
          }
          editArray(j) = newVopt
        }
      }
    }

    colIndexes.length match {
      case 2 =>
        val ListBuffer(v1, v2) = attrLambdas
        compiler.lambda2.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 3 =>
        val ListBuffer(v1, v2, v3) = attrLambdas
        compiler.lambda3.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 4 =>
        val ListBuffer(v1, v2, v3, v4) = attrLambdas
        compiler.lambda4.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 5 =>
        val ListBuffer(v1, v2, v3, v4, v5) = attrLambdas
        compiler.lambda5.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 6 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6) = attrLambdas
        compiler.lambda6.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 7 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7) = attrLambdas
        compiler.lambda7.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 8 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8) = attrLambdas
        compiler.lambda8.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 9 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9) = attrLambdas
        compiler.lambda9.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 10 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = attrLambdas
        compiler.lambda10.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 11 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = attrLambdas
        compiler.lambda11.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 12 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = attrLambdas
        compiler.lambda12.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 13 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = attrLambdas
        compiler.lambda13.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 14 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = attrLambdas
        compiler.lambda14.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 15 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = attrLambdas
        compiler.lambda15.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 16 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = attrLambdas
        compiler.lambda16.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 17 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) = attrLambdas
        compiler.lambda17.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 18 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) = attrLambdas
        compiler.lambda18.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 19 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) = attrLambdas
        compiler.lambda19.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 20 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) = attrLambdas
        compiler.lambda20.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 21 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) = attrLambdas
        compiler.lambda21.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j), v21(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }

      case 22 =>
        val ListBuffer(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) = attrLambdas
        compiler.lambda22.foreach { fn =>
          val applyFn = (j: Int) => fn(v1(j), v2(j), v3(j), v4(j), v5(j), v6(j), v7(j), v8(j), v9(j), v10(j), v11(j), v12(j), v13(j), v14(j), v15(j), v16(j), v17(j), v18(j), v19(j), v20(j), v21(j), v22(j))
          while (i < lastRow) {
            resolve(i, applyFn)
            i += 1
          }
        }
    }

    // Turn spinner off
    processing() = ""
  }

  def string(): Unit = {
    val cellBaseClass  = if (attrType == "BigInt" || attrType == "BigDecimal")
      "num" else "str"
    val toColType      = attrType match {
      case "Date" => (s: String) => dateZone2str(new Date(s.toLong))
      case _      => (s: String) => s
    }
    val colValueToNode = attrType match {
      case "String" => (s: String) => _str2frags(s).render
      case _        => (s: String) => s.render
    }
    transformValues(
      qr.str,
      toColType,
      updateCellCardOne(cellBaseClass, colValueToNode),
    )
  }

  def double(): Unit = {
    transformValues[Double, String](
      qr.num,
      (s: String) => s.toDouble,
      updateCellCardOne("num", (v: Double) => v.render),
    )
  }

  def listString(): Unit = {
    val colValueToItems = attrType match {
      case "String" => (vs: List[String]) => vs.sorted.map(v => li(_str2frags(v)))
      case _        => (vs: List[String]) => vs.sorted.map(li(_))
    }
    def transform[Ret](cellBaseClass: String,
                       toColType: Ret => List[String]): Unit = transformValues(
      qr.listStr,
      toColType,
      updateCellCardMany(cellBaseClass, colValueToItems)
    )
    attrType match {
      case "String"     => transform("items", (vs: js.Array[String]) => vs.toList.distinct)
      case "Boolean"    => transform("str", (vs: js.Array[Boolean]) => vs.toList.distinct.map(_.toString))
      case "Date"       => transform("str", (vs: js.Array[Date]) => vs.toList.distinct.map(v => {
        println("--------------")
        println("transform date 2: " + v)
        println("transform date 2: " + dateZone2str(v))
        dateZone2str(v)
      }
      ))
      case "UUID"       => transform("str", (vs: js.Array[UUID]) => vs.toList.distinct.map(_.toString))
      case "URI"        => transform("str", (vs: js.Array[URI]) => vs.toList.distinct.map(_.toString))
      case "BigInt"     => transform("num", (vs: js.Array[String]) => vs.toList.distinct)
      case "BigDecimal" => transform("num", (vs: js.Array[String]) => vs.toList.distinct)
    }
  }

  def listDouble(): Unit = {
    val toColType = attrType match {
      case "Int" => (vs: js.Array[Int]) => vs.toList.distinct.map(_.toDouble)
      case _     => (vs: js.Array[String]) => vs.toList.distinct.map(_.toDouble)
    }
    transformValues(
      qr.listNum,
      toColType,
      updateCellCardMany("num", (vs: List[Double]) => vs.sorted.map(li(_)))
    )
  }

  def mapString(): Unit = {
    val colValueToItems = attrType match {
      case "String" => (vs: Map[String, String]) =>
        vs.toList.sortBy(_._1).map {
          case (k, v) => li(_str2frags(k + " -> " + v))
        }
      case _        => (vs: Map[String, String]) =>
        vs.toList.sortBy(_._1).map {
          case (k, v) => li(k + " -> " + v.toString)
        }
    }
    def transform[Ret](cellBaseClass: String,
                       toColType: Ret => Map[String, String]): Unit = transformValues(
      qr.mapStr,
      toColType,
      updateCellCardMany(cellBaseClass, colValueToItems)
    )
    attrType match {
      case "String"     =>
        transform("items", (vs: js.Dictionary[String]) =>
          vs.toMap)
      case "Boolean"    =>
        transform("str", (vs: js.Dictionary[Boolean]) =>
          vs.toMap.map { case (k, v) => k -> v.toString })
      case "Date"       =>
        transform("str", (vs: js.Dictionary[Date]) =>
          vs.toMap.map { case (k, v) => {

            println("--------------")
            println("transform date 3: " + v)
            println("transform date 3: " + dateLocal2str(v))
            println("transform date 3: " + dateZone2str(v))

            k -> dateLocal2str(v)
          }
          })
      case "UUID"       =>
        transform("str", (vs: js.Dictionary[UUID]) =>
          vs.toMap.map { case (k, v) => k -> v.toString })
      case "URI"        =>
        transform("str", (vs: js.Dictionary[URI]) =>
          vs.toMap.map { case (k, v) => k -> v.toString })
      case "BigInt"     =>
        transform("num", (vs: js.Dictionary[String]) =>
          vs.toMap)
      case "BigDecimal" =>
        transform("num", (vs: js.Dictionary[String]) =>
          vs.toMap)
    }
  }

  def mapDouble(): Unit = {
    val toColType       = attrType match {
      case "Int" =>
        (vs: js.Dictionary[Int]) =>
          vs.toMap.map { case (k, v) => k -> v.toDouble }
      case _     =>
        (vs: js.Dictionary[String]) =>
          vs.toMap.map { case (k, v) => k -> v.toDouble }
    }
    val colValueToItems =
      (vs: Map[String, Double]) =>
        vs.toList.sortBy(_._1).map { case (k, v) => li(k + " -> " + v) }

    transformValues(
      qr.mapNum,
      toColType,
      updateCellCardMany("str", colValueToItems)
    )
  }
}
