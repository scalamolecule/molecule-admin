package moleculeadmin.client.app.logic.query.data.groupEdit.ops

import moleculeadmin.shared.ast.query.Col
import scala.collection.mutable.ListBuffer


case class ScalaCode(cols: Seq[Col], col: Col, scalaExpr: String)
  extends ScalaCodeImplicits(cols, col, scalaExpr) {

  val maxAttrLength = cols.map(_.attr.length).max

  def get: String = card match {
    case 1 => card1
    case 2 => card2
    case 3 => card3
  }

  def pad(attr: String): String = attr + " " * (maxAttrLength - attr.length)

  // Build scala code elements for ScalaFiddle compilation
  val transferTypes0  = new ListBuffer[String]
  val transferParams0 = new ListBuffer[String]
  val conversions0    = new ListBuffer[String]
  val processTypes0   = new ListBuffer[String]
  val processParams0  = new ListBuffer[String]

  cols.collect {
    case col@Col(_, _, _, _, _, tpe, _, card, opt, _, _, attrExpr, _, _, _)
      if attrExpr != "edit" =>

      val attr                          = attrResolver.postfixed(col)
      val (processType, paramConverter) = getTypeMappings(attr, tpe, card)
      val transferType                  = card match {
        case 1 if opt => "js.UndefOr[String]"
        case 1        => "String"
        case 2        => "js.Array[String]"
        case 3        => "js.Dictionary[String]"
      }
      transferTypes0 += transferType
      transferParams0 += s"$attr: $transferType"

      conversions0 += paramConverter
      processTypes0 += processType
      processParams0 += s"$attr: $processType"
  }

  val transferTypes : String = transferTypes0.mkString(", ")
  val transferParams: String = transferParams0.mkString(", ")
  val conversions   : String = conversions0.mkString(",\n          ")
  val processTypes  : String = processTypes0.mkString(", ")
  val processParams : String = processParams0.mkString(", ")
  val n                      = transferTypes0.length

  val imports: String =
    """import scalajs.js
      |import js.annotation.{JSExportTopLevel, JSExport}
      |import js.JSConverters._
      |import java.time._
      |import java.util.{Date, UUID}
      |import java.net.URI
      |""".stripMargin

  val versions: String =
    """// $ScalaVersion 2.12
      |// $ScalaJSVersion 0.6""".stripMargin


  def card1: String = {
    val implicits = attrType match {
      // Exclude using Float
      case "Float" | "Double" | "BigDecimal" => Seq(
        // whitelist
        int2bigDec,
        long2bigDec,
        bigInt2bigDec,
        double2bigDec,
        // blacklist
        float2bigDecErr).mkString("  ", "\n  ", "")

      // indenting here so that we can test empty imports
      case "Date" => "  " + dateImplicits
      case "UUID" => "  " + str2uuid
      case "URI"  => "  " + str2uri
      case _      => "" // no conversions
    }

    val mapToString = if (attrType == "Date")
      "v => v.withNano(v.getNano/1000000*1000000).toString"
    else
      "_.toString"

    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |$implicits
       |  @JSExport
       |  val lambda: js.Tuple$n[$transferTypes] => js.Tuple2[js.UndefOr[String], String] = {
       |    case js.Tuple$n($transferParams) =>
       |      try {
       |        val result: Option[$processType] = process(
       |          $conversions
       |        )
       |        js.Tuple2(result.map($mapToString).orUndefined, "")
       |      } catch {
       |        case e: Throwable => js.Tuple2(Option.empty[String].orUndefined, e.toString)
       |      }
       |  }
       |
       |  val process: ($processTypes) => Option[$processType] = {$shared
       |    ($processParams) => {
       |      $rhs
       |    }
       |  }
       |}
       |$versions""".stripMargin.trim
  }


  def card2: String = {
    val decImplicits = Seq(
      iter2arr,
      int2bigDec,
      long2bigDec,
      bigInt2bigDec,
      double2bigDec,
      float2bigDecErr,
      iterAny2iterBigDec,
    )

    val implicits = (attrType match {
      case "String"                 => Seq(iterStr2arr)
      case "Int"                    => Seq(iter2arr)
      case "Long" | "datom" | "ref" => Seq(iter2arr, iterAnyLong2iterBigInt)
      case "BigInt"                 => Seq(iter2arr, iterAny2iterBigInt)
      case "Float"                  => decImplicits
      case "Double"                 => decImplicits
      case "BigDecimal"             => decImplicits
      case "Boolean"                => Seq(iter2arr)
      case "Date"                   => Seq(iterLDT2arr, dateImplicits, iterAnyLDT2iterLDT)
      case "UUID"                   => Seq(iter2arr, str2uuid, iterAny2iterUUID)
      case "URI"                    => Seq(iter2arr, str2uri, iterAny2iterURI)
    }).mkString("  ", "\n  ", "")

    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |$implicits
       |  @JSExport
       |  val lambda: js.Tuple$n[$transferTypes] => js.Tuple2[js.Array[String], String] = {
       |    case js.Tuple$n($transferParams) =>
       |      try {
       |        val result: Iterable[$processType] = process(
       |          $conversions
       |        )
       |        // implicit conversion from Iterable[$processType] to js.Array[String]
       |        js.Tuple2(result, "")
       |      } catch {
       |        case e: Throwable => js.Tuple2(new js.Array[String](0), e.toString)
       |      }
       |  }
       |
       |  val process: ($processTypes) => Iterable[$processType] = {$shared
       |    ($processParams) => {
       |      $rhs
       |    }
       |  }
       |}
       |$versions""".stripMargin.trim
  }


  def card3: String = {
    val decImplicits = Seq(
      map2dict,
      int2bigDec,
      long2bigDec,
      bigInt2bigDec,
      double2bigDec,
      float2bigDecErr,
      mapAny2mapBigDec,
    )

    val implicits = (attrType match {
      case "String"                 => Seq(mapStr2dict)
      case "Int"                    => Seq(map2dict)
      case "Long" | "datom" | "ref" => Seq(map2dict, mapAnyLong2mapBigInt)
      case "BigInt"                 => Seq(map2dict, mapAny2mapBigInt)
      case "Float"                  => decImplicits
      case "Double"                 => decImplicits
      case "BigDecimal"             => decImplicits
      case "Boolean"                => Seq(map2dict)
      case "Date"                   => Seq(mapLDT2dict, dateImplicits, mapAnyLDT2mapLDT)
      case "UUID"                   => Seq(map2dict, str2uuid, mapAny2mapUUID)
      case "URI"                    => Seq(map2dict, str2uri, mapAny2mapURI)
    }).mkString("  ", "\n  ", "")

    // Empty Option is simply treated as an empty Map.
    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |$implicits
       |  @JSExport
       |  val lambda: js.Tuple$n[$transferTypes] => js.Tuple2[js.Dictionary[String], String] = {
       |    case js.Tuple$n($transferParams) =>
       |      try {
       |        val result: Map[String, $processType] = process(
       |          $conversions
       |        )
       |        // `result` implicitly converted from Map[String, $processType] to js.Dictionary[String]
       |        js.Tuple2(result, "")
       |      } catch {
       |        case e: Throwable => js.Tuple2(js.Dictionary.empty[String], e.toString)
       |      }
       |  }
       |
       |  val process: ($processTypes) => Map[String, $processType] = {$shared
       |    ($processParams) => {
       |      $rhs
       |    }
       |  }
       |}
       |$versions""".stripMargin
  }
}
