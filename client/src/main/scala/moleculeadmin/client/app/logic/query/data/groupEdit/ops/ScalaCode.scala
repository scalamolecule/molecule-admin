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

  // Aggregate mutable buffers with attr code
  var n = 0
  cols.collect {
    case col@Col(_, _, _, _, _, tpe, _, card, opt, _, _, _, _, _, _, kind)
      if kind != "edit" =>

      n += 1
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

  val (transferTypes, transferParams, conversions, processTypes, processParams) =
    if (n <= 22) {
      (
        transferTypes0.mkString(s"js.Tuple$n[", ", ", "]"),
        transferParams0.mkString(s"js.Tuple$n(", ", ", ")"),
        conversions0.mkString(",\n        "),
        processTypes0.mkString("(", ", ", ")"),
        processParams0.mkString("(\n      ", ",\n      ", "\n    )")
      )

    } else if (n <= 42) {
      (
        s"""js.Tuple2[
           |      js.Tuple20[${transferTypes0.take(20).mkString(", ")}],
           |      js.Tuple${n - 20}[${transferTypes0.drop(20).mkString(", ")}]
           |    ]""".stripMargin,

        s"""js.Tuple2(
           |      js.Tuple20(${transferParams0.take(20).mkString(", ")}),
           |      js.Tuple${n - 20}(${transferParams0.drop(20).mkString(", ")})
           |    )""".stripMargin,

        s"""${conversions0.take(20).mkString(",\n        ")}
           |      )(
           |        ${conversions0.drop(20).mkString(",\n        ")}""".stripMargin,


        s"""(${processTypes0.take(20).mkString(", ")}) =>
           |    (${processTypes0.drop(20).mkString(", ")})""".stripMargin,

        s"""(
           |      ${processParams0.take(20).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.drop(20).mkString(",\n      ")}
           |    )""".stripMargin
      )

    } else if (n <= 62) {
      (
        s"""js.Tuple3[
           |      js.Tuple20[${transferTypes0.take(20).mkString(", ")}],
           |      js.Tuple20[${transferTypes0.slice(20, 40).mkString(", ")}],
           |      js.Tuple${n - 40}[${transferTypes0.drop(40).mkString(", ")}]
           |    ]""".stripMargin,

        s"""js.Tuple3(
           |      js.Tuple20(${transferParams0.take(20).mkString(", ")}),
           |      js.Tuple20(${transferParams0.slice(20, 40).mkString(", ")}),
           |      js.Tuple${n - 40}(${transferParams0.drop(40).mkString(", ")})
           |    )""".stripMargin,

        s"""${conversions0.take(20).mkString(",\n        ")}
           |      )(
           |        ${conversions0.slice(20, 40).mkString(",\n        ")}
           |      )(
           |        ${conversions0.drop(40).mkString(",\n        ")}""".stripMargin,

        s"""(${processTypes0.take(20).mkString(", ")}) =>
           |    (${processTypes0.slice(20, 40).mkString(", ")}) =>
           |      (${processTypes0.drop(40).mkString(", ")})""".stripMargin,

        s"""(
           |      ${processParams0.take(20).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.slice(20, 40).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.drop(40).mkString(",\n      ")}
           |    )""".stripMargin
      )

    } else if (n <= 82) {
      (
        s"""js.Tuple4[
           |      js.Tuple20[${transferTypes0.take(20).mkString(", ")}],
           |      js.Tuple20[${transferTypes0.slice(20, 40).mkString(", ")}],
           |      js.Tuple20[${transferTypes0.slice(40, 60).mkString(", ")}],
           |      js.Tuple${n - 60}[${transferTypes0.drop(60).mkString(", ")}]
           |    ]""".stripMargin,

        s"""js.Tuple4(
           |      js.Tuple20(${transferParams0.take(20).mkString(", ")}),
           |      js.Tuple20(${transferParams0.slice(20, 40).mkString(", ")}),
           |      js.Tuple20(${transferParams0.slice(40, 60).mkString(", ")}),
           |      js.Tuple${n - 60}(${transferParams0.drop(60).mkString(", ")})
           |    )""".stripMargin,

        s"""${conversions0.take(20).mkString(",\n        ")}
           |      )(
           |        ${conversions0.slice(20, 40).mkString(",\n        ")}
           |      )(
           |        ${conversions0.slice(40, 60).mkString(",\n        ")}
           |      )(
           |        ${conversions0.drop(60).mkString(",\n        ")}""".stripMargin,

        s"""(${processTypes0.take(20).mkString(", ")}) =>
           |    (${processTypes0.slice(20, 40).mkString(", ")}) =>
           |      (${processTypes0.slice(40, 60).mkString(", ")}) =>
           |        (${processTypes0.drop(60).mkString(", ")})""".stripMargin,

        s"""(
           |      ${processParams0.take(20).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.slice(20, 40).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.slice(40, 60).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.drop(60).mkString(",\n      ")}
           |    )""".stripMargin
      )

    } else {
      (
        s"""js.Tuple5[
           |      js.Tuple20[${transferTypes0.take(20).mkString(", ")}],
           |      js.Tuple20[${transferTypes0.slice(20, 40).mkString(", ")}],
           |      js.Tuple20[${transferTypes0.slice(40, 60).mkString(", ")}],
           |      js.Tuple20[${transferTypes0.slice(60, 80).mkString(", ")}],
           |      js.Tuple${n - 80}[${transferTypes0.drop(80).mkString(", ")}]
           |    ]""".stripMargin,

        s"""js.Tuple5(
           |      js.Tuple20(${transferParams0.take(20).mkString(", ")}),
           |      js.Tuple20(${transferParams0.slice(20, 40).mkString(", ")}),
           |      js.Tuple20(${transferParams0.slice(40, 60).mkString(", ")}),
           |      js.Tuple20(${transferParams0.slice(60, 80).mkString(", ")}),
           |      js.Tuple${n - 80}(${transferParams0.drop(80).mkString(", ")})
           |    )""".stripMargin,

        s"""${conversions0.take(20).mkString(",\n        ")}
           |      )(
           |        ${conversions0.slice(20, 40).mkString(",\n        ")}
           |      )(
           |        ${conversions0.slice(40, 60).mkString(",\n        ")}
           |      )(
           |        ${conversions0.slice(60, 80).mkString(",\n        ")}
           |      )(
           |        ${conversions0.drop(80).mkString(",\n        ")}""".stripMargin,

        s"""(${processTypes0.take(20).mkString(", ")}) =>
           |    (${processTypes0.slice(20, 40).mkString(", ")}) =>
           |      (${processTypes0.slice(40, 60).mkString(", ")}) =>
           |        (${processTypes0.slice(60, 80).mkString(", ")}) =>
           |          (${processTypes0.drop(80).mkString(", ")})""".stripMargin,

        s"""(
           |      ${processParams0.take(20).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.slice(20, 40).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.slice(40, 60).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.slice(60, 80).mkString(",\n      ")}
           |    ) => (
           |      ${processParams0.drop(80).mkString(",\n      ")}
           |    )""".stripMargin
      )
    }

  val imports: String =
    """import scalajs.js
      |import js.annotation.{JSExportTopLevel, JSExport}
      |import js.JSConverters._
      |import java.time._
      |import java.util.{Date, UUID}
      |import java.net.URI
      |""".stripMargin

  val implicits = {
    lazy val decImplicits1 = Seq(
      // whitelist
      int2bigDec,
      long2bigDec,
      bigInt2bigDec,
      double2bigDec,
      // blacklist
      float2bigDecErr
    )
    lazy val decImplicits2 = Seq(
      iter2arr,
      int2bigDec,
      long2bigDec,
      bigInt2bigDec,
      double2bigDec,
      float2bigDecErr,
      iterAny2iterBigDec,
    )
    lazy val decImplicits3 = Seq(
      map2dict,
      int2bigDec,
      long2bigDec,
      bigInt2bigDec,
      double2bigDec,
      float2bigDecErr,
      mapAny2mapBigDec,
    )
    val implicitsList = cols.flatMap(col =>
      col.card match {
        case 1 => col.attrType match {
          case "Float" | "Double" | "BigDecimal" => decImplicits1
          case "String"                          => Nil
          case "Date"                            => Seq(dateImplicits)
          case "UUID"                            => Seq(str2uuid)
          case "URI"                             => Seq(str2uri)
          case _                                 => Nil
        }
        case 2 => attrType match {
          case "String"                 => Seq(iterStr2arr)
          case "Int"                    => Seq(iter2arr)
          case "Long" | "datom" | "ref" => Seq(iter2arr, iterAnyLong2iterBigInt)
          case "BigInt"                 => Seq(iter2arr, iterAny2iterBigInt)
          case "Float"                  => decImplicits2
          case "Double"                 => decImplicits2
          case "BigDecimal"             => decImplicits2
          case "Boolean"                => Seq(iter2arr)
          case "Date"                   => Seq(iterLDT2arr, dateImplicits, iterAnyLDT2iterLDT)
          case "UUID"                   => Seq(iter2arr, str2uuid, iterAny2iterUUID)
          case "URI"                    => Seq(iter2arr, str2uri, iterAny2iterURI)
        }
        case 3 => attrType match {
          case "String"                 => Seq(mapStr2dict)
          case "Int"                    => Seq(map2dict)
          case "Long" | "datom" | "ref" => Seq(map2dict, mapAnyLong2mapBigInt)
          case "BigInt"                 => Seq(map2dict, mapAny2mapBigInt)
          case "Float"                  => decImplicits3
          case "Double"                 => decImplicits3
          case "BigDecimal"             => decImplicits3
          case "Boolean"                => Seq(map2dict)
          case "Date"                   => Seq(mapLDT2dict, dateImplicits, mapAnyLDT2mapLDT)
          case "UUID"                   => Seq(map2dict, str2uuid, mapAny2mapUUID)
          case "URI"                    => Seq(map2dict, str2uri, mapAny2mapURI)
        }
      }
    ).distinct
    (regex +: implicitsList).mkString("  ", "\n  ", "")
  }

  val versions: String =
    """// $ScalaVersion 2.12
      |// $ScalaJSVersion 0.6""".stripMargin


  def card1: String = {
    val mapToString = if (attrType == "Date")
      "v => v.withNano(v.getNano/1000000*1000000).toString"
    else
      "_.toString"

    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |$implicits
       |  @JSExport
       |  val lambda: $transferTypes => js.Tuple2[js.UndefOr[String], String] = {
       |    case $transferParams => try {
       |      val result: Option[$processType] = process(
       |        $conversions
       |      )
       |      js.Tuple2(result.map($mapToString).orUndefined, "")
       |    } catch {
       |      case e: Throwable => js.Tuple2(Option.empty[String].orUndefined, e.toString)
       |    }
       |  }
       |
       |  val process: $processTypes => Option[$processType] = {$shared
       |    $processParams => {
       |      $rhs
       |    }
       |  }
       |}
       |$versions""".stripMargin.trim
  }

  def card2: String = {
    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |$implicits
       |  @JSExport
       |  val lambda: $transferTypes => js.Tuple2[js.Array[String], String] = {
       |    case $transferParams => try {
       |      val result: Iterable[$processType] = process(
       |        $conversions
       |      )
       |      // implicit conversion from Iterable[$processType] to js.Array[String]
       |      js.Tuple2(result, "")
       |    } catch {
       |      case e: Throwable => js.Tuple2(new js.Array[String](0), e.toString)
       |    }
       |  }
       |
       |  val process: $processTypes => Iterable[$processType] = {$shared
       |    $processParams => {
       |      $rhs
       |    }
       |  }
       |}
       |$versions""".stripMargin.trim
  }

  def card3: String = {
    // Empty Option is simply treated as an empty Map
    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |$implicits
       |  @JSExport
       |  val lambda: $transferTypes => js.Tuple2[js.Dictionary[String], String] = {
       |    case $transferParams => try {
       |      val result: Map[String, $processType] = process(
       |        $conversions
       |      )
       |      // `result` implicitly converted from Map[String, $processType] to js.Dictionary[String]
       |      js.Tuple2(result, "")
       |    } catch {
       |      case e: Throwable => js.Tuple2(js.Dictionary.empty[String], e.toString)
       |    }
       |  }
       |
       |  val process: $processTypes => Map[String, $processType] = {$shared
       |    $processParams => {
       |      $rhs
       |    }
       |  }
       |}
       |$versions""".stripMargin
  }
}
