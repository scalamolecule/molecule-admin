package moleculeadmin.client.app.domain.query.data.groupedit.ops
import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.util.HelpersAdmin
import scala.collection.mutable.ListBuffer


case class ScalaCode(col: Col, rhs0: String)
  extends ScalaCodeImplicits(col, rhs0) {

  val maxAttrLength = columns.now.map(_.attr.length).max

  def get: String = card match {
    case 1 => card1
    case 2 => card2
    case 3 => card3
  }
  def pad(attr: String): String = attr + " " * (maxAttrLength - attr.length)

  // Build scala code elements for scalafiddle compilation
  val transferTypes0  = new ListBuffer[String]
  val transferParams0 = new ListBuffer[String]
  val conversions0    = new ListBuffer[String]
  val processTypes0   = new ListBuffer[String]
  val processParams0  = new ListBuffer[String]

  columns.now.collect {
    case Col(_, _, nsAlias1, nsFull1, attr0, tpe, _, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>

      val attr1 = if (card > 1) clean(attr0) else attr0
      val attr  = if (nsAlias1 == nsAlias && nsFull1 == nsFull)
        attr1 else nsAlias + "_" + attr1

      val (transferType, processType, paramConverter) = getTypeMappings(attr, tpe, card)

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

  val imports: String =
    """import scalajs.js
      |import js.annotation.{JSExportTopLevel, JSExport}
      |import js.JSConverters._
      |import java.time._
      |import java.util.{Date, UUID}
      |import java.net.URI
      |""".stripMargin


  def card1: String = {
    val implicits = attrType match {
      case "Int" => Seq(
        // whitelist
        long2int,
        bigInt2int,
        // blacklist
        float2intErr,
        double2intErr,
        bigDec2intErr).mkString("\n  ")

      case "Long" | "datom" | "ref" | "BigInt" => Seq(
        // whitelist
        int2bigInt,
        long2bigInt,
        // blacklist
        float2bigIntErr,
        double2bigIntErr,
        bigDec2bigIntErr).mkString("\n  ")

      case "Float" | "Double" | "BigDecimal" => Seq(
        // whitelist
        int2bigDec,
        long2bigDec,
        bigInt2bigDec,
        double2bigDec,
        // blacklist
        float2bigDecErr).mkString("\n  ")

      case "Date"               => dateImplicits
      case "UUID"               => str2uuid
      case "URI"                => str2uri
      case "String" | "Boolean" => "" // no conversions needed
    }

    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |  $implicits
       |  @JSExport
       |  val lambda: ($transferTypes) => js.Tuple2[js.UndefOr[String], String] = {
       |    ($transferParams) =>
       |      try {
       |        val result: Option[$processType] = process(
       |          $conversions
       |        )
       |        js.Tuple2(result.map(_.toString).orUndefined, "")
       |      } catch {
       |        case e: Throwable => js.Tuple2(Option.empty[String].orUndefined, e.toString)
       |      }
       |  }
       |
       |  val process: ($processTypes) => Option[$processType] = {
       |    ($processParams) => {
       |      $rhs
       |    }
       |  }
       |}""".stripMargin.trim
  }


  def card2: String = {
    val implicits = attrType match {
      case "Int" => Seq(
        //        seq2array,
        //        seq2list,

        //        bigInt2int,

        float2intErr,
        double2intErr,
        bigDec2intErr,
      ).mkString("\n  ")

      case "Long" | "datom" | "ref" | "BigInt" => Seq(
        //    richArray,
        seq2array,
        seq2list,
        //
        //        intList2bigIntList,
        //        bigIntList2intList,

        float2bigIntErr,
        double2bigIntErr,
        bigDec2bigIntErr,
        //        int2bigInt,
        //
        //
        //        seqStr2listBigInt,
        //        seqInt2arrayString,
        //        seqLong2arrayString,
        //        seqBigInt2arrayString,
        //        arrayBigInt2arrayString,
      ).mkString("\n  ")

      case "Float" | "Double" | "BigDecimal" => Seq(
        //    richArray,
        seq2array,
        seq2list,


        //        bigInt2bigDec,
        //        int2bigInt,
        //        long2bigInt,


        float2bigDecErr,


        //        seqInt2arrayString,
        //        seqLong2arrayString,
        //        seqFloat2arrayString,
        //        seqDouble2arrayString,
        //        seqBigDec2arrayString,
        //        arrayBigDec2arrayString,
      ).mkString("\n  ")

      case "String"  => Seq(seqStr2arrayString, seq2list).mkString("\n  ")
      case "Boolean" => Seq(seq2array, seq2list).mkString("\n  ")
      case "Date"    => Seq(seq2array, seq2list, dateImplicits).mkString("\n  ")
      case "UUID"    => Seq(seq2array, seq2list, str2uuid).mkString("\n  ")
      case "URI"     => Seq(seq2array, seq2list, str2uri).mkString("\n  ")
    }

    s"""$imports
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |  $implicits
       |
       |  @JSExport
       |  val lambda: ($transferTypes) => js.Tuple2[js.Array[$processType], String] = {
       |    ($transferParams) =>
       |      try {
       |        // implicit conversion from List to js.Array
       |        val result: js.Array[$processType] = process(
       |          $conversions
       |        )
       |        js.Tuple2(result, "")
       |      } catch {
       |        case e: Throwable => js.Tuple2(new js.Array[$processType](0), e.toString)
       |      }
       |  }
       |
       |  val process: ($processTypes) => List[$processType] = {
       |    ($processParams) => {
       |      $rhs
       |    }
       |  }
       |}""".stripMargin.trim
  }


  def card3: String = {
    val implicits = attrType match {
      case "Long" | "datom" | "ref" | "BigInt" => Seq(
        mapInt2dictString,
        mapLong2dictString,
        mapBigInt2dict,
        wrapBigInt2dict,
        dictProcessBigInt2dictTransfer,
      ).mkString("\n  ")

      case "Float" | "Double" | "BigDecimal" => Seq(
        mapInt2dictString,
        mapLong2dictString,
        mapFloat2dict,
        mapDouble2dict,
        mapBigDec2dict,
        wrapBigDec2dict,
        dictProcessBigDec2dictTransfer,
      ).mkString("\n  ")

      case "Date" => Seq(
        mapDate2dict,
        wrapDate2dict,
        dictProcessDate2dictTransfer,
      ).mkString("\n  ")

      case _ => Seq(
        map2dict,
        wrap2dict,
        dictProcess2dictTransfer,
      ).mkString("\n  ")
    }

    // Empty Option is simply treated as an empty Map.
    s"""$imports
       |
       |@JSExportTopLevel("ScalaFiddle")
       |object ScalaFiddle {
       |  $mapImplicits
       |  $implicits
       |
       |  @JSExport
       |  val lambda: ($transferTypes) => js.Dictionary[$transferType] = {
       |    ($transferParams) =>
       |      try {
       |        // implicit conversion from List to js.Dictionary
       |        val result: js.Dictionary[$processType] = process(
       |          $conversions
       |        )
       |        js.Tuple2(result, "")
       |      } catch {
       |        case e: Throwable => js.Tuple2(js.Dictionary.empty[$processType], e.toString)
       |      }
       |  }
       |
       |  val process: ($processTypes) => js.Dictionary[$transferType] = {
       |    ($processParams) => {
       |      $rhs
       |    }
       |  }
       |}""".stripMargin
  }
}
