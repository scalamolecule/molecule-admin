package moleculeadmin.client.app.domain.query.data.groupedit.ops

case class ScalaCode(card: Int,
                     lhsTypesProcess: String,
                     lhsParamsTypesProcess: String,
                     lhsTypes: String,
                     lhsParamsTypes: String,
                     lhsParams: String,
                     attrType: String,
//                     processType: String,
//                     transferType: String,
                     rhs0: String) {

  def get: String = card match {
    case 1 => card1
    case 2 => card2
    case 3 => card3
  }

  val processed: Seq[String] =
    Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")

  val imports: String =
    """import java.util.{Date, UUID}
      |import java.net.URI""".stripMargin


  val processType  = attrType match {
    case "Int"                    => "Int"
    case "Long" | "datom" | "ref" => "BigInt"
    case "Float" | "Double"       => "BigDecimal"
    case _                        => attrType
  }

  val transferType = {
    if (processed.contains(attrType))
      "String"
    else if (attrType == "Date")
      "js.Date"
    else
      attrType
  }

  def card1: String = {
    val rhs = if (rhs0.isEmpty) s"Option.empty[$transferType]" else rhs0

    // Keeping Option handling within scala boundary
    s"""import scalajs.js.annotation.{JSExportTopLevel, JSExport}
       |$imports
       |@JSExportTopLevel("_EditLambda")
       |object _EditLambda {
       |  val process: ($lhsTypesProcess) => Option[$processType] = {
       |    ($lhsParamsTypesProcess) => {
       |      $rhs
       |    }
       |  }
       |  @JSExport
       |  val lambda: ($lhsTypes) => String = {
       |    ($lhsParamsTypes) =>
       |      // Poor man's Option
       |      process($lhsParams).fold("__None__")(_.toString)
       |  }
       |}""".stripMargin.trim
  }


  def card2: String = {
    val rhs = if (rhs0.isEmpty) s"new js.Array[$transferType]()" else rhs0

    val baseImplicits =
      s"""implicit def seqInt2arrayString(vs: Seq[Int]): js.Array[String] = {
         |    val array = new js.Array[String]()
         |    vs.foreach(v => array.push(v.toString))
         |    array
         |  }
         |  implicit def seqLong2arrayString(vs: Seq[Long]): js.Array[String] = {
         |    val array = new js.Array[String]()
         |    vs.foreach(v => array.push(v.toString))
         |    array
         |  }""".stripMargin

    val implicits = attrType match {
      case "Long" | "datom" | "ref" | "BigInt" =>
        s"""
           |  implicit class richArray(val a: js.Array[BigInt]) extends AnyVal {
           |    def :+(v: BigInt) = { a.push(v); a }
           |  }
           |  $baseImplicits
           |  implicit def seq${processType}2arrayString(vs: Seq[BigInt]): js.Array[String] = {
           |    val array = new js.Array[String]()
           |    vs.foreach(v => array.push(v.toString))
           |    array
           |  }
           |  implicit def array${processType}2arrayString(vs: js.Array[BigInt]): js.Array[String] = {
           |    val array = new js.Array[String]()
           |    vs.foreach(v => array.push(v.toString))
           |    array
           |  }""".stripMargin

      case "Float" | "Double" | "BigDecimal" =>
        s"""
           |  implicit class richArray(val a: js.Array[BigDecimal]) extends AnyVal {
           |    def :+(v: BigDecimal) = { a.push(v); a }
           |  }
           |  $baseImplicits
           |  implicit def seqFloat2arrayString(vs: Seq[Float]): js.Array[String] = {
           |    val array = new js.Array[String]()
           |    vs.foreach(v => array.push(v.toString))
           |    array
           |  }
           |  implicit def seqDouble2arrayString(vs: Seq[Double]): js.Array[String] = {
           |    val array = new js.Array[String]()
           |    vs.foreach(v => array.push(v.toString))
           |    array
           |  }
           |  implicit def seqBigDecimal2arrayString(vs: Seq[BigDecimal]): js.Array[String] = {
           |    val array = new js.Array[String]()
           |    vs.foreach(v => array.push(v.toString))
           |    array
           |  }
           |  implicit def arrayBigDecimal2arrayString(vs: js.Array[BigDecimal]): js.Array[String] = {
           |    val array = new js.Array[String]()
           |    vs.foreach(v => array.push(v.toString))
           |    array
           |  }""".stripMargin

      case "Date" =>
        s"""
           |  implicit class richArray(val a: js.Array[js.Date]) extends AnyVal {
           |    def :+(v: js.Date) = { a.push(v); a }
           |    def :+(v: Date) = { a.push(new js.Date(v.getTime().toDouble)); a }
           |  }
           |  implicit def seqDate2arrayString(vs: Seq[Date]): js.Array[js.Date] = {
           |    val array = new js.Array[js.Date]()
           |    vs.foreach(v => array.push(new js.Date(v.getTime().toDouble)))
           |    array
           |  }""".stripMargin

      case _ => ""
    }

    s"""import scalajs.js
       |import js.annotation.{JSExportTopLevel, JSExport}
       |import js.JSConverters._
       |$imports
       |@JSExportTopLevel("_EditLambda")
       |object _EditLambda {$implicits
       |  val process: ($lhsTypesProcess) => js.Array[$transferType] = {
       |    ($lhsParamsTypesProcess) => {
       |      $rhs
       |    }
       |  }
       |  @JSExport
       |  val lambda: ($lhsTypes) => js.Array[$transferType] = {
       |    ($lhsParamsTypes) =>
       |      process($lhsParams)
       |  }
       |}""".stripMargin
  }


  def card3: String = {
    val rhs = if (rhs0.isEmpty) s"js.Dictionary.empty[$transferType]" else rhs0

    val baseImplicits =
      s"""implicit def mapInt2dictString(map: Map[String, Int]): js.Dictionary[String] = {
         |    val dict = js.Dictionary.empty[String]
         |    map.foreach { case (k, v) => dict(k) = v.toString }
         |    dict
         |  }
         |  implicit def mapLong2dictString(map: Map[String, Long]): js.Dictionary[String] = {
         |    val dict = js.Dictionary.empty[String]
         |    map.foreach { case (k, v) => dict(k) = v.toString }
         |    dict
         |  }""".stripMargin


    val implicits = attrType match {
      case "Long" | "datom" | "ref" | "BigInt" =>
        s"""$baseImplicits
           |  implicit def mapBigInt2dict(map: Map[String, BigInt]): js.Dictionary[String] = {
           |    val dict = js.Dictionary.empty[String]
           |    map.foreach { case (k, v) => dict(k) = v.toString }
           |    dict
           |  }
           |  implicit def wrap2dict(pairs: js.WrappedDictionary[BigInt]): js.Dictionary[String] =  {
           |    val dict = js.Dictionary.empty[String]
           |    pairs.foreach { case (k, v) => dict(k) = v.toString }
           |    dict
           |  }
           |  implicit def dictProcess2dictTransfer(dictProcess: js.Dictionary[BigInt]): js.Dictionary[String] = {
           |    val dictTransfer = js.Dictionary.empty[String]
           |    dictProcess.foreach { case (k, v) => dictTransfer(k) = v.toString }
           |    dictTransfer
           |  }""".stripMargin

      case "Float" | "Double" | "BigDecimal" =>
        s"""$baseImplicits
           |  implicit def mapFloat2dict(map: Map[String, Float]): js.Dictionary[String] = {
           |    val dict = js.Dictionary.empty[String]
           |    map.foreach { case (k, v) => dict(k) = v.toString }
           |    dict
           |  }
           |  implicit def mapDouble2dict(map: Map[String, Double]): js.Dictionary[String] = {
           |    val dict = js.Dictionary.empty[String]
           |    map.foreach { case (k, v) => dict(k) = v.toString }
           |    dict
           |  }
           |  implicit def mapBigDecimal2dict(map: Map[String, BigDecimal]): js.Dictionary[String] = {
           |    val dict = js.Dictionary.empty[String]
           |    map.foreach { case (k, v) => dict(k) = v.toString }
           |    dict
           |  }
           |  implicit def wrap2dict(wrap: js.WrappedDictionary[BigDecimal]): js.Dictionary[String] =  {
           |    val dict = js.Dictionary.empty[String]
           |    wrap.foreach { case (k, v) => dict(k) = v.toString }
           |    dict
           |  }
           |  implicit def dictProcess2dictTransfer(dictProcess: js.Dictionary[BigDecimal]): js.Dictionary[String] = {
           |    val dictTransfer = js.Dictionary.empty[String]
           |    dictProcess.foreach { case (k, v) => dictTransfer(k) = v.toString }
           |    dictTransfer
           |  }""".stripMargin

      case "Date" =>
        s"""implicit def map2dict(map: Map[String, Date]): js.Dictionary[js.Date] =  {
           |    val dict = js.Dictionary.empty[js.Date]
           |    map.foreach { case (k, v) => dict(k) = (new js.Date(v.getTime().toDouble)) }
           |    dict
           |  }
           |  implicit def wrap2dict(wrap: js.WrappedDictionary[Date]): js.Dictionary[js.Date] =  {
           |    val dict = js.Dictionary.empty[js.Date]
           |    wrap.foreach { case (k, v) => dict(k) = (new js.Date(v.getTime().toDouble)) }
           |    dict
           |  }
           |  implicit def dictProcess2dictTransfer(dictProcess: js.Dictionary[Date]): js.Dictionary[js.Date] =  {
           |    val dictTransfer = js.Dictionary.empty[js.Date]
           |    dictProcess.foreach { case (k, v) => dictTransfer(k) = (new js.Date(v.getTime().toDouble)) }
           |    dictTransfer
           |  }""".stripMargin

      case _ =>
        s"""implicit def map2dict(map: Map[String, $processType]): js.Dictionary[$transferType] =  {
           |    val dict = js.Dictionary.empty[$transferType]
           |    map.foreach { case (k, v) => dict(k) = v }
           |    dict
           |  }
           |  implicit def wrap2dict(wrap: js.WrappedDictionary[$processType]): js.Dictionary[$transferType] =  {
           |    val dict = js.Dictionary.empty[$transferType]
           |    wrap.foreach { case (k, v) => dict(k) = v }
           |    dict
           |  }
           |  implicit def dictProcess2dictTransfer(dictProcess: js.Dictionary[$processType]): js.Dictionary[$transferType] =  {
           |    val dictTransfer = js.Dictionary.empty[$transferType]
           |    dictProcess.foreach { case (k, v) => dictTransfer(k) = v }
           |    dictTransfer
           |  }""".stripMargin
    }

    // Empty Option is simply treated as an empty Map.
    s"""import scalajs.js
       |import js.annotation.{JSExportTopLevel, JSExport}
       |import js.JSConverters._
       |$imports
       |@JSExportTopLevel("_EditLambda")
       |object _EditLambda {
       |  $implicits
       |  implicit class richDict(val dict: js.Dictionary[$processType]) extends AnyVal {
       |    def :+(pair: (String, $processType)) = { dict(pair._1) = pair._2; dict }
       |  }
       |  implicit def otherMap2dict(otherMap: Map[_, _]): js.Dictionary[$transferType] =  {
       |    if (otherMap.isEmpty) {
       |      js.Dictionary.empty[$transferType]
       |    } else {
       |      val (k, v) = otherMap.head
       |      val err = if (!k.isInstanceOf[String] || !v.isInstanceOf[$processType])
       |        "Map should be of type Map[Strings, $processType]. Found: " + otherMap
       |      else
       |        "Unexpected Map: " + otherMap
       |      org.scalajs.dom.window.alert(err)
       |      throw new RuntimeException(err)
       |    }
       |  }
       |  val process: ($lhsTypesProcess) => js.Dictionary[$transferType] = {
       |    ($lhsParamsTypesProcess) => {
       |      $rhs
       |    }
       |  }
       |  @JSExport
       |  val lambda: ($lhsTypes) => js.Dictionary[$transferType] = {
       |    ($lhsParamsTypes) =>
       |      process($lhsParams)
       |  }
       |}""".stripMargin
  }
}
