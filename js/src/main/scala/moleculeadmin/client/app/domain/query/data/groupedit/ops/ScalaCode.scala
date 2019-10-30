package moleculeadmin.client.app.domain.query.data.groupedit.ops
import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.shared.ast.query.Col
import scala.collection.mutable.ListBuffer


case class ScalaCode(col: Col, rhs0: String) extends TypeMappings {

  val Col(_, _, nsAlias, nsFull, _, attrType, _, card, _, _, _, _, _, _) = col

  def get: String = card match {
    case 1 => card1
    case 2 => card2
    case 3 => card3
  }

  // Build scala code elements for scalafiddle compilation

  val lhsTypesProcess0       = new ListBuffer[String]
  val lhsParamsTypesProcess0 = new ListBuffer[String]
  val lhsTypes0              = new ListBuffer[String]
  val lhsAssignments0        = new ListBuffer[String]
  val lhsParamsTypes0        = new ListBuffer[String]
  val lhsParams0             = new ListBuffer[String]

  columns.now.collect {
    case Col(_, _, `nsAlias`, `nsFull`, attr, tpe, _, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>
      val (tpeProcess, tpeTransfer, paramConverter) = getTypeMappings(attr, tpe, card)

      val d0 = if (tpe == "Date") "0" else ""
      lhsTypesProcess0 += tpeProcess
      lhsParamsTypesProcess0 += s"$attr$d0: $tpeProcess"
      if (tpe == "Date")
        lhsAssignments0 += s"val $attr = cloneDate($attr$d0)"
      lhsTypes0 += tpeTransfer
      lhsParamsTypes0 += s"$attr: $tpeTransfer"
      lhsParams0 += paramConverter

    case Col(_, _, nsAlias, _, attr, tpe, _, card, _, _, _, attrExpr, _, _)
      if attrExpr != "edit" =>
      val Ns_attr = nsAlias + "_" + attr

      val (tpeProcess, tpeTransfer, paramConverter) = getTypeMappings(Ns_attr, tpe, card)

      val d0 = if (tpe == "Date") "0" else ""
      lhsTypesProcess0 += tpeProcess
      lhsParamsTypesProcess0 += s"$Ns_attr$d0: $tpeProcess"
      if (tpe == "Date")
        lhsAssignments0 += s"val $Ns_attr = cloneDate($Ns_attr$d0)"
      lhsTypes0 += tpeTransfer
      lhsParamsTypes0 += s"$Ns_attr: $tpeTransfer"
      lhsParams0 += paramConverter
  }

  val lhsTypesProcess      : String = lhsTypesProcess0.mkString(", ")
  val lhsParamsTypesProcess: String = lhsParamsTypesProcess0.mkString(", ")
  val lhsAssignments       : String = lhsAssignments0.mkString("\n      ", "\n      ", "")
  val lhsTypes             : String = lhsTypes0.mkString(", ")
  val lhsParamsTypes       : String = lhsParamsTypes0.mkString(", ")
  val lhsParams            : String = lhsParams0.mkString(", ")

  val processed: Seq[String] =
    Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")

  val imports: String =
    """import scalajs.js
      |import js.annotation.{JSExportTopLevel, JSExport}
      |import js.JSConverters._
      |import java.time._
      |import java.util.{Date, UUID}
      |import java.net.URI
      |""".stripMargin

  val processType: String = attrType match {
    case "Int"                    => "Int"
    case "Long" | "datom" | "ref" => "BigInt"
    case "Float" | "Double"       => "BigDecimal"
    case "Date"                   => "LocalDateTime"
    case _                        => attrType // Boolean, UUID, URI
  }

  val transferType: String = {
    if (processed.contains(attrType))
      "String"
    else if (attrType == "Date")
      "LocalDateTime"
    else
      attrType // Boolean, UUID, URI
  }

  def card1: String = {
    val rhs = if (rhs0.isEmpty) s"Option.empty[$processType]" else rhs0

    val tpe: String = attrType match {
      case "datom" | "ref" => "Long"
      case t               => t
    }

    val noFloat: String =
      "\"\"\"" + s"Float's not allowed in $tpe expression `$rhs0`" + "\"\"\""

    val noDouble: String =
      "\"\"\"" + s"Double's not allowed in $tpe expression `$rhs0`" + "\"\"\""

    val noBigDecimal: String =
      "\"\"\"" + s"BigDecimal's not allowed in $tpe expression `$rhs0`" + "\"\"\""

    val useDouble =
      "\"\"\"" + s"Please use Double instead of Float in expression " +
        s"`$rhs0` to get correct floating point precision." + "\"\"\""

    val implicits = attrType match {
      case "Int" =>
        s"""
           |  implicit def bigInt2int(v: BigInt): Int = v.toString.toInt
           |  implicit def float2int(v: Float): Int =
           |    throw new IllegalArgumentException(
           |      $noFloat
           |    )
           |  implicit def double2int(v: Double): Int =
           |    throw new IllegalArgumentException(
           |      $noDouble
           |    )
           |  implicit def bigDec2int(v: BigDecimal): Int =
           |    throw new IllegalArgumentException(
           |      $noBigDecimal
           |    )""".stripMargin

      case "Long" | "datom" | "ref" | "BigInt" =>
        s"""
           |  implicit def int2bigInt(v: Int): BigInt = BigInt(v)
           |  implicit def long2bigInt(v: Long): BigInt = BigInt(v)
           |  implicit def float2bigInt(v: Float): BigInt =
           |    throw new IllegalArgumentException(
           |      $noFloat
           |    )
           |  implicit def double2bigInt(v: Double): BigInt =
           |    throw new IllegalArgumentException(
           |      $noDouble
           |    )
           |  implicit def bigDec2bigInt(v: BigDecimal): BigInt =
           |    throw new IllegalArgumentException(
           |      $noBigDecimal
           |    )""".stripMargin

      case "Float" | "Double" | "BigDecimal" =>
        s"""
           |  implicit def int2bigDec(v: Int): BigDecimal = BigDecimal(v)
           |  implicit def long2bigDec(v: Long): BigDecimal = BigDecimal(v)
           |  implicit def bigInt2bigDec(v: BigInt): BigDecimal = BigDecimal(v)
           |  implicit def float2bigDec(v: Float): BigDecimal =
           |    throw new IllegalArgumentException(
           |      $useDouble
           |    )
           |  implicit def double2bigDec(v: Double): BigDecimal = BigDecimal(v.toString)""".stripMargin

      case "Date" =>
        val q = "\"\"\""
        s"""
           |  implicit def str2ldt(s: String): LocalDateTime = {
           |    val nano = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])\\.(\\d{1,9})$q.r
           |    val sec  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])$q.r
           |    val min  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])$q.r
           |    val ymd  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])$q.r
           |    try {
           |      s match {
           |        case nano(y, m, d, hh, mm, ss, n) => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, n.padTo(9, '0').toInt)
           |        case sec(y, m, d, hh, mm, ss)     => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, 0)
           |        case min(y, m, d, hh, mm)         => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, 0, 0)
           |        case ymd(y, m, d)                 => LocalDateTime.of(y.toInt, m.toInt, d.toInt, 0, 0, 0, 0)
           |        case other                        => throw new IllegalArgumentException("Unexpected date string: " + other)
           |      }
           |    } catch {
           |      case e: Throwable =>
           |        error = e.toString
           |        LocalDateTime.MIN
           |    }
           |  }
           |  // Ensure LocalDateTime objects have access to methods (why aren't they available without cloning?)
           |  def cloneDate(d: LocalDateTime) = LocalDateTime.of(d.getYear, d.getMonth, d.getDayOfMonth, d.getHour, d.getMinute, d.getSecond, d.getNano)
           |  """.stripMargin

      case _ => "" // Boolean, UUID, URI
    }

    // Keeping Option handling within scala boundary
    s"""$imports
       |@JSExportTopLevel("_EditLambda")
       |object _EditLambda {
       |  var error = ""$implicits
       |  val process: ($lhsTypesProcess) => Option[$processType] = {
       |    ($lhsParamsTypesProcess) => {$lhsAssignments
       |      $rhs
       |    }
       |  }
       |  @JSExport
       |  val lambda: ($lhsTypes) => String = {
       |    ($lhsParamsTypes) =>
       |      process($lhsParams).fold("__None__"){
       |        case v if error.nonEmpty => "__ERR__" + error + "__ERR__" + v
       |        case v                   => v.toString
       |      }
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

    s"""$imports
       |import js.JSConverters._
       |
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
    s"""$imports
       |
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
