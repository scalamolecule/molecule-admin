package moleculeadmin.client.app.domain.query.data.groupedit.ops

import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.shared.ast.query.Col
import scala.collection.mutable.ListBuffer


abstract class ScalaCodeImplicits(col: Col, rhs0: String) extends TypeMappings {
  val Col(_, _, nsAlias, nsFull, attr, attrType, _, card, _, _, _, _, _, _) = col

  val q = "\"\"\""

  val opt = col.attr.last == '$'

  val processType: String = attrType match {
    case "Int"                    => "Int"
    case "Long" | "datom" | "ref" => "BigInt"
    case "Float" | "Double"       => "BigDecimal"
    case "Date"                   => "LocalDateTime"
    case _                        => attrType // Boolean, UUID, URI
  }

  val transferType: String = {
    val processed = Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")
    if (processed.contains(attrType))
      "String"
    else if (attrType == "Date")
      "LocalDateTime"
    else
      attrType // Int, Boolean, UUID, URI
  }

  val rhs = card match {
    case 1 =>
      if (rhs0.isEmpty || rhs0 == "None")
        s"Option.empty[$processType]"
      else
        rhs0.replaceAllLiterally("\n", "\n      ")

    case _ =>
      if (rhs0.isEmpty || rhs0 == "Nil")
        if (card == 2)
          s"List.empty[$processType]"
        else
          s"Map.empty[String, $processType]"
      else if (opt && card > 1)
        rhs0.replaceAllLiterally("\n", "\n      ").replace(attr, attr.init)
      else
        rhs0.replaceAllLiterally("\n", "\n      ")
  }

  val tpe: String = attrType match {
    case "datom" | "ref" => "Long"
    case t               => t
  }

  val noFloat   = q + s"Float not allowed in $tpe expression `$rhs`" + q
  val noDouble  = q + s"Double not allowed in $tpe expression `$rhs`" + q
  val noBigDec  = q + s"BigDecimal not allowed in $tpe expression `$rhs`" + q
  val useDouble = q + s"Please use Double instead of Float in expression `$rhs` to get correct floating point precision." + q

  // floating point -> Int error

  val float2intErr  =
    s"""implicit def float2int(v: Float): Int =
       |    throw new IllegalArgumentException(
       |      $noFloat
       |    )""".stripMargin
  val double2intErr =
    s"""implicit def double2int(v: Double): Int =
       |    throw new IllegalArgumentException(
       |      $noDouble
       |    )""".stripMargin
  val bigDec2intErr =
    s"""implicit def bigDec2int(v: BigDecimal): Int =
       |    throw new IllegalArgumentException(
       |      $noBigDec
       |    )""".stripMargin

  // floating point -> BigInt error

  val float2bigIntErr  =
    s"""implicit def float2bigInt(v: Float): BigInt =
       |    throw new IllegalArgumentException(
       |      $noFloat
       |    )""".stripMargin
  val double2bigIntErr =
    s"""implicit def double2bigInt(v: Double): BigInt =
       |    throw new IllegalArgumentException(
       |      $noDouble
       |    )""".stripMargin
  val bigDec2bigIntErr =
    s"""implicit def bigDec2bigInt(v: BigDecimal): BigInt =
       |    throw new IllegalArgumentException(
       |      $noBigDec
       |    )""".stripMargin

  // Float -> BigDecimal error

  val float2bigDecErr =
    s"""implicit def float2bigDec(v: Float): BigDecimal =
       |    throw new IllegalArgumentException(
       |      $useDouble
       |    )""".stripMargin

  val long2int   = "implicit def long2int(v: Long): Int = v.toInt"
  val bigInt2int = "implicit def bigInt2int(v: BigInt): Int = v.toString.toInt"

  val int2bigInt  = "implicit def int2bigInt(v: Int): BigInt = BigInt(v)"
  val long2bigInt = "implicit def long2bigInt(v: Long): BigInt = BigInt(v)"

  val int2bigDec    = "implicit def int2bigDec(v: Int): BigDecimal = BigDecimal(v)"
  val long2bigDec   = "implicit def long2bigDec(v: Long): BigDecimal = BigDecimal(v)"
  val bigInt2bigDec = "implicit def bigInt2bigDec(v: BigInt): BigDecimal = BigDecimal(v)"
  val double2bigDec = "implicit def double2bigDec(v: Double): BigDecimal = BigDecimal(v)"

  val str2uuid = "implicit def str2uuid(s: String): UUID = UUID.fromString(s)"
  val str2uri  = "implicit def str2uri(s: String): URI = new URI(s)"

  val dateImplicits =
    s"""implicit def str2ldt(s: String): LocalDateTime = {
       |    val msec = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])\\.(\\d{1,3})$q.r
       |    val sec  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])$q.r
       |    val min  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])$q.r
       |    val ymd  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])$q.r
       |    s.trim match {
       |      case msec(y, m, d, hh, mm, ss, ms) => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, ms.padTo(3, '0').toInt * 1000 * 1000)
       |      case sec(y, m, d, hh, mm, ss)      => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, 0)
       |      case min(y, m, d, hh, mm)          => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, 0, 0)
       |      case ymd(y, m, d)                  => LocalDateTime.of(y.toInt, m.toInt, d.toInt, 0, 0, 0, 0)
       |      case ""                            => throw new IllegalArgumentException(s"Please provide a non-empty date string")
       |      case other                         => throw new IllegalArgumentException(s"Non-valid date string: `$$other`")
       |    }
       |  }
       |  // Ensure LocalDateTime objects have access to methods (why aren't they available without cloning?)
       |  def cloneDate(d: LocalDateTime) = LocalDateTime.of(d.getYear, d.getMonth, d.getDayOfMonth, d.getHour, d.getMinute, d.getSecond, d.getNano)
       |""".stripMargin

  val intList2bigIntList = "implicit def intList2bigIntList(vs: List[Int]): List[BigInt] = vs.map(BigInt(_))"
  val bigIntList2intList = "implicit def bigIntList2intList(vs: List[BigInt]): List[Int] = vs.map(_.toString.toInt)"


  // Card 2 ====================================================

  val seq2array =
    s"""implicit def seq2array(vs: Seq[$processType]): js.Array[$processType] = {
       |    val array = new js.Array[$processType]()
       |    vs.foreach(v => array.push(v))
       |    array
       |  }""".stripMargin

  val seq2list =
    s"""implicit def seq2list(vs: Seq[$processType]): List[$processType] = {
       |    vs.toList
       |  }""".stripMargin

  val richArray =
    s"""implicit class richArray(val a: js.Array[BigInt]) extends AnyVal {
       |    def :+(v: BigInt) = { a.push(v); a }
       |  }""".stripMargin

  val seqStr2listBigInt =
    s"""implicit def seqStr2listBigInt(vs: Seq[$processType]): List[$processType] = {
       |    vs.toList
       |  }""".stripMargin

  val seqInt2arrayString =
    s"""implicit def seqInt2arrayString(vs: Seq[Int]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val seqLong2arrayString =
    s"""implicit def seqLong2arrayString(vs: Seq[Long]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val seqBigInt2arrayString =
    s"""implicit def seqBigInt2arrayString(vs: Seq[BigInt]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val arrayBigInt2arrayString =
    s"""implicit def arrayBigInt2arrayString(vs: js.Array[BigInt]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val seqFloat2arrayString =
    s"""implicit def seqFloat2arrayString(vs: Seq[Float]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val seqDouble2arrayString =
    s"""implicit def seqDouble2arrayString(vs: Seq[Double]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val seqBigDec2arrayString =
    s"""implicit def seqBigDecimal2arrayString(vs: Seq[BigDecimal]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val arrayBigDec2arrayString =
    s"""implicit def arrayBigDecimal2arrayString(vs: js.Array[BigDecimal]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin

  val seqStr2arrayString =
    s"""implicit def seqStr2arrayString(vs: Seq[String]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v))
       |    array
       |  }""".stripMargin


  // Card 3 ====================================================

  val dictProcess2dictTransfer =
    s"""implicit def dictProcess2dictTransfer(dictProcess: js.Dictionary[$processType]): js.Dictionary[$transferType] =  {
       |    val dictTransfer = js.Dictionary.empty[$transferType]
       |    dictProcess.foreach { case (k, v) => dictTransfer(k) = v }
       |    dictTransfer
       |  }""".stripMargin

  val mapImplicits =
    s"""implicit class richDict(val dict: js.Dictionary[$processType]) extends AnyVal {
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
       |  }""".stripMargin


  val mapInt2dictString =
    s"""implicit def mapInt2dictString(map: Map[String, Int]): js.Dictionary[String] = {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val mapLong2dictString =
    s"""implicit def mapLong2dictString(map: Map[String, Long]): js.Dictionary[String] = {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val mapBigInt2dict =
    s"""implicit def mapBigInt2dict(map: Map[String, BigInt]): js.Dictionary[String] = {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val wrapBigInt2dict =
    s"""implicit def wrapBigInt2dict(pairs: js.WrappedDictionary[BigInt]): js.Dictionary[String] =  {
       |    val dict = js.Dictionary.empty[String]
       |    pairs.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val dictProcessBigInt2dictTransfer =
    s"""implicit def dictProcessBigInt2dictTransfer(dictProcess: js.Dictionary[BigInt]): js.Dictionary[String] = {
       |    val dictTransfer = js.Dictionary.empty[String]
       |    dictProcess.foreach { case (k, v) => dictTransfer(k) = v.toString }
       |    dictTransfer
       |  }""".stripMargin

  val mapFloat2dict =
    s"""implicit def mapFloat2dict(map: Map[String, Float]): js.Dictionary[String] = {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val mapDouble2dict =
    s"""implicit def mapDouble2dict(map: Map[String, Double]): js.Dictionary[String] = {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val mapBigDec2dict =
    s"""implicit def mapBigDecimal2dict(map: Map[String, BigDecimal]): js.Dictionary[String] = {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val wrapBigDec2dict =
    s"""implicit def wrapBigDec2dict(wrap: js.WrappedDictionary[BigDecimal]): js.Dictionary[String] =  {
       |    val dict = js.Dictionary.empty[String]
       |    wrap.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val dictProcessBigDec2dictTransfer =
    s"""implicit def dictProcessBigDec2dictTransfer(dictProcess: js.Dictionary[BigDecimal]): js.Dictionary[String] = {
       |    val dictTransfer = js.Dictionary.empty[String]
       |    dictProcess.foreach { case (k, v) => dictTransfer(k) = v.toString }
       |    dictTransfer
       |  }""".stripMargin

  val mapDate2dict =
    s"""implicit def mapDate2dict(map: Map[String, Date]): js.Dictionary[js.Date] =  {
       |    val dict = js.Dictionary.empty[js.Date]
       |    map.foreach { case (k, v) => dict(k) = (new js.Date(v.getTime().toDouble)) }
       |    dict
       |  }""".stripMargin

  val wrapDate2dict =
    s"""implicit def wrapDate2dict(wrap: js.WrappedDictionary[Date]): js.Dictionary[js.Date] =  {
       |    val dict = js.Dictionary.empty[js.Date]
       |    wrap.foreach { case (k, v) => dict(k) = (new js.Date(v.getTime().toDouble)) }
       |    dict
       |  }""".stripMargin

  val dictProcessDate2dictTransfer =
    s"""implicit def dictProcessDate2dictTransfer(dictProcess: js.Dictionary[Date]): js.Dictionary[js.Date] =  {
       |    val dictTransfer = js.Dictionary.empty[js.Date]
       |    dictProcess.foreach { case (k, v) => dictTransfer(k) = (new js.Date(v.getTime().toDouble)) }
       |    dictTransfer
       |  }""".stripMargin

  val map2dict =
    s"""implicit def map2dict(map: Map[String, $processType]): js.Dictionary[$transferType] =  {
       |    val dict = js.Dictionary.empty[$transferType]
       |    map.foreach { case (k, v) => dict(k) = v }
       |    dict
       |  }""".stripMargin

  val wrap2dict =
    s"""implicit def wrap2dict(wrap: js.WrappedDictionary[$processType]): js.Dictionary[$transferType] =  {
       |    val dict = js.Dictionary.empty[$transferType]
       |    wrap.foreach { case (k, v) => dict(k) = v }
       |    dict
       |  }""".stripMargin
}
