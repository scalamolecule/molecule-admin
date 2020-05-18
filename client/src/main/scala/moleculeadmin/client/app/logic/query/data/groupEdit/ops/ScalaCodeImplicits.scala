package moleculeadmin.client.app.logic.query.data.groupEdit.ops
import moleculeadmin.shared.ast.query.Col
import moleculeadmin.shared.ops.query.ColOps


abstract class ScalaCodeImplicits(cols: Seq[Col], col: Col, scalaExpr: String)
  extends TypeMappings with ColOps {

  val Col(_, _, nsAlias, nsFull, attr, attrType, _, card, _, _, _, _, _, _, _) = col

  val q = "\"\"\""

  val opt = col.attr.last == '$'

  val processType: String = attrType match {
    case "Int"                    => "Int"
    case "Long" | "datom" | "ref" => "BigInt"
    case "Float" | "Double"       => "BigDecimal"
    case "Date"                   => "LocalDateTime"
    case _                        => attrType // Boolean, UUID, URI
  }

  val attrResolver = ResolveAttrs(cols)

  val transferType: String = {
    val convertedTypes = Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")
    if (convertedTypes.contains(attrType))
      "String"
    else if (attrType == "Date")
      "LocalDateTime"
    else
      attrType // Int, Boolean, UUID, URI
  }

  val (shared, rhs) = {
    val (shared0, rhs0) = scalaExpr.split("\n---\n").toList match {
      case List(a, b) => ("\n    " + a.replace("\n", "\n    "), b)
      case List(b)    => ("", b)
      case _          => ("", "")
    }
    val rhs1 = card match {
      case 1 =>
        if (rhs0.isEmpty || rhs0 == "None")
          s"Option.empty[$processType]"
        else
          rhs0.replace("\n", "\n      ")

      case _ =>
        if (rhs0.isEmpty || rhs0 == "Nil")
          if (card == 2)
            s"List.empty[$processType]"
          else
            s"Map.empty[String, $processType]"
        else if (opt && card > 1)
          rhs0.replace("\n", "\n      ").replace(attr, attr.init)
        else
          rhs0.replace("\n", "\n      ")
    }
    (shared0, rhs1)
  }

  val tpe: String = attrType match {
    case "datom" | "ref" => "Long"
    case t               => t
  }

  val noLong    = q + s"Long not allowed in $tpe expression `$rhs`" + q
  val noFloat   = q + s"Float not allowed in $tpe expression `$rhs`" + q
  val noDouble  = q + s"Double not allowed in $tpe expression `$rhs`" + q
  val noBigDec  = q + s"BigDecimal not allowed in $tpe expression `$rhs`" + q
  val useDouble = q + s"Please use Double instead of Float in expression `$rhs` to get correct floating point precision." + q

  // preventing Float
  val int2bigDec    = "implicit def int2bigDec(v: Int): BigDecimal = BigDecimal(v)"
  val long2bigDec   = "implicit def long2bigDec(v: Long): BigDecimal = BigDecimal(v)"
  val bigInt2bigDec = "implicit def bigInt2bigDec(v: BigInt): BigDecimal = BigDecimal(v)"
  val double2bigDec = "implicit def double2bigDec(v: Double): BigDecimal = BigDecimal(v)"

  val int2bigInt    = "implicit def int2bigInt(v: Int): BigInt = BigInt(v)"
  val long2bigInt   = "implicit def long2bigInt(v: Long): BigInt = BigInt(v)"
  val double2bigInt = "implicit def double2bigInt(v: Double): BigInt = BigInt(v.toString)"

  val float2bigDecErr =
    s"""implicit def float2bigDec(v: Float): BigDecimal =
       |    throw new IllegalArgumentException(
       |      $useDouble
       |    )""".stripMargin

  val str2uuid = "implicit def str2uuid(s: String): UUID = UUID.fromString(s)"
  val str2uri  = "implicit def str2uri(s: String): URI = new URI(s)"

  val regex =
    s"""implicit class Regex(sc: StringContext) {
       |    def r = new scala.util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
       |  }""".stripMargin

  val dateImplicits =
    s"""implicit def str2ldt(s: String): LocalDateTime = {
       |    val msec = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])\\.(\\d{1,3})$q.r
       |    val sec  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])$q.r
       |    val min  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])[T ]+(2[0-3]|1[0-9]|0?[0-9]):([1-5][0-9]|0?[0-9])$q.r
       |    val ymd  = $q(\\d{1,4})-(1[0-2]|0?[0-9])-(3[01]|[12][0-9]|0?[0-9])$q.r
       |    val ym   = $q(\\d{1,4})-(1[0-2]|0?[0-9])$q.r
       |    val year = $q(\\d{1,4})$q.r
       |    s.trim match {
       |      case msec(y, m, d, hh, mm, ss, ms) => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, ms.padTo(3, '0').toInt * 1000000)
       |      case sec(y, m, d, hh, mm, ss)      => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, ss.toInt, 0)
       |      case min(y, m, d, hh, mm)          => LocalDateTime.of(y.toInt, m.toInt, d.toInt, hh.toInt, mm.toInt, 0, 0)
       |      case ymd(y, m, d)                  => LocalDateTime.of(y.toInt, m.toInt, d.toInt, 0, 0, 0, 0)
       |      case ym(y, m)                      => LocalDateTime.of(y.toInt, m.toInt, 1, 0, 0, 0, 0)
       |      case year(y)                       => LocalDateTime.of(y.toInt, 1, 1, 0, 0, 0, 0)
       |      case ""                            =>
       |        throw new IllegalArgumentException(s"Please provide a non-empty date string")
       |      case other                         =>
       |        throw new IllegalArgumentException(s"Non-valid date string: `$$other`. " +
       |          "Expected form for a full date is `2001-03-05 07:09:11.123`")
       |    }
       |  }
       |  // Convenience LocalDateTime constructor
       |  def d(
       |    y: Int,
       |    m: Int = 1,
       |    d: Int = 1,
       |    hh: Int = 0,
       |    mm: Int = 0,
       |    ss: Int = 0,
       |    ms: Int = 0
       |  ) = {
       |    if (ms > 999 || ms < 0)
       |      throw new IllegalArgumentException(s"Milliseconds should be in range 0-999. Found: $$ms")
       |    else
       |      LocalDateTime.of(y, m, d, hh, mm, ss, ms * 1000 * 1000)
       |  }""".stripMargin


  // Card 2 ====================================================

  val iterAny2iterBigInt =
    s"""implicit def iterAny2iterBigInt(l: Iterable[Any]): Iterable[BigInt] = l.map {
       |    case n: Int    => BigInt(n)
       |    case n: Long   => BigInt(n)
       |    case n: BigInt => n
       |    case v         =>
       |      throw new IllegalArgumentException(
       |        "Invalid BigInt value " + v + " of type " + v.getClass +
       |        ". Value can be of type Int, Long or BigInt."
       |      )
       |  }""".stripMargin

  val iterAnyLong2iterBigInt =
    s"""implicit def iterAnyLong2iterBigInt(l: Iterable[Any]): Iterable[BigInt] = l.map {
       |    case n: Int                         => BigInt(n)
       |    case n: Long                        => BigInt(n)
       |    case n: BigInt if n < Long.MinValue => throw new IllegalArgumentException("Number is too small to be a Long: " + n)
       |    case n: BigInt if n > Long.MaxValue => throw new IllegalArgumentException("Number is too big to be a Long: " + n)
       |    case n: BigInt                      => n
       |    case v                              =>
       |      throw new IllegalArgumentException(
       |        "Invalid Long value " + v + " of type " + v.getClass +
       |        ". Value can be of type Int, Long or BigInt."
       |      )
       |  }""".stripMargin

  val iterAny2iterBigDec =
    s"""implicit def iterAny2iterBigDec(l: Iterable[Any]): Iterable[BigDecimal] = l.map {
       |    case n: Int        => BigDecimal(n)
       |    case n: Long       => BigDecimal(n)
       |    case n: BigInt     => BigDecimal(n)
       |    case n: Double     => BigDecimal(n)
       |    case n: BigDecimal => n
       |    case v             =>
       |      throw new IllegalArgumentException(
       |        "Invalid BigDecimal value " + v + " of type " + v.getClass +
       |        ". Value can be of type Int, Long, BigInt, Double or BigDecimal."
       |      )
       |  }""".stripMargin

  val iterAnyLDT2iterLDT =
    s"""implicit def iterAnyLDT2iterLDT(l: Iterable[Any]): Iterable[LocalDateTime] = l.map {
       |    case s: String        => str2ldt(s)
       |    case d: LocalDateTime => d
       |    case v                =>
       |      throw new IllegalArgumentException(
       |        "Invalid LocalDateValue value " + v + " of type " + v.getClass +
       |        ". Value can be of type String or LocalDateTime."
       |      )
       |  }""".stripMargin

  val iterAny2iterUUID =
    s"""implicit def iterAny2iterUUID(l: Iterable[Any]): Iterable[UUID] = l.map {
       |    case s: String => UUID.fromString(s)
       |    case u: UUID   => u
       |    case v         =>
       |      throw new IllegalArgumentException(
       |        "Invalid UUID value " + v + " of type " + v.getClass +
       |        ". Value can be of type String or UUID."
       |      )
       |  }""".stripMargin

  val iterAny2iterURI =
    s"""implicit def iterAny2iterURI(l: Iterable[Any]): Iterable[URI] = l.map {
       |    case s: String => new URI(s)
       |    case u: URI    => u
       |    case v         =>
       |      throw new IllegalArgumentException(
       |        "Invalid URI value " + v + " of type " + v.getClass +
       |        ". Value can be of type String or URI."
       |      )
       |  }""".stripMargin


  // From processType to transferType

  val iter2arr =
    s"""implicit def iter2arr(vs: Iterable[$processType]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.toString))
       |    array
       |  }""".stripMargin


  val iterStr2arr =
    s"""implicit def iterStr2arr(vs: Iterable[String]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v))
       |    array
       |  }""".stripMargin

  val iterLDT2arr =
    s"""implicit def iterLDT2arr(vs: Iterable[$processType]): js.Array[String] = {
       |    val array = new js.Array[String]()
       |    vs.foreach(v => array.push(v.withNano(v.getNano/1000000*1000000).toString))
       |    array
       |  }""".stripMargin


  // Card 3 ====================================================

  val mapAny2mapBigInt =
    s"""implicit def mapAny2mapBigInt(l: Map[String, Any]): Map[String, BigInt] = l.map {
       |    case (k, n: Int)    => k -> BigInt(n)
       |    case (k, n: Long)   => k -> BigInt(n)
       |    case (k, n: BigInt) => k -> n
       |    case (k, v)         =>
       |      throw new IllegalArgumentException(
       |        "Invalid String/BigInt pair (" + k + " -> " + v + ") of types " + k.getClass + "/" + v.getClass +
       |        ". Key should be a String and value of type Int, Long or BigInt."
       |      )
       |  }""".stripMargin

  val mapAnyLong2mapBigInt =
    s"""implicit def mapAnyLong2mapBigInt(l: Map[String, Any]): Map[String, BigInt] = l.map {
       |    case (k, n: Int)                         => k -> BigInt(n)
       |    case (k, n: Long)                        => k -> BigInt(n)
       |    case (k, n: BigInt) if n < Long.MinValue => throw new IllegalArgumentException("Number value is too small to be a Long: " + n)
       |    case (k, n: BigInt) if n > Long.MaxValue => throw new IllegalArgumentException("Number value is too big to be a Long: " + n)
       |    case (k, n: BigInt)                      => k -> n
       |    case (k, v)                                =>
       |      throw new IllegalArgumentException(
       |        "Invalid String/Long pair (" + k + " -> " + v + ") of types " + k.getClass + "/" + v.getClass +
       |        ". Key should be a String and value of type Int, Long or BigInt."
       |      )
       |  }""".stripMargin

  val mapAny2mapBigDec =
    s"""implicit def mapAny2mapBigDec(l: Map[String, Any]): Map[String, BigDecimal] = l.map {
       |    case (k, n: Int)        => k -> BigDecimal(n)
       |    case (k, n: Long)       => k -> BigDecimal(n)
       |    case (k, n: BigInt)     => k -> BigDecimal(n)
       |    case (k, n: Double)     => k -> BigDecimal(n)
       |    case (k, n: BigDecimal) => k -> n
       |    case (k, v)             =>
       |      throw new IllegalArgumentException(
       |        "Invalid String/BigDecimal pair (" + k + " -> " + v + ") of types " + k.getClass + "/" + v.getClass +
       |        ". Key should be a String and value of type Int, Long, BigInt, Double or BigDecimal."
       |      )
       |  }""".stripMargin

  val mapAnyLDT2mapLDT =
    s"""implicit def mapAnyLDT2mapLDT(l: Map[String, Any]): Map[String, LocalDateTime] = l.map {
       |    case (k, s: String)        => k -> str2ldt(s)
       |    case (k, d: LocalDateTime) => k -> d
       |    case (k, v)                =>
       |      throw new IllegalArgumentException(
       |        "Invalid String/LocalDateValue pair (" + k + " -> " + v + ") of types " + k.getClass + "/" + v.getClass +
       |        ". Key should be a String and value of type String or LocalDateTime."
       |      )
       |  }""".stripMargin

  val mapAny2mapUUID =
    s"""implicit def mapAny2mapUUID(l: Map[String, Any]): Map[String, UUID] = l.map {
       |    case (k, s: String) => k -> UUID.fromString(s)
       |    case (k, u: UUID)   => k -> u
       |    case (k, v)         =>
       |      throw new IllegalArgumentException(
       |        "Invalid String/UUID pair (" + k + " -> " + v + ") of types " + k.getClass + "/" + v.getClass +
       |        ". Key should be a String and value of type String or UUID."
       |      )
       |  }""".stripMargin

  val mapAny2mapURI =
    s"""implicit def mapAny2mapURI(l: Map[String, Any]): Map[String, URI] = l.map {
       |    case (k, s: String) => k -> new URI(s)
       |    case (k, u: URI)    => k -> u
       |    case (k, v)         =>
       |      throw new IllegalArgumentException(
       |        "Invalid String/URI pair (" + k + " -> " + v + ") of types " + k.getClass + "/" + v.getClass +
       |        ". Key should be a String and value of type String or URI."
       |      )
       |  }""".stripMargin


  // From processType to transferType

  val map2dict =
    s"""implicit def map2dict(m: Map[String, $processType]): js.Dictionary[String] =  {
       |    val dict = js.Dictionary.empty[String]
       |    m.foreach { case (k, v) => dict(k) = v.toString }
       |    dict
       |  }""".stripMargin

  val mapStr2dict =
    s"""implicit def map2dict(mapOfString: Map[String, $processType]): js.Dictionary[String] =  {
       |    val dict = js.Dictionary.empty[String]
       |    mapOfString.foreach { case (k, v) => dict(k) = v }
       |    dict
       |  }""".stripMargin

  val mapLDT2dict =
    s"""implicit def mapDate2dict(map: Map[String, LocalDateTime]): js.Dictionary[String] =  {
       |    val dict = js.Dictionary.empty[String]
       |    map.foreach { case (k, v) => dict(k) = v.withNano(v.getNano/1000000*1000000).toString }
       |    dict
       |  }""".stripMargin
}
