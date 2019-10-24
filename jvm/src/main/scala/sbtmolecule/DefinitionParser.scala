package sbtmolecule

import Ast._

case class DefinitionParser(defFileName: String, lines0: List[String], allIndexed: Boolean = true) {

  val lines: List[String] = lines0.map(_.trim)

  def parse: Definition = {

    // Checks .......................................................................

    // Check package statement
    lines.collectFirst {
      case r"package (.*)$p\..*" => p
    }.getOrElse {
      throw new SchemaDefinitionException("Found no package statement in definition file")
    }

    // Check input/output arities
    lines collect {
      case r"@InOut\((\d+)$in, (\d+)$out\)" => (in.toString.toInt, out.toString.toInt) match {
        case (i: Int, _) if i < 0 || i > 3  => throw new SchemaDefinitionException(s"Input arity in '$defFileName' was $in. It should be in the range 0-3")
        case (_, o: Int) if o < 1 || o > 22 => throw new SchemaDefinitionException(s"Output arity of '$defFileName' was $out. It should be in the range 1-22")
        case (i: Int, o: Int)               => (i, o)
      }
    } match {
      case Nil           => throw new SchemaDefinitionException(
        """Please annotate the first namespace definition with '@InOut(inArity, outArity)' where:
          |inArity is a number between 1-3 for how many inputs molecules of this schema can await
          |outArity is a number between 1-22 for how many output attributes molecules of this schema can have""".stripMargin)
      case h :: t :: Nil => throw new SchemaDefinitionException(
        """
          |Only the first namespace should be annotated with @InOut since all namespaces in a schema will need
          |to share the same arities to be able to carry over type information uniformly across namespaces.""".stripMargin)
      case annotations   => annotations.head
    }

    // Check domain name
    lines.collectFirst {
      case r"class (.*)${dmn}Definition"        => throw new SchemaDefinitionException(s"Can't use class as definition container in $defFileName. Please use an object:\nobject ${dmn}Definiton { ...")
      case r"class (.*)${dmn}Definition \{"     => throw new SchemaDefinitionException(s"Can't use class as definition container in $defFileName. Please use an object:\nobject ${dmn}Definiton { ...")
      case r"class (.*)${dmn}Definition \{ *\}" => throw new SchemaDefinitionException(s"Can't use class as definition container in $defFileName. Please use an object:\nobject ${dmn}Definiton { ...")
      case r"trait (.*)${dmn}Definition"        => throw new SchemaDefinitionException(s"Can't use trait as definition container in $defFileName. Please use an object:\nobject ${dmn}Definiton { ...")
      case r"trait (.*)${dmn}Definition \{"     => throw new SchemaDefinitionException(s"Can't use trait as definition container in $defFileName. Please use an object:\nobject ${dmn}Definiton { ...")
      case r"trait (.*)${dmn}Definition \{ *\}" => throw new SchemaDefinitionException(s"Can't use trait as definition container in $defFileName. Please use an object:\nobject ${dmn}Definiton { ...")
    }

    lines.collect {
      case r"object (.*)${name}Definition"      => name
      case r"object (.*)${name}Definition \{"   => name
      case r"object (.*)${name}Definition \{\}" => name
    } match {
      case Nil                      => throw new SchemaDefinitionException("Couldn't find definition object <domain>Definition in " + defFileName)
      case l: List[_] if l.size > 1 => throw new SchemaDefinitionException(s"Only one definition object per definition file allowed. Found ${l.size}:" + l.mkString("\n - ", "Definition\n - ", "Definition"))
      case domainNameList           => firstLow(domainNameList.head)
    }


    // Parse ..........................................

    def parseOptions(str0: String, acc: Seq[Optional] = Nil, attr: String, curFullNs: String = ""): Seq[Optional] = {
      val indexed = Optional( """":db/index"             , true.asInstanceOf[Object]""", "Indexed")
      val options = str0 match {
        case r"\.doc\((.*)$msg\)(.*)$str" => parseOptions(str, acc :+ Optional(s"""":db/doc"               , $msg""", ""), attr, curFullNs)
        case r"\.fulltext(.*)$str"        => parseOptions(str, acc :+ Optional("""":db/fulltext"          , true.asInstanceOf[Object]""", "Fulltext[Ns, In]"), attr, curFullNs)
        case r"\.uniqueValue(.*)$str"     => parseOptions(str, acc :+ Optional("""":db/unique"            , ":db.unique/value"""", "UniqueValue"), attr, curFullNs)
        case r"\.uniqueIdentity(.*)$str"  => parseOptions(str, acc :+ Optional("""":db/unique"            , ":db.unique/identity"""", "UniqueIdentity"), attr, curFullNs)
        case r"\.isComponent(.*)$str"     => parseOptions(str, acc :+ Optional("""":db/isComponent"       , true.asInstanceOf[Object]""", "IsComponent"), attr, curFullNs)
        case r"\.noHistory(.*)$str"       => parseOptions(str, acc :+ Optional("""":db/noHistory"         , true.asInstanceOf[Object]""", "NoHistory"), attr, curFullNs)
        case r"\.indexed(.*)$str"         => parseOptions(str, acc :+ indexed, attr, curFullNs)
        case ""                           => acc
        case unexpected                   => throw new SchemaDefinitionException(s"Unexpected options code for attribute `$attr` in namespace `$curFullNs` in $defFileName:\n" + unexpected)
      }
      if (allIndexed) (options :+ indexed).distinct else options
    }
    val isComponent = Optional("""":db/isComponent"       , true.asInstanceOf[Object]""", "IsComponent")

    val reservedAttrNames = List("a", "e", "v", "t", "tx", "txInstant", "op", "save", "insert", "update", "retract ", "self", "apply", "assert", "replace", "not", "contains", "k")

    def parseAttr(backTics: Boolean, attrClean: String, str: String, curPart: String, curFullNs: String, attrGroup0: Option[String]): DefAttr = {
      if (reservedAttrNames.contains(attrClean))
        throw new SchemaDefinitionException(s"Attribute name `$attrClean` in $defFileName not allowed " +
          s"since it collides with one of the following reserved attribute names:\n" + reservedAttrNames.mkString("\n")
        )
      val attr         = if (backTics) s"`$attrClean`" else attrClean
      val curNs        = if (curFullNs.contains('_')) curFullNs.split("_").last else curFullNs
      val curPartDotNs = if (curFullNs.contains('_')) curFullNs.replace("_", ".") else curFullNs

      str match {
        case r"oneString(.*)$str"     => Val(attr, attrClean, "OneString", "String", "", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneInt(.*)$str"        => Val(attr, attrClean, "OneInt", "Int", "", "long", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneLong(.*)$str"       => Val(attr, attrClean, "OneLong", "Long", "", "long", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneFloat(.*)$str"      => Val(attr, attrClean, "OneFloat", "Float", "", "double", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneDouble(.*)$str"     => Val(attr, attrClean, "OneDouble", "Double", "", "double", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneBigInt(.*)$str"     => Val(attr, attrClean, "OneBigInt", "BigInt", "", "bigint", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneBigDecimal(.*)$str" => Val(attr, attrClean, "OneBigDecimal", "BigDecimal", "", "bigdec", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneByte(.*)$str"       => Val(attr, attrClean, "OneByte", "Byte", "", "bytes", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneBoolean(.*)$str"    => Val(attr, attrClean, "OneBoolean", "Boolean", "", "boolean", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneDate(.*)$str"       => Val(attr, attrClean, "OneDate", "Date", "", "instant", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneUUID(.*)$str"       => Val(attr, attrClean, "OneUUID", "UUID", "", "uuid", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneURI(.*)$str"        => Val(attr, attrClean, "OneURI", "URI", "", "uri", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"oneAny(.*)$str"        => Val(attr, attrClean, "OneAny", "Any", "", "object", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)

        case r"manyString(.*)$str"     => Val(attr, attrClean, "ManyString", "Set[String]", "String", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyInt(.*)$str"        => Val(attr, attrClean, "ManyInt", "Set[Int]", "Int", "long", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyLong(.*)$str"       => Val(attr, attrClean, "ManyLong", "Set[Long]", "Long", "long", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyFloat(.*)$str"      => Val(attr, attrClean, "ManyFloat", "Set[Float]", "Float", "double", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyDouble(.*)$str"     => Val(attr, attrClean, "ManyDouble", "Set[Double]", "Double", "double", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyBigInt(.*)$str"     => Val(attr, attrClean, "ManyBigInt", "Set[BigInt]", "BigInt", "bigint", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyBigDecimal(.*)$str" => Val(attr, attrClean, "ManyBigDecimal", "Set[BigDecimal]", "BigDecimal", "bigdec", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyByte(.*)$str"       => Val(attr, attrClean, "ManyByte", "Set[Byte]", "Byte", "bytes", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyBoolean(.*)$str"    => Val(attr, attrClean, "ManyBoolean", "Set[Boolean]", "Boolean", "boolean", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyDate(.*)$str"       => Val(attr, attrClean, "ManyDate", "Set[Date]", "Date", "instant", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyUUID(.*)$str"       => Val(attr, attrClean, "ManyUUID", "Set[UUID]", "UUID", "uuid", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"manyURI(.*)$str"        => Val(attr, attrClean, "ManyURI", "Set[URI]", "URI", "uri", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)

        case r"mapString(.*)$str"     => Val(attr, attrClean, "MapString", "Map[String, String]", "String", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapInt(.*)$str"        => Val(attr, attrClean, "MapInt", "Map[String, Int]", "Int", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapLong(.*)$str"       => Val(attr, attrClean, "MapLong", "Map[String, Long]", "Long", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapFloat(.*)$str"      => Val(attr, attrClean, "MapFloat", "Map[String, Float]", "Float", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapDouble(.*)$str"     => Val(attr, attrClean, "MapDouble", "Map[String, Double]", "Double", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapBigInt(.*)$str"     => Val(attr, attrClean, "MapBigInt", "Map[String, BigInt]", "BigInt", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapBigDecimal(.*)$str" => Val(attr, attrClean, "MapBigDecimal", "Map[String, BigDecimal]", "BigDecimal", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapByte(.*)$str"       => Val(attr, attrClean, "MapByte", "Map[String, Byte]", "Byte", "bytes", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapBoolean(.*)$str"    => Val(attr, attrClean, "MapBoolean", "Map[String, Boolean]", "Boolean", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapDate(.*)$str"       => Val(attr, attrClean, "MapDate", "Map[String, Date]", "Date", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapUUID(.*)$str"       => Val(attr, attrClean, "MapUUID", "Map[String, UUID]", "UUID", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"mapURI(.*)$str"        => Val(attr, attrClean, "MapURI", "Map[String, URI]", "URI", "string", parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)

        case r"oneEnum\((.*?)$enums\)(.*)$str" =>
          val rawEnums: Seq[String] = enums.replaceAll("\"", "").split(",").toList.map(_.trim)
          if (rawEnums.forall(_.matches("[a-zA-Z_][a-zA-Z0-9_]*")))
            Enum(attr, attrClean, "OneEnum", "String", "", rawEnums, parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
          else
            throw new SchemaDefinitionException("Enum values can only match `[a-z][a-zA-Z0-9_]*`. Found:\n" + rawEnums.mkString("`", "`\n`", "`"))

        case r"manyEnum\((.*?)$enums\)(.*)$str" =>
          val rawEnums: Seq[String] = enums.replaceAll("\"", "").split(",").toList.map(_.trim)
          if (rawEnums.forall(_.matches("[a-zA-Z_][a-zA-Z0-9_]*")))
            Enum(attr, attrClean, "ManyEnums", "Set[String]", "String", rawEnums, parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
          else
            throw new SchemaDefinitionException("Enum values can only match [a-zA-Z_][a-zA-Z0-9_]*`. Found:\n" + rawEnums.mkString("`", "`\n`", "`"))


        // Bidirectional edge ref

        case r"oneBiEdge\[(.*)$biRef\](.*)$str" =>
          val (refNs, revRef) = parseBiEdgeRefTypeArg("one", biRef, attr, curPart, curFullNs)
          Ref(attr, attrClean, "OneRefAttr", "OneRef", "Long", "", refNs, parseOptions(str, Nil, attr, curFullNs) :+ isComponent, Some("BiEdgeRef_"), revRef, attrGroup = attrGroup0)

        case r"manyBiEdge\[(.*)$biRef\](.*)$str" =>
          val (refNs, revRef) = parseBiEdgeRefTypeArg("many", biRef, attr, curPart, curFullNs)
          Ref(attr, attrClean, "ManyRefAttr", "ManyRef", "Set[Long]", "Long", refNs, parseOptions(str, Nil, attr, curFullNs) :+ isComponent, Some("BiEdgeRef_"), revRef, attrGroup = attrGroup0)


        // Bidirectional ref

        case r"oneBi\[(.*)$biRef\](.*)$str" =>
          val (refNs, bi, revRef) = parseBiRefTypeArg("one", biRef, attr, curPart, curFullNs)
          Ref(attr, attrClean, "OneRefAttr", "OneRef", "Long", "", refNs, parseOptions(str, Nil, attr, curFullNs), Some(bi), revRef, attrGroup = attrGroup0)

        case r"manyBi\[(.*)$biRef\](.*)$str" =>
          val (refNs, bi, revRef) = parseBiRefTypeArg("many", biRef, attr, curPart, curFullNs)
          Ref(attr, attrClean, "ManyRefAttr", "ManyRef", "Set[Long]", "Long", refNs, parseOptions(str, Nil, attr, curFullNs), Some(bi), revRef, attrGroup = attrGroup0)

        // Bidirectional edge target
        case r"target\[(.*)$biTargetRef\](.*)$str" =>
          val (targetNs, revRef) = parseTargetRefTypeArg(biTargetRef, attr, curPart, curFullNs)
          Ref(attr, attrClean, "OneRefAttr", "OneRef", "Long", "", targetNs, parseOptions(str, Nil, attr, curFullNs), Some("BiTargetRef_"), revRef, attrGroup = attrGroup0)


        // Reference

        case r"one\[(.*)$ref\](.*)$str"  => Ref(attr, attrClean, "OneRefAttr", "OneRef", "Long", "", parseRefTypeArg(ref, curPart), parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)
        case r"many\[(.*)$ref\](.*)$str" => Ref(attr, attrClean, "ManyRefAttr", "ManyRef", "Set[Long]", "Long", parseRefTypeArg(ref, curPart), parseOptions(str, Nil, attr, curFullNs), attrGroup = attrGroup0)


        // Missing ref type args

        case r"oneBi(.*)$str" => throw new SchemaDefinitionException(
          s"""Type arg missing for bidirectional ref definition `$attr` in `$curPartDotNs` of $defFileName.
             |Please add something like:
             |  val $attr = oneBi[$curNs] // for bidirectional self-reference, or:
             |  val $attr = oneBi[<otherNamespace>.<revRefAttr>.type] // for "outgoing" bidirectional reference to other namespace""".stripMargin)

        case r"manyBi(.*)$str" => throw new SchemaDefinitionException(
          s"""Type arg missing for bidirectional ref definition `$attr` in `$curPartDotNs` of $defFileName.
             |Please add something like:
             |  val $attr = manyBi[$curNs] // for bidirectional self-reference, or:
             |  val $attr = manyBi[<otherNamespace>.<revRefAttr>.type] // for "outgoing" bidirectional reference to other namespace""".stripMargin)

        case r"rev(.*)$str" => throw new SchemaDefinitionException(
          s"""Type arg missing for bidirectional reverse ref definition `$attr` in `$curPartDotNs` of $defFileName.
             |Please add the namespace where the bidirectional ref pointing to this attribute was defined:
             |  val $attr = rev[<definingNamespace>]""".stripMargin)

        case r"one(.*)$str" => throw new SchemaDefinitionException(
          s"""Type arg missing for ref definition `$attr` in `$curPartDotNs` of $defFileName.
             |Please add something like:
             |  val $attr = one[$curNs] // for self-reference, or
             |  val $attr = one[<otherNamespace>] // for ref towards other namespace""".stripMargin)

        case r"many(.*)$str" => throw new SchemaDefinitionException(
          s"""Type arg missing for ref definition `$attr` in `$curPartDotNs` of $defFileName.
             |Please add something like:
             |  val $attr = many[$curNs] // for self-reference, or
             |  val $attr = many[<otherNamespace>] // for ref towards other namespace""".stripMargin)

        case unexpected => throw new SchemaDefinitionException(s"Unexpected attribute code in $defFileName:\n" + unexpected)
      }
    }

    def parseRefTypeArg(refStr: String, curPartition: String = ""): String = refStr match {
      case r"\w*Definition\.([a-z].*)$partref"  => partref.replace(".", "_")
      case r"([a-z]\w*)$part\.(.*)$ref"         => part + "_" + ref
      case r"(.*)$ref" if curPartition.nonEmpty => curPartition + "_" + ref
      case r"(.*)$ref"                          => ref
    }

    def parseBiEdgeRefTypeArg(card: String, refStr: String, baseAttr: String, basePart: String = "", baseFullNs: String = ""): (String, String) = {

      refStr match {

        // With MyDefinition .......................................

        // val selfRef = oneBi[MyDomainDefinition.ThisPartition.ThisNamespace.selfRef.type]  // or manyBi
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"\w*Definition\.([a-z]\w*)$part\.(.*)$edgeNs\.(.*)$targetAttr\.type" if s"${part}_$edgeNs" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$edgeNs]")

        // val outRefAttr = oneBi[MyDomainDefinition.ThisPartition.OtherNamespace.revRefAttr.type]  // or manyBi
        // should be only
        // val outRefAttr = oneBi[OtherNamespace.revRefAttr.type]
        case r"\w*Definition\.([a-z]\w*)$part\.(.*)$edgeNs\.(.*)$targetAttr\.type" if part == basePart =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName should have " +
            s"only the namespace prefix in the type argument:\n  val $baseAttr = ${card}Bi[$edgeNs.$targetAttr.type]")

        // val outRefAttr = oneBi[MyDomainDefinition.SomePartition.OtherNamespace.toRefAttr.type]
        case r"\w*Definition\.([a-z]\w*)$part\.(.*)$edgeNs\.(.*)$targetAttr\.type" => (s"${part}_$edgeNs", targetAttr)


        // With partition .......................................

        // val selfRef = oneBi[ThisPartition.ThisNamespace.selfRef.type]
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"([a-z]\w*)$part\.(.*)$edgeNs\.(.*)$targetAttr\.type" if s"${part}_$edgeNs" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and can't have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$edgeNs]")

        // val selfRef = oneBi[ThisNamespace.selfRef.type]
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"(.*)$edgeNs\.(.*)$targetAttr\.type" if basePart.nonEmpty && s"${basePart}_$edgeNs" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$edgeNs]")

        // val outgoingRef = oneBi[SomePartition.OtherNamespace.toRefAttr.type]
        case r"([a-z]\w*)$part\.(.*)$edgeNs\.(.*)$targetAttr\.type" => (s"${part}_$edgeNs", targetAttr)


        // With edge ns .......................................

        // val selfRef = oneBi[ThisNamespace.selfRef.type]
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"(.*)$edgeNs\.(.*)$targetAttr\.type" if edgeNs == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$edgeNs]")

        // val outRefAttr = oneBi[OtherNamespace.toRefAttr.type]
        case r"(.*)$edgeNs\.(.*)$targetAttr\.type" if basePart.nonEmpty => (s"${basePart}_$edgeNs", targetAttr)

        // val outRefAttr = oneBi[OtherNamespace.revRefAttr.type]
        case r"(.*)$edgeNs\.(.*)$targetAttr\.type" => (edgeNs, targetAttr)

        // Incorrect edge definition
        // val selfRef = oneBi[selfRef.type] // presuming it hasn't been imported from another namespace
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"(.*)$a\.type" if a == baseAttr =>
          val ns = if (basePart.nonEmpty) baseFullNs.split("_").last else baseFullNs
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and only needs the current namespace as type argument:\n  val $baseAttr = ${card}Bi[$ns]")
      }
    }

    def parseTargetRefTypeArg(refStr: String, baseAttr: String, basePart: String = "", baseFullNs: String = ""): (String, String) = {
      refStr match {

        // val outRefAttr = oneBi[OtherNamespace.revRefAttr.type]
        case r"(.*)$targetNs\.(.*)$targetAttr\.type" if basePart.nonEmpty => (s"${basePart}_$targetNs", targetAttr)

        // val outRefAttr = oneBi[OtherNamespace.revRefAttr.type]
        case r"(.*)$targetNs\.(.*)$targetAttr\.type" => (targetNs, targetAttr)

        case other =>
          throw new SchemaDefinitionException(
            s"""Target reference `$baseAttr` in `$baseFullNs` of $defFileName should have a type arg pointing to
               |the attribute that points to this. Something like:
               |  val $baseAttr: AnyRef = target[<baseNs>.<biAttr>.type]
               |(Since this is a recursive definitionn, we need to add a return type)""".stripMargin)
      }
    }

    def parseBiRefTypeArg(card: String, refStr: String, baseAttr: String, basePart: String = "", baseFullNs: String = ""): (String, String, String) = {

      refStr match {

        // val selfRef = oneBi[MyDomainDefinition.ThisPartition.ThisNamespace.selfRef.type]  // or manyBi
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"\w*Definition\.([a-z]\w*)$part\.(.*)$otherNs\.(.*)$targetAttr\.type" if s"${part}_$otherNs" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$otherNs]")

        // val selfRef = oneBi[ThisPartition.ThisNamespace.selfRef.type]
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"([a-z]\w*)$part\.(.*)$otherNs\.(.*)$targetAttr\.type" if s"${part}_$otherNs" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$otherNs]")

        // val selfRef = oneBi[ThisNamespace.selfRef.type]
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"(.*)$otherNs\.(.*)$targetAttr\.type" if otherNs == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$otherNs]")


        // val otherRef = oneBi[MyDomainDefinition.SomePartition.OtherNamespace.toRefAttr.type]
        case r"\w*Definition\.([a-z]\w*)$part\.(.*)$otherNs\.(.*)$targetAttr\.type" => (s"${part}_$otherNs", "BiOtherRef_", targetAttr)

        // val otherRef = oneBi[SomePartition.OtherNamespace.revRefAttr.type]
        case r"([a-z]\w*)$part\.(.*)$otherNs\.(.*)$targetAttr\.type" => (s"${part}_$otherNs", "BiOtherRef_", targetAttr)

        // val otherRef = oneBi[OtherNamespace.toRefAttr.type]
        case r"(.*)$otherNs\.(.*)$targetAttr\.type" if basePart.nonEmpty => (s"${basePart}_$otherNs", "BiOtherRef_", targetAttr)

        // val otherRef = oneBi[OtherNamespace.revRefAttr.type]
        case r"(.*)$otherNs\.(.*)$targetAttr\.type" => (otherNs, "BiOtherRef_", targetAttr)


        // val selfRef = oneBi[MyDomainDefinition.ThisPartition.ThisNamespace] // or manyBi
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"\w*Definition\.([a-z]\w*)$part\.(.*)$selfRef" if s"${part}_$selfRef" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have the attribute name specified. This is enough:\n  val $baseAttr = ${card}Bi[$selfRef]")

        // val selfRef = oneBi[ThisPartition.ThisNamespace]
        // should be only
        // val selfRef = oneBi[ThisNamespace]
        case r"([a-z]\w*)$part\.(.*)$selfRef" if s"${part}_$selfRef" == baseFullNs =>
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is a self-reference " +
            s"and doesn't need to have partition prefix specified. This is enough:\n  val $baseAttr = ${card}Bi[$selfRef]")

        // val selfRef = oneBi[ThisNamespace]
        case selfNs if basePart.nonEmpty && s"${basePart}_$selfNs" == baseFullNs => (s"${basePart}_$selfNs", "BiSelfRef_", "")

        // val selfRef = oneBi[ThisNamespace]
        case selfNs if selfNs == baseFullNs => (selfNs, "BiSelfRef_", "")

        // val selfRef = oneBi[OtherNamespace]
        case dodgyNs =>
          val part = if (basePart.nonEmpty) s"$basePart." else ""
          throw new SchemaDefinitionException(s"Bidirectional reference `$baseAttr` in `$baseFullNs` of $defFileName is ambiguous. " +
            s"\nPlease choose from one of those 2 options:" +
            s"\n1. Self-reference : val $baseAttr = ${card}Bi[${baseFullNs.replace("_", ".")}]" +
            s"\n2. Other-reference: val $baseAttr = ${card}Bi[$part$dodgyNs.<reverseRefAttr>.type]" +
            s"\nwhere <reverseRefAttr> is a ref in the other namespace that points back to this ref attribute like:" +
            s"\nval reverseRefAttr = ${card}Bi[${baseFullNs.replace("_", ".")}.$baseAttr.type]"
          )
      }
    }

    def someDescr(descr: String): Option[String] = if (descr.nonEmpty) Some(descr) else None

    def attrCmt(emptyLine: Int, comment: String): Option[String] = (emptyLine, comment) match {
      case (0, "")  => None
      case (1, "")  => Some("")
      case (_, cmt) => Some(cmt)
    }

    val definition: Definition = lines.zipWithIndex.foldLeft(0, "", Definition("", -1, -1, "", "", "", Seq())) {
      case ((emptyLine, cmt, d), (line, i)) => line.trim match {
        case r"\/\/\s*val .*"                                   => (emptyLine, "", d)
        case r"\/\/\s*(.*?)$comment\s*-*"                       => (emptyLine, comment, d)
        case r"package (.*)$pkg\.schema"                        => (0, "", d.copy(pkg = pkg))
        case r"import molecule\.schema\.definition._"           => (0, "", d)
        case r"import molecule\.schema\.definition\.(.*)$t"     => throw new SchemaDefinitionException(s"Schema definition api in $defFileName (line ${i + 1}) should be imported with `import molecule.schema.definition._`")
        case r"@InOut\((\d+)$inS, (\d+)$outS\)"                 => (0, "", d.copy(in = inS.toString.toInt, out = outS.toString.toInt))
        case r"object\s+([A-Z][a-zA-Z0-9]*)${dmn}Definition \{" => (0, "", d.copy(domain = dmn))

        // Partition definitions
        case r"object\s+(tx|db|molecule)$part\s*\{"    => throw new SchemaDefinitionException(s"Partition name '$part' in $defFileName (line ${i + 1}) is not allowed. `tx`, `db` and `molecule` are reserved partition names.")
        case r"object\s+([a-z][a-zA-Z0-9]*)$part\s*\{" => (0, "", d.copy(curPart = part, curPartDescr = cmt, nss = d.nss :+ Namespace(part, someDescr(cmt), "", None)))
        case r"object\s+([A-Z][a-zA-Z0-9]*)$part\s*\{" => throw new SchemaDefinitionException(s"Partition name '$part' in $defFileName (line ${i + 1}) should start with a lowercase letter and contain only [a-zA-Z0-9].")
        case r"object\s+(.*)$part\s*\{"                => throw new SchemaDefinitionException(s"Unexpected partition name '$part' in $defFileName (line ${i + 1}).\nPartition names have to start with a lowercase letter and contain only [a-zA-Z0-9].")

        // Ns definitions
        case r"trait\s+([A-Z][a-zA-Z0-9]*)$ns\s*\{" if d.curPart.nonEmpty => (0, "", d.copy(nss = (if (d.nss.last.ns.isEmpty) d.nss.init else d.nss) :+ Namespace(d.curPart, someDescr(d.curPartDescr), d.curPart + "_" + ns, someDescr(cmt))))
        case r"trait\s+([A-Z][a-zA-Z0-9]*)$ns\s*\{"                       => (0, "", d.copy(nss = d.nss :+ Namespace("db.part/user", None, ns, someDescr(cmt))))
        case r"trait\s+([a-z][a-zA-Z0-9]*)$ns\s*\{"                       => throw new SchemaDefinitionException(s"Namespace name '$ns' in $defFileName (line ${i + 1}) should start with a capital letter and contain only [a-zA-Z0-9].")
        case r"trait\s+(.*)$ns\s*\{"                                      => throw new SchemaDefinitionException(s"Unexpected namespace name '$ns' in $defFileName (line ${i + 1}).\nNamespace names have to start with a capital letter [A-Z] and contain only [a-zA-Z0-9].")

        // Attribute definitions
        case r"val\s+(\`?)${q1}get(\w*)$a(\`?)$q2\s*\=\s*(.*)$str"                       => throw new SchemaDefinitionException(s"Attribute name `get$a` not allowed to start with `get` in $defFileName (line ${i + 1}).")
        case r"val\s+(\`?)$q1([a-z][a-zA-Z0-9]*)$a(\`?)$q2\s*:\s*AnyRef\s*\=\s*(.*)$str" => (0, "", d.addAttr(parseAttr(q1.nonEmpty, a, str, d.curPart, d.nss.last.ns, attrCmt(emptyLine, cmt))))
        case r"val\s+(\`?)$q1([a-z][a-zA-Z0-9]*)$a(\`?)$q2\s*\=\s*(.*)$str"              => (0, "", d.addAttr(parseAttr(q1.nonEmpty, a, str, d.curPart, d.nss.last.ns, attrCmt(emptyLine, cmt))))
        case r"val\s+(\`?)$q1([A-Z][a-zA-Z0-9]*)$a(\`?)$q2\s*:\s*AnyRef\s*\=\s*(.*)$str" => throw new SchemaDefinitionException(s"Attribute name `$a` in $defFileName (line ${i + 1}) should start with a lowercase letter and contain only [a-zA-Z0-9].")
        case r"val\s+(\`?)$q1([A-Z][a-zA-Z0-9]*)$a(\`?)$q2\s*\=\s*(.*)$str"              => throw new SchemaDefinitionException(s"Attribute name `$a` in $defFileName (line ${i + 1}) should start with a lowercase letter and contain only [a-zA-Z0-9].")
        case r"val\s+(\`?)$q1(.*)$a(\`?)$q2\s*:\s*AnyRef\s*\=\s*(.*)$str"                => throw new SchemaDefinitionException(s"Unexpected attribute name `$a` in $defFileName (line ${i + 1}).\nAttribute names have to start with lower letter [a-z] and contain only [a-zA-Z0-9].")
        case r"val\s+(\`?)$q1(.*)$a(\`?)$q2\s*\=\s*(.*)$str"                             => throw new SchemaDefinitionException(s"Unexpected attribute name `$a` in $defFileName (line ${i + 1}).\nAttribute names have to start with lower letter [a-z] and contain only [a-zA-Z0-9].")

        case "}"                     => (0, "", d)
        case ""                      => (1, cmt, d)
        case r"object .* extends .*" => (0, "", d)
        case unexpected              => throw new SchemaDefinitionException(s"Unexpected definition code in $defFileName (line ${i + 1}):\n" + unexpected)
      }
    }._3

    resolve(definition)
  }


  def resolve(definition: Definition): Definition = {
    val updatedNss1: Seq[Namespace] = markBidrectionalEdgeProperties(definition.nss)
    val updatedNss3: Seq[Namespace] = definition.nss.foldLeft(updatedNss1) { case (updatedNss2, curNs) =>
      addBackRefs(updatedNss2, curNs)
    }
    val updatedNss4: Seq[Namespace] = resolveEdgeToOther(updatedNss3)
    definition.copy(nss = updatedNss4)
  }

  def resolveEdgeToOther(nss: Seq[Namespace]): Seq[Namespace] = nss.map { ns =>
    val isBaseEntity: Boolean = ns.attrs.collectFirst {
      case Ref(attr, _, _, _, _, _, refNs, _, Some("BiEdgeRef_"), revRef, _) => true
    } getOrElse false

    if (isBaseEntity) {
      val newAttrs: Seq[DefAttr] = ns.attrs.map {
        case biEdgeRefAttr@Ref(attr1, _, _, _, _, _, edgeNs1, _, Some("BiEdgeRef_"), revRef1, _) =>
          nss.collectFirst {
            case Namespace(part2, _, ns2, _, _, attrs2) if part2 == ns.part && ns2 == edgeNs1 =>
              attrs2.collectFirst {
                case Ref(attr3, _, _, _, _, _, refNs3, _, Some("BiTargetRef_"), revRef3, _) if refNs3 == ns.ns =>
                  biEdgeRefAttr.copy(revRef = attr3)
              } getOrElse {
                val baseNs = ns.ns.replace("_", ".")
                throw new SchemaDefinitionException(s"Couldn't find target reference in edge namespace `${edgeNs1.replace("_", ".")}` that points back to `$baseNs.$attr1`. " +
                  s"Expecting something like:\nval ${firstLow(baseNs.split('.').last)} = target[${baseNs.split('.').last}.$attr1.type]")
              }
          } getOrElse {
            val baseNs = ns.ns.replace("_", ".")
            throw new SchemaDefinitionException(s"Couldn't find target reference in edge namespace `${edgeNs1.replace("_", ".")}` that points back to `$baseNs.$attr1`. " +
              s"Expecting something like:\nval ${firstLow(baseNs.split('.').last)} = target[${baseNs.split('.').last}.$attr1.type]")
          }
        case other                                                                               => other
      }
      ns.copy(attrs = newAttrs)
    } else {
      ns
    }
  }

  def markBidrectionalEdgeProperties(nss: Seq[Namespace]): Seq[Namespace] = nss.map { ns =>

    val isEdge: Boolean = ns.attrs.collectFirst {
      case Ref(_, _, _, _, _, _, _, _, Some("BiTargetRef_"), _, _) => true
    } getOrElse false

    if (isEdge) {
      val newAttrs: Seq[DefAttr] = ns.attrs.map {
        case biEdgeRefAttr@Ref(_, _, _, _, _, _, _, _, Some("BiEdgeRefAttr_"), _, _) => biEdgeRefAttr

        case biTargetRef@Ref(_, _, _, _, _, _, _, _, Some("BiTargetRef_"), _, _) => biTargetRef

        case Ref(attr, _, _, _, _, _, _, _, Some(bi), _, _) if bi.substring(6, 10) != "Prop" => throw new SchemaDefinitionException(
          s"""Namespace `${ns.ns}` is already defined as a "property edge" and can't also define a bidirectional reference `$attr`.""")

        case ref: Ref   => ref.copy(bi = Some("BiEdgePropRef_"))
        case enum: Enum => enum.copy(bi = Some("BiEdgePropAttr_"))
        case value: Val => value.copy(bi = Some("BiEdgePropAttr_"))
        case other      => other
      }
      ns.copy(opt = Some(Edge), attrs = newAttrs)
    } else {
      ns
    }
  }

  def addBackRefs(nss: Seq[Namespace], curNs: Namespace): Seq[Namespace] = {
    // Gather OneRefs (ManyRefs are treated as nested data structures)
    val refMap: Map[String, Ref] = curNs.attrs.collect {
      case outRef@Ref(_, _, _, _, _, _, refNs, _, _, _, _) => refNs -> outRef
    }.toMap

    nss.map {
      case ns2 if refMap.nonEmpty && refMap.keys.toList.contains(ns2.ns) => {
        val attrs2 = refMap.foldLeft(ns2.attrs) { case (attrs, (refNs, outRef@Ref(_, _, _, _, tpe, _, _, _, _, _, _))) =>
          val cleanNs = if (curNs.ns.contains('_')) curNs.ns.split("_").tail.head else curNs.ns
          // todo: check not to backreference same-named namespaces in different partitions
          curNs.ns match {
            case ns1 if ns1 == ns2.ns => attrs
            case other                => attrs :+ BackRef(s"_$cleanNs", s"_$cleanNs", "", "", "", "", curNs.ns)
          }
        }.distinct
        ns2.copy(attrs = attrs2)
      }
      case ns2                                                           => ns2
    }
  }
}
