package moleculeadmin.server.utils
import java.io.File
import ammonite.ops._
import db.admin.dsl.moleculeAdmin._
import molecule.api.out10._
import molecule.facade.Conn
import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.util.HelpersAdmin
import sbtmolecule.Ast._
import sbtmolecule.DefinitionParser
import scala.io.Source

case class DefFile(db: String, path: Option[String] = None, schemaDefFile: Option[File] = None) extends BaseApi with HelpersAdmin {

  implicit val conn = Conn(base + "/MoleculeAdmin")

  def parse(defFile: File): Definition = DefinitionParser(defFile.getName, Source.fromFile(defFile).getLines().toList).parse

  def getFile(path: String): Either[String, File] = {
    val file = new File(path)
    if (!file.isFile) {
      Left(s"Can't find definition file in path '$path'")
    } else {
      Right(file)
    }
  }

  def getDefFile: Either[String, File] = schemaDefFile match {
    case Some(defFile) => Right(defFile)
    case None          => path match {
      case Some(suppliedPath) => getFile(suppliedPath)
      case None               => meta_Db.name_(db).defFilePath.get.headOption match {
        case Some(pathFromDb) => getFile(pathFromDb)
        case None             => Left(s"Missing definition file path in database `$db`")
      }
    }
  }

  def saveToMetaDb: Either[String, Long] = getDefFile match {
    case Left(msg)          => Left(msg)
    case Right(defFilePath) =>
      val d                   = parse(defFilePath)
      val (nss, pkg, in, out) = (d.nss, d.pkg, d.in, d.out)

      // Use id of existing meta db or create new meta db
      val dbId = meta_Db.e.name_(db).get.headOption.getOrElse(meta_Db.name(db).save.eid)

      // Recursively retract partitions, namespaces and attributes
      meta_Db(dbId).partitions.get.foreach(_.foreach(_.retract))

      val str0   = Option.empty[String]
      val enums0 = Set.empty[String]

      def refNsAttrs(ns0: String): Seq[String] = nss.filter(_.ns == ns0).head.attrs.map(_.attrClean)

      val partitions: Seq[(Int, String, Option[String], Set[Long])] = for {
        ((part, partDescr), i) <- nss.map(ns => (ns.part, ns.partDescr)).distinct.zipWithIndex
        partN = i + 1
      } yield {
        var nsN                                                               = 0
        val namespaces: Seq[(Int, String, String, Option[String], Set[Long])] = for {
          ns <- nss if ns.part == part && ns.ns.nonEmpty // exclude namespaces with no attributes defined (ns name is empty)
        } yield {
          nsN += 1
          val attributes: Seq[(Int, String, Int, String, Set[String], Option[String], Set[String], Option[String], Option[String], Option[String])] = (for {
            (attr, k) <- ns.attrs.zipWithIndex
            attrN = k + 1
          } yield attr match {
            case Enum(a, ac, clazz, _, _, enums, ops, _, _, attrGroup) =>
              val (card, tpe) = cardType(clazz)
              Some((attrN, ac, card, tpe, enums.toSet, str0, opts(ops), doc(ops), attrGroup, str0))

            case Val(a, ac, clazz, _, _, _, ops, _, _, attrGroup) =>
              val (card, tpe) = cardType(clazz)
              Some((attrN, ac, card, tpe, enums0, str0, opts(ops), doc(ops), attrGroup, str0))

            case Ref(a, ac, clazz, _, _, _, refNs, ops, bi, _, attrGroup) =>
              val (card, tpe) = cardType(clazz, bi)
              val descrAttr   = Seq("nameSort", "name").find(refNsAttrs(refNs) contains _)
              Some((attrN, ac, card, tpe, enums0, Some(refNs), opts(ops), doc(ops), attrGroup, descrAttr))

            case _: BackRef => None
          }).flatten

          val attrIds = if (attributes.isEmpty) Set.empty[Long] else
              meta_Attribute.pos.name.card.tpe.enums.refNs$.options.doc$.attrGroup$.descrAttr$.insert(attributes).eidSet
          (nsN, ns.ns.split("_").last, ns.ns, ns.nsDescr, attrIds)
        }

        val nsIds = if(namespaces.isEmpty) Set.empty[Long] else
          meta_Namespace.pos.name.nameFull.descr$.attrs.insert(namespaces).eidSet
        (partN, part.low, partDescr, nsIds)
      }

      val partIds = if (partitions.isEmpty) Set.empty[Long] else
        meta_Partition.pos.name.descr$.namespaces.insert(partitions).eidSet

      // Update meta db
      path match {
        case Some(path1) => meta_Db(dbId).isMolecular(true).defFilePath(path1).pkg(pkg).inputArity(in).outputArity(out).partitions(partIds).update
        case None        => meta_Db(dbId).isMolecular(true).partitions(partIds).update
      }

      Right(dbId)
  }


  def getDefFileSchema: FlatSchema = {
    val defFile = getDefFile match {
      case Left(msg)       => throw new RuntimeException(msg) // lazy for now
      case Right(defFile1) => defFile1
    }
    var i       = 0
    (for {
      Namespace(part, partDescr, ns, nsDescr, _, attrs) <- parse(defFile).nss
      attrDef <- attrs if attrDef.attr.head != '_'
    } yield {
      attrDef match {
        case Enum(_, attr, clazz, _, _, enums, ops, _, _, attrGroup) =>
          i += 1
          val options     = opts2(ops).filterNot(_ == "indexed").sorted
          val (card, tpe) = cardType(clazz)
          Some(FlatAttr(i, part, partDescr, ns.split("_").last, ns, nsDescr, attr, card, tpe, enums, None, options, doc(ops), attrGroup))

        case Val(_, attr, clazz, _, _, _, ops, _, _, attrGroup) =>
          i += 1
          val options     = opts2(ops).filterNot(_ == "indexed").sorted
          val (card, tpe) = cardType(clazz)
          Some(FlatAttr(i, part, partDescr, ns.split("_").last, ns, nsDescr, attr, card, tpe, Nil, None, options, doc(ops), attrGroup))

        case Ref(_, attr, clazz, _, _, _, refNs, ops, bi, _, attrGroup) =>
          i += 1
          val options     = opts2(ops).filterNot(_ == "indexed").sorted
          val (card, tpe) = cardType(clazz, bi)
          Some(FlatAttr(i, part, partDescr, ns.split("_").last, ns, nsDescr, attr, card, tpe, Nil, Some(refNs), options, doc(ops), attrGroup))
        //          Some(Attr(i, part, partDescr, ns.split("_").last, ns, nsDescr, attr, card, tpe, Nil, Some(refNs.low), options, doc(ops), attrGroup))

        case _ => None // BackRef's ignored
      }
    }).flatten
  }


  private def doc(options: Seq[Optional]): Option[String] = options
    .map(_.datomicKeyValue)
    .find(_.startsWith("""":db/doc"""))
    .map(_.split(""", """").last.init)

  private def opts(options: Seq[Optional]): Set[String] = options
    .map(_.clazz)
    .filter(_.nonEmpty)
    //    .filterNot(_ == "Indexed")
    .map(o => firstLow(o.split('[').head))
    .toSet

  private def opts2(options: Seq[Optional]): Seq[String] = options
    .map(_.clazz)
    .filter(_.nonEmpty)
    //    .filterNot(_ == "Indexed")
    .map(o => firstLow(o.split('[').head))

  private def cardType(clazz: String, bi: Option[String] = None): (Int, String) = clazz match {
    case "OneString"     => (1, "String")
    case "OneInt"        => (1, "Int")
    case "OneLong"       => (1, "Long")
    case "OneFloat"      => (1, "Float")
    case "OneDouble"     => (1, "Double")
    case "OneBigInt"     => (1, "BigInt")
    case "OneBigDecimal" => (1, "BigDecimal")
    case "OneBoolean"    => (1, "Boolean")
    case "OneDate"       => (1, "Date")
    case "OneUUID"       => (1, "UUID")
    case "OneURI"        => (1, "URI")
    case "OneByte"       => (1, "Byte")

    case "ManyString"     => (2, "String")
    case "ManyInt"        => (2, "Int")
    case "ManyLong"       => (2, "Long")
    case "ManyFloat"      => (2, "Float")
    case "ManyDouble"     => (2, "Double")
    case "ManyBigInt"     => (2, "BigInt")
    case "ManyBigDecimal" => (2, "BigDecimal")
    case "ManyBoolean"    => (2, "Boolean")
    case "ManyDate"       => (2, "Date")
    case "ManyUUID"       => (2, "UUID")
    case "ManyURI"        => (2, "URI")
    case "ManyByte"       => (2, "Byte")

    case "MapString"     => (3, "String")
    case "MapInt"        => (3, "Int")
    case "MapLong"       => (3, "Long")
    case "MapFloat"      => (3, "Float")
    case "MapDouble"     => (3, "Double")
    case "MapBigInt"     => (3, "BigInt")
    case "MapBigDecimal" => (3, "BigDecimal")
    case "MapBoolean"    => (3, "Boolean")
    case "MapDate"       => (3, "Date")
    case "MapUUID"       => (3, "UUID")
    case "MapURI"        => (3, "URI")
    case "MapByte"       => (3, "Byte")

    case "OneEnum"   => (1, "String")
    case "ManyEnums" => (2, "String")

    // todo: test all bidirectional combinations!
    case "OneRefAttr" if bi.contains("BiEdgeRef_")         => (1, "biEdge")
    case "OneRefAttr" if bi.contains("BiTargetRef_")       => (1, "target")
    case "OneRefAttr" if bi.getOrElse("").startsWith("Bi") => (1, "bi")
    case "OneRefAttr"                                      => (1, "ref")

    case "ManyRefAttr" if bi.contains("BiEdgeRef_")         => (2, "biEdge")
    case "ManyRefAttr" if bi.getOrElse("").startsWith("Bi") => (2, "bi")
    case "ManyRefAttr"                                      => (2, "ref")

    case other => throw new RuntimeException(s"Unexpected clazz: `$other`")
  }


  def recreateFrom(metaSchema: MetaSchema): Either[String, MetaSchema] = {

    def genAttr(part: String, attribute: Attr, longest: Int, allAttrs: Seq[String]): String = {
      val Attr(i, attr, card0, attrType, enums0, ref, options0, doc0, attrGroup, _, _, descrAttr, _) = attribute
      val valIndent                                                                                  = if (part == "db.part/user") "    " else "      "

      val attrFull = if (scalaKeywords.contains(attr)) s"`$attr`" else attr

      val s = " " * (longest - attrFull.length)

      val cardPrefix = card0 match {
        case 1 => "one"
        case 2 => "many"
        case 3 => "map"
      }

      val tpe = if (enums0.isDefined)
        cardPrefix + "Enum"
      else
        ref match {
          case None                               => cardPrefix + attrType.capitalize
          case Some(refNs) if refNs.contains('_') =>
            val Array(refPart, refNs1) = refNs.split("_")
            val resolvedNs             = if (refPart == part)
              refNs1
            else if (allAttrs.contains(refPart))
              db.capitalize + s"Definition.$refPart.$refNs1"
            else
              s"$refPart.$refNs1"
            cardPrefix + s"[$resolvedNs]"
          case Some(refNs)                        => cardPrefix + "[" + refNs.capitalize + "]"
        }

      val enums = enums0 match {
        case None     => ""
        case Some(es) =>
          es.toList.sorted.mkString("(\"", "\", \"", "\")")
      }

      val options = options0.map(_.filterNot(_ == "indexed")) match {
        case None       => ""
        case Some(opts) => "." + opts.mkString(".")
      }

      val doc = doc0 match {
        case None    => ""
        case Some(d) => s""".doc("$d")"""
      }

      val descrVal = attrGroup match {
        case None     =>
          valIndent
        case Some("") =>
          s"""
             |$valIndent""".stripMargin
        case Some(d)  =>
          s"""
             |$valIndent// $d
             |$valIndent""".stripMargin
      }
      s"${descrVal}val $attrFull $s= $tpe$enums$options$doc"
    }

    def genNs(part: String, namespace: Ns): String = {
      val Ns(_, ns, _, descr0, _, attrs) = namespace
      val nsIndent                       = if (part == "db.part/user") "  " else "    "
      if (ns.isEmpty) "" else {
        val allAttrs = attrs.map(_.name)

        val attrGroups: Seq[Seq[Attr]] = attrs.foldLeft(Seq.empty[Seq[Attr]]) {
          case (groups, a) if groups.isEmpty         => Seq(Seq(a))
          case (groups, a) if a.attrGroup$.isDefined => groups :+ Seq(a)
          case (groups, a) if groups.size == 1       => Seq(groups.head :+ a)
          case (groups, a)                           => groups.init :+ (groups.last :+ a)
        }

        val attrGroupSections = attrGroups.map { g =>
          val longest: Int         = if (g.nonEmpty) g.map(_.name.length).max else 0
          val attrs1 : Seq[String] = g.map(attr => genAttr(part, attr, longest, allAttrs))
          attrs1.mkString("\n")
        }

        val descrNs = descr0 match {
          case None    => nsIndent
          case Some(d) =>
            s"""$nsIndent// $d
               |$nsIndent""".stripMargin
        }
        s"""
           |${descrNs}trait ${ns.capitalize} {
           |${attrGroupSections.mkString("\n")}
           |$nsIndent}""".stripMargin
      }
    }

    def genPart(partition: Part): String = {
      val Part(_, part, descr0, _, nss) = partition
      val descr                         = descr0 match {
        case None    => "  "
        case Some(d) =>
          val line = "-" * (90 - d.length)
          s"""
             |  // $d $line
             |
             |  """.stripMargin
      }

      if (part == "db.part/user") {
        nss.map(ns => genNs(part, ns)) mkString "\n"
      } else {
        s"""
           |${descr}object $part {
           |${nss.map(ns => genNs(part, ns)) mkString "\n"}
           |  }""".stripMargin
      }
    }

    getDefFile match {
      case Left(msg)      =>
        Left(msg)
      case Right(defFile) =>
        implicit val conn = Conn(base + "/MoleculeAdmin")
        meta_Db.name_(db).defFilePath.pkg.inputArity.outputArity.get match {
          case Seq((path1, pkg, inputArity, outputArity)) =>
            val code =
              s"""package $pkg.schema
                 |import molecule.schema.definition._
                 |
                 |@InOut($inputArity, $outputArity)
                 |object ${db.capitalize}Definition {
                 |${metaSchema.parts map genPart mkString "\n"}
                 |}
                 |""".stripMargin

            val filePath = path1.tail.split('/').foldLeft(root)((acc, seg) => acc / seg)
            write.over(filePath, code)
            //Thread.sleep(2000)
            Right(metaSchema)

          case Nil      => Left(s"Missing meta data on database `$db`")
          case multiple => Left(s"Unexpected found $multiple instances of meta database `$db`")
        }
    }
  }
}