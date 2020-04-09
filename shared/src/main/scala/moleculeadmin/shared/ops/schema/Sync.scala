package moleculeadmin.shared.ops.schema

import moleculeadmin.shared.api.SchemaApi
import moleculeadmin.shared.ast.schema.FlatAttr


case class Sync(baseSchema: Seq[FlatAttr], testSchema: Seq[FlatAttr]) extends SchemaApi {


  def part: Option[(Seq[(String, Boolean)], Seq[(String, Boolean)])] = {
    val basePartitions: Seq[String] = baseSchema.map(_.part).distinct.sorted

    if (basePartitions.isEmpty || (basePartitions.length == 1 && basePartitions.head.isEmpty)) {
      None
    } else {
      val testPartitions: Seq[String] = testSchema.map(_.part).distinct.sorted

      val partsNotInTest = basePartitions.diff(testPartitions)
      val partsNotInBase = testPartitions.diff(basePartitions)

      if (partsNotInTest.isEmpty && partsNotInBase.isEmpty) {
        None
      } else {
        // Prepare base part data
        val basePartData = basePartitions.map {
          case part if partsNotInTest.contains(part) => (part, false)
          case part                                  => (part, true)
        }
        // Prepare test part data
        val testPartData = testPartitions.map {
          case part if partsNotInBase.contains(part) => (part, false)
          case part                                  => (part, true)
        }
        Some(basePartData, testPartData)
      }
    }
  }


  def ns: Seq[(String, Seq[(String, Boolean)], Seq[(String, Boolean)])] = {

    val testNss: Seq[(String, Seq[String])] = testSchema.groupBy(_.part).toSeq.sortBy(_._2.head.pos).map(g => g._1 -> g._2.map(_.ns).distinct.sorted)

    val baseNss: Map[String, Seq[String]] = {
      // Double-checking that we have the same partitions
      val testPartitions = testSchema.map(_.part).distinct
      val basePartitions = baseSchema.map(_.part).distinct

      //      println("baseSchema")
      //      baseSchema foreach println
      //      println("---------------------------------")
      //      println("testSchema")
      //      testSchema foreach println
      //      println("========================================")
      //      println("basePartitions")
      //      basePartitions foreach println
      //      println("---------------------------------")
      //      println("testPartitions")
      //      testPartitions foreach println

      if (testPartitions.diff(basePartitions).nonEmpty)
        throw new RuntimeException(
          "Base and test partitions are unexpectedly not equal. Maybe the database has been created " +
            "from a previous version of the schema definition file? " +
            "Check for instance if the generated Schema transaction file matches the schema definition file.")

      baseSchema.groupBy(_.part).map { g =>
        g._1 -> g._2.map(_.ns).distinct.sorted
      }
    }

    testNss.flatMap { case (part, testNssInPart) =>
      val baseNssInPart       : Seq[String] = baseNss(part)
      val testNssNotInBasePart: Seq[String] = testNssInPart.diff(baseNssInPart)
      val baseNssNotInTestPart: Seq[String] = baseNssInPart.diff(testNssInPart)

      if (testNssNotInBasePart.isEmpty && baseNssNotInTestPart.isEmpty) {
        // test/base ns are in sync - no error data
        None
      } else {
        // Prepare base ns data
        val baseNssData = baseNssInPart.map {
          case ns if baseNssNotInTestPart.contains(ns) => (ns, false)
          case ns                                      => (ns, true)
        }

        // Prepare test ns data
        val testNssData = testNssInPart.map {
          case ns if testNssNotInBasePart.contains(ns) => (ns, false)
          case ns                                      => (ns, true)
        }

        Some((part, baseNssData, testNssData))
      }
    }
  }


  def attr: Seq[(String, String, Seq[(String, Boolean)], Seq[(String, Boolean)])] = {

    val testAttrs: Seq[(String, (String, String, Seq[String]))] = testSchema.groupBy(_.nsFull).toSeq.sortBy(_._2.head.pos).map { g =>
      val firstAttr       = g._2.head
      val (part, nsAlias) = (firstAttr.part, firstAttr.ns)
      val testAttrNames   = g._2.map(_.attr).distinct.sorted
      g._1 -> (part, nsAlias, testAttrNames)
    }

    val baseAttrs: Map[String, Seq[String]] = {
      // Double-checking that we have the same namespaces
      val testNss = testSchema.map(_.nsFull).distinct
      val baseNss = baseSchema.map(_.nsFull).distinct
      assert(testNss.diff(baseNss).isEmpty)

      baseSchema.groupBy(_.nsFull).map { g =>
        val baseAttrNames = g._2.map(_.attr).distinct.sorted
        g._1 -> baseAttrNames
      }
    }

    testAttrs.flatMap { case (nsFull, (part, nsAlias, testAttrsInNs)) =>
      val baseAttrsInNs       : Seq[String] = baseAttrs(nsFull)
      val testAttrsNotInBaseNs: Seq[String] = testAttrsInNs.diff(baseAttrsInNs)
      val baseAttrsNotInTestNs: Seq[String] = baseAttrsInNs.diff(testAttrsInNs)

      if (testAttrsNotInBaseNs.isEmpty && baseAttrsNotInTestNs.isEmpty) {
        // test/base ns are in sync - no error data
        None
      } else {
        // Prepare base attr data
        val baseAttrsData = baseAttrsInNs.map {
          case attr if baseAttrsNotInTestNs.contains(attr) => (attr, false)
          case attr                                        => (attr, true)
        }

        // Prepare test attr data
        val testAttrsData = testAttrsInNs.map {
          case attr if testAttrsNotInBaseNs.contains(attr) => (attr, false)
          case attr                                        => (attr, true)
        }

        Some((part, nsAlias, baseAttrsData, testAttrsData))
      }
    }
  }


  def attrCard: Seq[(String, String, Seq[(String, Int, Int, Boolean, String)])] = {

    val baseCardinalities: Map[String, Int] = baseSchema.map(a => a.nsFull + "_" + a.attr -> a.card).toMap

    testSchema.groupBy(_.nsFull).toSeq.sortBy(_._2.head.pos).flatMap { case (_, attrs) =>
      val (okAttr, attrCardsWithError) = attrs.sortBy(_.pos).foldLeft(
        true,
        Seq.empty[(String, Int, Int, Boolean, String)]
      ) {
        case ((prevOk, acc), a) =>
          val attrKey       = a.nsFull + "_" + a.attr
          val baseCard      = baseCardinalities(attrKey)
          val (ok, comment) = (baseCard, a.card) match {
            case (2, 3)           => (true, " (Ok, cardinality '3' indicates mapped values)")
            case (2, 4)           => (true, " (Ok, cardinality '4' indicates keyed mapped values)")
            case (l, d) if l != d => (false, "")
            case (_, _)           => (true, "")
          }
          (prevOk && ok, acc :+ (a.attr, baseCard, a.card, ok, comment))
      }

      if (okAttr) None else Some(attrs.head.part, attrs.head.ns, attrCardsWithError)
    }
  }


  def attrType: Seq[(String, String, Seq[(String, String, String, Boolean, String)])] = {

    val baseTypes: Map[String, String] = baseSchema.map(a => a.nsFull + "_" + a.attr -> a.tpe).toMap

    testSchema.groupBy(_.nsFull).toSeq.sortBy(_._2.head.pos).flatMap { case (_, attrs) =>

      val (okAttr, attrTypesWithError) = attrs.sortBy(_.pos).foldLeft(
        true,
        Seq.empty[(String, String, String, Boolean, String)]
      ) {
        case ((prevOk, acc), a) =>
          val attrKey       = a.nsFull + "_" + a.attr
          val baseTpe       = baseTypes(attrKey)
          val (ok, comment) = (a.card, baseTpe, a.tpe, a.enums) match {
            case (_, "Long", "Int", _)              => (true, " (Ok, Int saved as Long in Datomic)")
            case (_, "Double", "Float", _)          => (true, " (Ok, Float saved as Double in Datomic)")
            case (_, "ref", "String", Nil)          => (false, "Missing enum values?")
            case (_, "ref", "String", _)            => (true, " (Ok, enum values are saved as refs)")
            case (3 | 4, "String", "Int", _)        => (true, " (Ok, Int map saved as String in Datomic)")
            case (3 | 4, "String", "Long", _)       => (true, " (Ok, Long map saved as String in Datomic)")
            case (3 | 4, "String", "Float", _)      => (true, " (Ok, Float map saved as String in Datomic)")
            case (3 | 4, "String", "Double", _)     => (true, " (Ok, Double map saved as String in Datomic)")
            case (3 | 4, "String", "BigInt", _)     => (true, " (Ok, BigInt map saved as String in Datomic)")
            case (3 | 4, "String", "BigDecimal", _) => (true, " (Ok, BigDecimal map saved as String in Datomic)")
            case (3 | 4, "String", "Byte", _)       => (true, " (Ok, Byte map saved as String in Datomic)")
            case (3 | 4, "String", "Boolean", _)    => (true, " (Ok, Boolean map saved as String in Datomic)")
            case (3 | 4, "String", "Date", _)       => (true, " (Ok, Date map saved as String in Datomic)")
            case (3 | 4, "String", "UUID", _)       => (true, " (Ok, UUID map saved as String in Datomic)")
            case (3 | 4, "String", "URI", _)        => (true, " (Ok, URI map saved as String in Datomic)")
            case (_, l, d, _) if l != d             => (false, "")
            case (_, _, _, _)                       => (true, "")
          }
          (prevOk && ok, acc :+ (a.attr, baseTpe, a.tpe, ok, comment))
      }

      if (okAttr) None else Some(attrs.head.part, attrs.head.ns, attrTypesWithError)
    }
  }


  def attrOptions: Seq[(String, String, Seq[(String, Seq[String], Seq[String], Boolean)])] = {
    val baseOptionsMap: Map[String, Seq[String]] =
      baseSchema.map(a => a.nsFull + "_" + a.attr -> a.options).toMap

    testSchema.groupBy(_.nsFull).toSeq.sortBy(_._2.head.pos).flatMap {
      case (_, attrs) =>
        val (okAttr, attrOptionsWithError) = attrs.sortBy(_.pos).foldLeft(
          true,
          Seq.empty[(String, Seq[String], Seq[String], Boolean)]
        ) {
          case ((prevOk, acc), a) =>
            val attrKey                  = a.nsFull + "_" + a.attr
            val baseOptions: Seq[String] = baseOptionsMap(attrKey).sorted
            val ok                       = baseOptions.map {
              case "identity" => "uniqueIdentity"
              case "value"    => "uniqueValue"
              case opt        => opt
            }.sorted == a.options.sorted

            (prevOk && ok, acc :+ (a.attr, baseOptions, a.options, ok))
        }

        if (okAttr) None else Some(attrs.head.part, attrs.head.ns, attrOptionsWithError)
    }
  }


  def metaAttrRef: Seq[(String, String, Seq[(String, String, String, String, Boolean)])] = {
    val baseRefs: Map[String, Option[String]] =
      baseSchema.map(a => a.nsFull + "_" + a.attr -> a.refNs$).toMap

    testSchema.groupBy(_.nsFull).toSeq.sortBy(_._2.head.pos).flatMap { case (_, attrs) =>

      val (okNs, attrRefsWithError) = attrs.sortBy(_.pos).foldLeft(
        true,
        Seq.empty[(String, String, String, String, Boolean)]
      ) {
        case ((prevOk, acc), a) =>
          val attrKey    = a.nsFull + "_" + a.attr
          val baseRef    = baseRefs(attrKey)
          val ok         = (baseRef, a.refNs$) match {
            case (Some(l), Some(d)) if l != d => false
            case (_, _)                       => true
          }
          val baseRefStr = baseRef match {
            case Some(refNs) => refNs
            case None        => ""
          }
          val testRefStr = a.refNs$ match {
            case Some(refNs) => refNs
            case None        => ""
          }
          (prevOk && ok, acc :+ (a.attr, a.tpe, baseRefStr, testRefStr, ok))
      }

      if (okNs) None else Some(attrs.head.part, attrs.head.ns, attrRefsWithError)
    }
  }

}