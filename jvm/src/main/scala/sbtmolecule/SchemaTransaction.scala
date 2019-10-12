package sbtmolecule

import Ast._

object SchemaTransaction {

  // Generate ..........................................

  def apply(d: Definition): String = {

    def attrStmts(ns: String, a: DefAttr): String = {
      val ident = s"""":db/ident"             , ":${firstLow(ns)}/${a.attrClean}""""
      def tpe(t: String) = s"""":db/valueType"         , ":db.type/$t""""
      def card(c: String) = s"""":db/cardinality"       , ":db.cardinality/$c""""
      val stmts = a match {
        case Val(_, _, clazz, _, _, t, options, _, _, _) if clazz.take(3) == "One" => Seq(tpe(t), card("one")) ++ options.map(_.datomicKeyValue)
        case Val(_, _, _, _, _, t, options, _, _, _)                               => Seq(tpe(t), card("many")) ++ options.map(_.datomicKeyValue)
        case a: DefAttr if a.clazz.take(3) == "One"                                => Seq(tpe("ref"), card("one")) ++ a.options.map(_.datomicKeyValue)
        case a: DefAttr                                                            => Seq(tpe("ref"), card("many")) ++ a.options.map(_.datomicKeyValue)
        case unexpected                                                            => throw new SchemaDefinitionException(s"Unexpected attribute statement:\n" + unexpected)
      }
      s"Util.map(${(ident +: stmts).mkString(",\n             ")})"
    }

    def enums(part: String, ns: String, a: String, es: Seq[String]): String = {
      val partition = if (part.isEmpty) ":db.part/user" else s":$part"
      es.map(e =>
        s"""Util.map(":db/id", Peer.tempid("$partition"), ":db/ident", ":${firstLow(ns)}.$a/$e")""").mkString(",\n    ")
    }

    // Prepare schema for edge interlink meta data if a property edge is defined
    val (partitions: Seq[String], nss: Seq[Namespace]) = {
      val parts = d.nss.map(_.part).filter(_.nonEmpty).distinct
      if (parts.contains("molecule"))
        throw new SchemaDefinitionException("Partition name `molecule` is reserved by Molecule. Please choose another partition name.")
      d.nss.collectFirst {
        case ns if ns.attrs.collectFirst {
          case Ref(_, _, _, _, _, _, _, _, Some("BiTargetRef_"), _, _) => true
        }.getOrElse(false) => {
          val moleculeMetaNs = Namespace("molecule", None, "molecule_Meta", None, None, Seq(
            Ref("otherEdge", "otherEdge", "OneRefAttr", "OneRef", "Long", "", "molecule_Meta", Seq(
              Optional("""":db/index"             , true.asInstanceOf[Object]""", "Indexed"),
              // Is component so that retracts automatically retracts the other edge
              Optional("""":db/isComponent"       , true.asInstanceOf[Object]""", "IsComponent")
            ))))
          (parts :+ "molecule", d.nss :+ moleculeMetaNs)
        }
      }.getOrElse(parts, d.nss)
    }

    val partitionDefinitions: String = {
      val ps = partitions.map { p =>
        s"""|Util.map(":db/ident"             , ":$p",
            |             ":db/id"                , Peer.tempid(":db.part/db"),
            |             ":db.install/_partition", ":db.part/db")""".stripMargin
      }
      if (ps.nonEmpty) {
        s"""|lazy val partitions = Util.list(
            |
            |    ${ps.mkString(",\n\n    ")}
            |  )
            |""".stripMargin
      } else "lazy val partitions = Util.list()\n"
    }

    val attributeDefinitions: String = nss.filterNot(_.attrs.isEmpty).map { ns =>
      val exts = ns.opt.getOrElse("").toString
      val header = "// " + ns.ns + exts + " " + ("-" * (65 - (ns.ns.length + exts.length)))
      val attrs = ns.attrs.flatMap { a =>
        val attr = attrStmts(ns.ns, a)
        a match {
          case e: Enum     => Seq(attr, enums(ns.part, ns.ns, a.attrClean, e.enums))
          case br: BackRef => Nil
          case _           => Seq(attr)
        }
      }
      header + "\n\n    " + attrs.mkString(",\n\n    ")
    }.mkString(",\n\n\n    ")

    s"""|/*
        |* AUTO-GENERATED Molecule DSL schema boilerplate code
        |*
        |* To change:
        |* 1. edit schema definition file in `${d.pkg}.schema/`
        |* 2. `sbt compile` in terminal
        |* 3. Refresh and re-compile project in IDE
        |*/
        |package ${d.pkg}.schema
        |import molecule.schema.SchemaTransaction
        |import datomic.{Util, Peer}
        |
        |object ${d.domain}Schema extends SchemaTransaction {
        |
        |  $partitionDefinitions
        |
        |  lazy val namespaces = Util.list(
        |
        |    $attributeDefinitions
        |  )
        |}""".stripMargin
  }
}
