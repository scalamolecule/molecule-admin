package moleculeadmin.shared.ops.query.attr

import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.lib.molecule.ast.model._
import moleculeadmin.shared.ops.query.{Base, DebugBranches}


trait AttrOps extends DebugBranches {


  def upsertAttr(model: Seq[Element],
                 path0: Seq[(String, String)],
                 attr: String,
                 attrType: String,
                 car: Int,
                 enums: Option[Set[String]],
                 operand: String,
                 input: String
                ): Either[String, Seq[Element]] = {
    def castedValue = if (input == "") null else attrType match {
      case "Int"                    => input.toInt
      case "Long" | "ref" | "datom" => input.toLong

      // Hacks for scalajs number handling
      case "Float" | "Double" if input.contains('.') => "__n__" + input

      case "BigInt"     => BigInt(input)
      case "BigDecimal" => BigDecimal(input)
      case "Boolean"    => input.toBoolean
      case "Date"       => new java.util.Date(input)
      case "UUID"       => java.util.UUID.fromString(input)
      case "URI"        => new java.net.URI(input)
      case _            => input
    }

    def castedValues: Seq[Any] = if (input == "") Nil else (attrType match {
      case "String"                 => input.split("\\|\\|")
      case "Int"                    => input.replaceAll(" ", "").split(',').map(_.toInt)
      case "Long" | "ref" | "datom" => input.replaceAll(" ", "").split(',').map(_.toLong)
      case "Float" | "Double"       => input.replaceAll(" ", "").split(',').map(n => "__n__" + n)
      case "BigInt"                 => input.replaceAll(" ", "").split(',').map(n => BigInt(n))
      case "BigDecimal"             => input.replaceAll(" ", "").split(',').map(n => BigDecimal(n))
      case "Boolean"                => input.replaceAll(" ", "").split(',').map(_.toBoolean)
      case "Date"                   => input.split(',').map(d => new java.util.Date(d))
      case "UUID"                   => input.replaceAll(" ", "").split(',').map(u => java.util.UUID.fromString(u))
      case "URI"                    => input.replaceAll(" ", "").split(',').map(u => new java.net.URI(u))
      case _                        => input.replaceAll(" ", "").split(',')
    }).toList

    val comparedValue = operand match {
      case "Word =" if input.trim.isEmpty    => Left("Please use `=` operand to match an empty string.")
      case "Word =" if castedValues.nonEmpty => Right(Fulltext(castedValues))
      case "=" if castedValues.nonEmpty      => Right(Eq(castedValues))
      case "!="                              => Right(Neq(castedValues))
      case "<"                               => Right(Lt(castedValue))
      case ">"                               => Right(Gt(castedValue))
      case "<="                              => Right(Le(castedValue))
      case ">="                              => Right(Ge(castedValue))
      case "min" if input.nonEmpty           => Right(Fn("min", Some(input.toInt)))
      case "min"                             => Right(Fn("min", None))
      case "max" if input.nonEmpty           => Right(Fn("max", Some(input.toInt)))
      case "max"                             => Right(Fn("max", None))
      case "rand" if input.nonEmpty          => Right(Fn("rand", Some(input.toInt)))
      case "rand"                            => Right(Fn("rand", None))
      case "sample" if input.nonEmpty        => Right(Fn("sample", Some(input.toInt)))
      case "sample"                          => Right(Fn("sample", Some(1)))
      case "distinct"                        => Right(Distinct)
      case "count"                           => Right(Fn("count", None))
      case "count-distinct"                  => Right(Fn("count-distinct", None))
      case "sum"                             => Right(Fn("sum", None))
      case "avg"                             => Right(Fn("avg", None))
      case "median"                          => Right(Fn("median", None))
      case "variance"                        => Right(Fn("variance", None))
      case "stddev"                          => Right(Fn("stddev", None))
      case "t"                               => Right(Fn("t", None))
      case "tx"                              => Right(Fn("tx", None))
      case "txInstant"                       => Right(Fn("txInstant", None))
      case _ if attr == "e" || attr == "e_"  => Right(EntValue)
      case _                                 => Right(VarValue)
    }

    def debug = {
      println("--- upsertAttr ---")
      println("model    :" + model)
      println("path     :" + path0)
      println("attr     :" + attr + " - " + attrType + " - " + car + " - " + enums)
      println("op/values:" + operand + " - " + input + " - " + comparedValue)
    }
    // debug

    comparedValue match {
      case Right(v)  => Right(upsertBranch(model, path0, attr, attrType, car, enums, v))
      case Left(msg) => Left(msg)
    }
  }


  def upsertBranch(model: Seq[Element],
                   path0: Seq[(String, String)],
                   attr: String,
                   attrType: String,
                   car: Int,
                   enums: Option[Set[String]],
                   v: Value
                  ): Seq[Element] = {
    val ns                       = path0.last._2
    val attrClean                = clean(attr)
    val enumPrefix               = if (enums.isDefined) Some(s":$ns.$attrClean/") else None
    val (before, branch0, after) = isolateBranch(model, path0)
    val (prev, cur, sub)         = isolateAttr(branch0, ns, attr)

    //    println("")
    //    println(s":$ns/$attr   " + v)
    //    println("PRE: " + prev)
    //    println("CUR: " + cur)
    //    println("SUB: " + sub)

    val additives = Seq(
      "orig",
      "count", "count-distinct", "sum", "avg", "median", "variance", "stddev",
      "t", "tx", "txInstant"
    )

    val cur2: Seq[Element] = if (cur.isEmpty) {
      attr match {
        case "e" => v match {
          case Eq(vs) if vs.isEmpty => Seq(Generic(ns, "e", "datom", EntValue))
          case _                    => Seq(Generic(ns, "e", "datom", v))
        }
        case _   => v match {
          case Fn(fn@("count" | "count-distinct" | "sum" | "avg" | "median" | "variance" | "stddev"), _) =>
            Seq(
              // Base tacit attribute
              Atom(ns, attr + "_", attrType, car, VarValue, enumPrefix, Nil, Seq(fn)),
              // Additional attribute
              Atom(ns, attr, attrType, car, v)
            )
          case Fn(fn@("t" | "tx" | "txInstant"), _)                                                      =>
            Seq(
              // Base mandatory attribute
              Atom(ns, attr, attrType, car, VarValue, enumPrefix, Nil, Seq(fn)),
              // Additional tx attribute
              Generic(ns, fn, "datom", NoValue)
            )

          case Eq(vs) if vs.isEmpty => Seq(Atom(ns, attr, attrType, car, VarValue, enumPrefix))
          case _                    => Seq(Atom(ns, attr, attrType, car, v, enumPrefix))
        }
      }
    } else {
      cur.head match {
        case g@Generic(_, "e", _, Fn("count", _))          => v match {
          case Fn("count", _)       => g.copy(value = EntValue) +: cur.tail
          case Eq(vs) if vs.isEmpty => g.copy(value = EntValue) +: cur.tail
          case _                    => g.copy(value = v) +: cur.tail
        }
        case g@Generic(_, "e" | "e_", _, _)                => v match {
          case Eq(vs) if vs.isEmpty => g.copy(value = EntValue) +: cur.tail
          case _                    => g.copy(value = v) +: cur.tail
        }
        case a@Atom(_, _, _, _, oldFn, _, _, curAdditives) => v match {
          case Fn(fn, _) if additives.contains(fn) => {
            val toggleAdditive            = v match {
              case Fn(fn, _) => fn
              case other     => throw new RuntimeException("Unexpected Value: " + other)
            }
            val newAdditives: Seq[String] = if (curAdditives.contains(toggleAdditive))
              additives.intersect(curAdditives.filterNot(_ == toggleAdditive))
            else
              additives.intersect(curAdditives :+ toggleAdditive)

            val newAdditiveAttrs: Seq[Element] = newAdditives.map {
              case "orig"        => Atom(ns, attr, attrType, car, VarValue, None, Nil, Seq("edit"))
              case "count"          => Atom(ns, attr, attrType, car, Fn("count", None))
              case "count-distinct" => Atom(ns, attr, attrType, car, Fn("count-distinct", None))
              case "sum"            => Atom(ns, attr, attrType, car, Fn("sum", None))
              case "avg"            => Atom(ns, attr, attrType, car, Fn("avg", None))
              case "median"         => Atom(ns, attr, attrType, car, Fn("median", None))
              case "variance"       => Atom(ns, attr, attrType, car, Fn("variance", None))
              case "stddev"         => Atom(ns, attr, attrType, car, Fn("stddev", None))
              case "t"              => Generic(ns, "t", "datom", NoValue)
              case "tx"             => Generic(ns, "tx", "datom", NoValue)
              case "txInstant"      => Generic(ns, "txInstant", "datom", NoValue)
            }
            a.copy(keys = newAdditives) +: newAdditiveAttrs
          }

          case Eq(vs) if vs.isEmpty => oldFn match {
            case Eq(_) => a.copy(value = VarValue) +: cur.tail
            case _     => cur
          }

          // Toggle
          case Fn(fn, None) => oldFn match {
            case Fn(`fn`, _) => a.copy(value = VarValue) +: cur.tail
            case _           => a.copy(value = v) +: cur.tail
          }

          case _ => a.copy(value = v) +: cur.tail
        }

        case other => throw new RuntimeException("Unexpected current head element: " + other)
      }
    }

    val result = prev ++ cur2 ++ sub
    //    println("RES: List(" + result.mkString("\n          ") + ")")
    before ++ result ++ after
  }


  def marked(fn: String, attrValue: Value): String = {
    val m = " marked"
    (fn, attrValue) match {
      case ("distinct", Distinct)                                               => m
      case ("op", Fulltext(_) | Eq(_) | Neq(_) | Gt(_) | Lt(_) | Ge(_) | Le(_)) => m
      case (`fn`, Fn(`fn`, _))                                                  => m
      case _                                                                    => ""
    }
  }

  def validate(op: String, attrType: String, input: String): Option[String] = (op, attrType) match {
    case ("Word =", "String")            =>
      if (input.contains("||") || input.contains(",")) {
        Some("OR expressions for fulltext searches not implemented.")
      } else if (Seq("a", "an", "and", "are", "as", "at", "be", "but", "by",
        "for", "if", "in", "into", "is", "it",
        "no", "not", "of", "on", "or", "such",
        "that", "the", "their", "then", "there", "these",
        "they", "this", "to", "was", "will", "with").contains(input.trim)) {
        Some(
          """Fulltext search is constrained by several defaults
            |(which cannot be altered): searches are case insensitive,
            |remove apostrophe or apostrophe and s sequences, and
            |filter out the following common English stop words:
            |
            |"a", "an", "and", "are", "as", "at", "be", "but", "by",
            |"for", "if", "in", "into", "is", "it",
            |"no", "not", "of", "on", "or", "such",
            |"that", "the", "their", "then", "there", "these",
            |"they", "this", "to", "was", "will", "with"""".stripMargin)
      } else None
    case ("=", "String")                 => if (input.contains("|||")) Some("Please use `||` to separate OR-seeds to search for") else None
    case ("=", "Int")                    => if (input.replaceAll(" ", "").split(',').forall(n => n.nonEmpty && n.isInt)) None else Some("Integer expected (comma-separate integers to apply OR-semantics)")
    case ("=", "Long" | "ref" | "datom") => if (input.replaceAll(" ", "").split(',').forall(n => n.nonEmpty && n.isLong)) None else Some("Long expected (comma-separate longs to apply OR-semantics)")
    case ("=", "Float")                  => if (input.replaceAll(" ", "").split(',').forall(n => n.nonEmpty && n.isFloat)) None else Some("Float expected (comma-separate floats to apply OR-semantics)")
    case ("=", "Double")                 => if (input.replaceAll(" ", "").split(',').forall(n => n.nonEmpty && n.isDouble)) None else Some("Double expected (comma-separate doubles to apply OR-semantics)")
    case ("=", "BigInt")                 => if (input.replaceAll(" ", "").split(',').forall(n => n.nonEmpty && n.isBigInt)) None else Some("BigInt expected (comma-separate bigints to apply OR-semantics)")
    case ("=", "BigDecimal")             => if (input.replaceAll(" ", "").split(',').forall(n => n.nonEmpty && n.isBigDecimal)) None else Some("BigDecimal expected (comma-separate bigdecimals to apply OR-semantics)")
    case (_, "Int")                      => if (input.trim.isInt) None else Some("Integer expected")
    case (_, "Long" | "ref" | "datom")   => if (input.trim.isLong) None else Some("Long expected")
    case (_, "Float")                    => if (input.trim.isFloat) None else Some("Float expected")
    case (_, "Double")                   => if (input.trim.isDouble) None else Some("Double expected")
    case (_, "BigInt")                   => if (input.trim.isBigInt) None else Some("BigInt expected")
    case (_, "BigDecimal")               => if (input.trim.isBigDecimal) None else Some("BigDecimal expected")
    case _                               => None
  }
}
