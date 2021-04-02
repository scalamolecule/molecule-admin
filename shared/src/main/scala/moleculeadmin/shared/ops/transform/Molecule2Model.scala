package moleculeadmin.shared.ops.transform

import java.net.URI
import java.util.{Date, UUID}
import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.metaSchema.{MetaAttr, MetaNs}
import moleculeadmin.shared.ops.query.BaseQuery
import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.metaSchema._
import molecule.ast.model._
import moleculeadmin.shared.ops.query.BaseQuery
import moleculeadmin.shared.util.HelpersAdmin
import scala.collection.mutable.ListBuffer


object Molecule2Model {
  def apply(molecule: String)
           (implicit nsMap: Map[String, MetaNs]): Either[String, Seq[Element]] =
    new Molecule2Model(molecule, nsMap).getModel
}

class Molecule2Model(molecule0: String, nsMap: Map[String, MetaNs])
  extends QueryApi with BaseQuery with HelpersAdmin with Model2Molecule {

  // Schema extractions

  val nssFull = nsMap.keys.toList

  def noTick(s: String): String = s.replace("`", "")

  val attrNames: Map[String, Seq[String]] = nsMap.map {
    case (nsFull, nsDef) => nsFull -> (Seq("e", "e_") ++ nsDef.attrs.tail
      .flatMap(a => Seq(a.name, noTick(a.name) + "$", noTick(a.name) + "_")))
  }

  val attrDefs: Map[String, Map[String, MetaAttr]] = nsMap.map {
    case (nsFull, nsDef) => nsFull ->
      (("e" -> nsDef.attrs.head) +:
        nsDef.attrs.tail
          .flatMap(a => Seq(
            a.name -> a,
            noTick(a.name) + "$" -> a,
            noTick(a.name) + "_" -> a)
          )).toMap
  }

  val refs: Map[String, Seq[String]] = nsMap.map {
    case (nsFull, nsDef) => nsFull -> nsDef.attrs.tail
      .filter(a => a.refNs$.isDefined)
      .map(a => noTick(a.name).capitalize).distinct
  }

  val refDefs: Map[String, Map[String, MetaAttr]] = nsMap.map {
    case (nsFull, nsDef) => nsFull -> nsDef.attrs.tail
      .filter(a => a.refNs$.isDefined)
      .map(a => noTick(a.name).capitalize -> a)
      .toMap
  }

  val refNss: Map[String, Seq[String]] = nsMap.map {
    case (nsFull, nsDef) => nsFull -> nsDef.attrs.tail
      .filter(a => a.refNs$.isDefined)
      .map(a => a.refNs$.get)
      .distinct
  }

  val backRefNss: Map[String, Seq[String]] = nsMap.keys.map { nsFull =>
    nsFull -> refNss.filter {
      case (_, refNss1) => refNss1.contains(nsFull)
    }.map("_" + _._1).toList
  }.toMap

  val backRefs: Set[String] = backRefNss.values.flatten.toSet

  // Building vars
  var first         = true
  var expectElement = true
  var prevAttr      = ""
  var prevNsFull    = ""
  var curNsFull     = ""
  var eidNsFull     = ""
  var molecule      = molecule0.replace("\n", "").trim
  val elements      = new ListBuffer[Element]

  def debug(token: String = "not suplied...") = {
    println("=====================================")
    println(s"prevNs   : `$prevNsFull`")
    println(s"curNsFull: `$curNsFull`")
    println(s"token    : `$token`")
    println("attrNames : " + attrNames)
    println("refs      : " + refs)
    println("refNss    : " + refNss)
    println("backRefNss: " + backRefNss)
    println("backRefs  : " + backRefs)
    //    println("refDefs --------------")
    //    refDefs.foreach { case (ns1, attrs1) =>
    //      println(ns1)
    //      attrs1 foreach println
    //    }
  }

  //  debug("1")


  def getModel: Either[String, Seq[Element]] = {
    try {
      if (molecule.isEmpty)
        throw new IllegalArgumentException("Can't get model from empty molecule")

      // Recursively resolve tokens from start to end
      resolve()
      Right(elements.toList)
    } catch {
      case t: Throwable =>
        t.getStackTrace foreach println
        Left(t.getMessage)
    }
  }

  def resolve(): Unit = molecule.headOption match {
    case None                         => // done
    case Some('_')                    =>
      resolveBackRef()
    case Some(c) if curNsFull.isEmpty =>
      resolveFirstNs()
    case Some(c) if c.isUpper         =>
      resolveRef()
    case _                            =>
      resolveAttr()
  }


  def resolveFirstNs(): Unit = {
    val firstNs = extractCleanToken
    // First ns
    if (nssFull.contains(firstNs)) {
      prevNsFull = firstNs
      curNsFull = firstNs
    } else {
      throw new IllegalArgumentException(
        s"Unrecognized initial namespace name `$firstNs` in molecule: $molecule0")
    }
    resolve()
  }

  def resolveRef(): Unit = {
    val token = extractCleanToken
    if (refs(curNsFull).contains(token)) {
      // allowed next ref ns
      val ref = refDefs(curNsFull)(token)
      elements += Bond(curNsFull, token.low, ref.refNs$.get, ref.card)
      curNsFull = ref.refNs$.get
    } else {
      throw new IllegalArgumentException(
        s"Unrecognized ref namespace `$token` from namespace `$curNsFull` " +
          s"in molecule: $molecule0")
    }
    resolve()
  }

  def resolveBackRef(): Unit = {
    val token = extractCleanToken
    if (backRefs.contains(token)) {
      val backRefNs = token.tail
      if (backRefNss(curNsFull).contains(token)) {
        elements += ReBond(backRefNs)
        curNsFull = backRefNs
        prevAttr = "" // reset when rebonding
      } else {
        // un-allowed backRef
        throw new IllegalArgumentException(
          s"Namespace `$curNsFull` can't reference directly back to namespace " +
            s"`$backRefNs` in molecule: $molecule0")
      }
    } else {
      throw new IllegalArgumentException(
        s"Unrecognized back ref namespace `$token` from namespace `$curNsFull` " +
          s"in molecule: $molecule0")
    }
    resolve()
  }

  def checkReserved(attr: String) = if (reservedAttrNames2.contains(attr)) {
    throw new IllegalArgumentException(
      s"Can't use reserved generic attribute name `$attr` in molecule: $molecule0")
  }

  def isFn(fn: String): Boolean = comparisonFns.contains(fn)

  def resolveAttr(): Unit = {
    // (accepting last character to be a punctuation)
    molecule = (molecule.trim match {
      case r"(`?[a-z]\w*`?[$$_]?)$attr *\. *([\w><=!]+)$fn *\((.*?)$expr\) *\. *(.+)$tail\.?" if isFn(fn) => resolveAttrFnExpr(attr, fn, expr); tail
      case r"(`?[a-z]\w*`?[$$_]?)$attr *\. *([\w><=!]+)$fn *\((.*)$expr\)\.?" if isFn(fn)                 => resolveAttrFnExpr(attr, fn, expr); ""
      case r"(`?[a-z]\w*`?[$$_]?)$attr +([\w><=!]+)$fn +(.*)$expr\.?" if isFn(fn)                         => resolveAttrFnExpr(attr, fn, expr); "" // postfix, last expr
      case r"(`?[a-z]\w*`?[$$_]?)$attr *\((.*?)$expr\) *\. *(.+)$tail\.?"                                 => resolveAttrFnExpr(attr, "apply", expr); tail
      case r"(`?[a-z]\w*`?[$$_]?)$attr *\((.*)$expr\)\.?"                                                 => resolveAttrFnExpr(attr, "apply", expr); ""
      case r"(`?[a-z]\w*`?[$$_]?)$attr *\. *(.+)$tail\.?"                                                 => resolveCleanAttr(attr); tail
      case r"(`?[a-z]\w*`?[$$_]?)$attr\.?"                                                                => resolveCleanAttr(attr); ""
      case other                                                                                          =>
        throw new IllegalArgumentException(s"Unrecognized pattern `$other` in molecule: $molecule0")
    }).trim
    resolve()
  }

  def addGenericTx(attr: String): Unit = {
    val (ns, prevAttr1) = (curNsFull, prevAttr)
    val updatedElements = elements.foldLeft(0: Int, List.empty[Element]) {
      case ((0, acc), a@Atom(`ns`, `prevAttr1`, _, _, _, _, _, _)) =>
        (1, acc :+ a.copy(keys = a.keys :+ attr))
      case ((done, acc), e)                                        =>
        (done, acc :+ e)
    }._2
    elements.clear()
    elements ++= updatedElements :+ Generic(curNsFull, attr, "datom", NoValue)
  }

  def addEdit(attr: String, tpe: String, card: Int): Unit = {
    val aggrs           = Seq("count", "count-distinct", "sum", "avg", "median", "variance", "stddev")
    val (ns, prevAttr1) = (curNsFull, prevAttr)
    val updatedElements = elements.foldLeft(0: Int, List.empty[Element]) {
      case ((0, _), Atom(`ns`, `prevAttr1`, _, _, Fn(fn, _), _, _, _))
        if aggrs.contains(fn) =>
        throw new IllegalArgumentException(
          s"Expecting aggregate function after clean attribute:" +
            s"\nFound    : `$attr($fn).$attr`" +
            s"\nExpecting: `$attr.$attr($fn)`"
        )

      case ((0, acc), a@Atom(`ns`, `prevAttr1`, _, _, value, _, _, _)) =>
        (1, acc :+ a.copy(keys = a.keys :+ "orig", value = value))
      case ((done, acc), e)                                            =>
        (done, acc :+ e)
    }._2
    elements.clear()
    elements ++= updatedElements :+
      Atom(ns, attr, tpe, card, VarValue, None, Seq(), Seq("edit"))
  }

  def resolveCleanAttr(attr: String): Unit = {
    checkReserved(attr)
    if (Seq("t", "tx", "txInstant").contains(attr)) {
      addGenericTx(attr)
    } else if (attrNames(curNsFull).contains(attr)) {
      attr match {
        case "e" =>
          eidNsFull = curNsFull
          elements += Generic(curNsFull, "e", "datom", EntValue)

        case "e_" => throw new IllegalArgumentException(
          s"`e_` can only be tacit if an id is applied to it (`e_(12345L)`) in " +
            s"molecule: $molecule0")

        case _ =>
          val a = attrDefs(curNsFull)(attr)
          if (eidNsFull == curNsFull && // only group edit if eid is present in ns
            prevNsFull == curNsFull && // same ns
            prevAttr == attr // same attr name
          ) {
            addEdit(attr, a.tpe, a.card)
          } else {
            val (value, enumPrefix) = if (a.enums.nonEmpty)
              (EnumVal, Some(s":$curNsFull.$attr/")) else (VarValue, None)
            elements += Atom(curNsFull, attr, a.tpe, a.card, value, enumPrefix)
          }
      }
      prevNsFull = curNsFull
      prevAttr = attr
    } else {
      throw new IllegalArgumentException(s"Unrecognized attribute name `$attr` " +
        s"in molecule: $molecule0")
    }
  }

  def resolveAttrFnExpr(attr: String, fn: String, expr: String): Unit = {
    checkReserved(attr)
    //    debug(s"$attr-$fn-$expr")
    if (attrNames(curNsFull).contains(attr)) {
      attr match {
        case "e" | "e_" => fn match {
          case "apply" => expr.trim match {
            case r"([0-9, L]*)$numberData" =>
              val numbers = numberData
                .split(",").toSeq
                .map(_.trim.replace("L", "").toLong)
              elements += Generic(curNsFull, attr, "datom", Eq(numbers))
            case "count"                   =>
              elements += Generic(curNsFull, attr, "datom", Fn("count"))

            case _ => throw new IllegalArgumentException(
              s"Unrecognized expression value `$expr` for entity id attribute " +
                s"`$attr` in molecule: $molecule0")
          }
          case _       => throw new IllegalArgumentException(
            s"Un-allowed expression function `$fn` for entity id attribute " +
              s"`$attr` in molecule: $molecule0")
        }

        case _ =>
          val a                            = attrDefs(curNsFull)(attr)
          val (tpe, card, enumPrefix, opt) = (
            a.tpe,
            a.card,
            if (a.enums.nonEmpty) Some(s":$curNsFull.$attr/") else None,
            attr.last == '$'
          )
          fn match {
            case "apply" if opt => throw new IllegalArgumentException(
              s"Can't apply value to optional attribute `$attr` in molecule: $molecule0")
            case "apply"        => resolveApply(attr, tpe, card, expr, enumPrefix)
            case ">"            => resolveComparison(attr, tpe, card, expr, vs => Gt(vs.head), enumPrefix)
            case ">="           => resolveComparison(attr, tpe, card, expr, vs => Ge(vs.head), enumPrefix)
            case "<"            => resolveComparison(attr, tpe, card, expr, vs => Lt(vs.head), enumPrefix)
            case "<="           => resolveComparison(attr, tpe, card, expr, vs => Le(vs.head), enumPrefix)
            case "!=" | "not"   => resolveComparison(attr, tpe, card, expr, vs => Neq(vs), enumPrefix)
            case "contains"     => resolveComparison(attr, tpe, card, expr, vs => Fulltext(vs), enumPrefix, true)
            case _              =>
              // Function names have already been verified, so we should never get here (unless fn name list is wrong)
              throw new IllegalArgumentException(
                s"Unrecognized expression function `$fn` for attribute `$attr` in molecule: $molecule0")
          }
      }
      prevNsFull = curNsFull
      prevAttr = attr
    } else {
      throw new IllegalArgumentException(s"Unrecognized attribute name `$attr` (having expression) in molecule: $molecule0")
    }
  }

  def resolveComparison(
    attr: String,
    tpe: String,
    card: Int,
    expr: String,
    fn: Seq[Any] => Value,
    enumPrefix: Option[String],
    fulltext: Boolean = false
  ): Unit = {
    expr.trim match {
      case "" => throw new IllegalArgumentException(
        s"Comparing an empty value to attribute `$attr` not allowed in molecule: $molecule0")
      case _  => tpe match {
        case "Int"        => elements += Atom(curNsFull, attr, tpe, card, fn(intValues(attr, expr)), enumPrefix)
        case "Long"       => elements += Atom(curNsFull, attr, tpe, card, fn(longValues(attr, expr)), enumPrefix)
        case "ref"        => elements += Atom(curNsFull, attr, tpe, card, fn(longValues(attr, expr)), enumPrefix)
        case "Float"      => elements += Atom(curNsFull, attr, tpe, card, fn(floatValues(attr, expr)), enumPrefix)
        case "Double"     => elements += Atom(curNsFull, attr, tpe, card, fn(doubleValues(attr, expr)), enumPrefix)
        case "String"     => elements += Atom(curNsFull, attr, tpe, card, fn(stringValues(attr, expr, fulltext)), enumPrefix)
        case "Boolean"    => elements += Atom(curNsFull, attr, tpe, card, fn(booleanValues(attr, expr)), enumPrefix)
        case "Date"       => elements += Atom(curNsFull, attr, tpe, card, fn(dateValues(attr, expr)), enumPrefix)
        case "UUID"       => elements += Atom(curNsFull, attr, tpe, card, fn(uuidValues(attr, expr)), enumPrefix)
        case "URI"        => elements += Atom(curNsFull, attr, tpe, card, fn(uriValues(attr, expr)), enumPrefix)
        case "BigInt"     => elements += Atom(curNsFull, attr, tpe, card, fn(bigIntValues(attr, expr)), enumPrefix)
        case "BigDecimal" => elements += Atom(curNsFull, attr, tpe, card, fn(bigDecimal(attr, expr)), enumPrefix)
      }
    }
  }

  def aggr(attr: String, fn: String, tpe: String, card: Int): Unit = {
    val (ns, prevAttr1) = (curNsFull, prevAttr)
    val updatedElements = elements.foldLeft(0: Int, List.empty[Element]) {
      case ((0, acc), a@Atom(`ns`, `prevAttr1`, _, _, _, _, _, _)) => (1, acc :+ a.copy(keys = a.keys :+ fn))
      case ((done, acc), e)                                        => (done, acc :+ e)
    }._2
    elements.clear()
    elements ++= updatedElements :+ Atom(ns, attr, tpe, card, Fn(fn, None))
  }

  def resolveApply(attr: String, tpe: String, card: Int, expr: String, enumPrefix: Option[String]): Unit = {
    expr.trim match {
      case "None" | "Nil" =>
        elements += Atom(curNsFull, attr, tpe, card, Fn("not"), enumPrefix)

      case "countDistinct" =>
        aggr(attr, "count-distinct", tpe, card)

      case fn if aggrFns.contains(fn) =>
        aggr(attr, fn, tpe, card)

      case r"(min|max|rand|sample)$fn\(([1-9][0-9]*)$n\)" =>
        elements += Atom(curNsFull, attr, tpe, card, Fn(fn, Some(n.toInt)), enumPrefix)

      case "" =>
        throw new IllegalArgumentException(
          s"Applying an empty value to attribute `$attr` not allowed in molecule: $molecule0")

      case _ => tpe match {
        case "Int"        => elements += Atom(curNsFull, attr, tpe, card, Eq(intValues(attr, expr)), enumPrefix)
        case "Long"       => elements += Atom(curNsFull, attr, tpe, card, Eq(longValues(attr, expr)), enumPrefix)
        case "ref"        => elements += Atom(curNsFull, attr, tpe, card, Eq(longValues(attr, expr)), enumPrefix)
        case "Float"      => elements += Atom(curNsFull, attr, tpe, card, Eq(floatValues(attr, expr)), enumPrefix)
        case "Double"     => elements += Atom(curNsFull, attr, tpe, card, Eq(doubleValues(attr, expr)), enumPrefix)
        case "String"     => elements += Atom(curNsFull, attr, tpe, card, Eq(stringValues(attr, expr)), enumPrefix)
        case "Boolean"    => elements += Atom(curNsFull, attr, tpe, card, Eq(booleanValues(attr, expr)), enumPrefix)
        case "Date"       => elements += Atom(curNsFull, attr, tpe, card, Eq(dateValues(attr, expr)), enumPrefix)
        case "UUID"       => elements += Atom(curNsFull, attr, tpe, card, Eq(uuidValues(attr, expr)), enumPrefix)
        case "URI"        => elements += Atom(curNsFull, attr, tpe, card, Eq(uriValues(attr, expr)), enumPrefix)
        case "BigInt"     => elements += Atom(curNsFull, attr, tpe, card, Eq(bigIntValues(attr, expr)), enumPrefix)
        case "BigDecimal" => elements += Atom(curNsFull, attr, tpe, card, Eq(bigDecimal(attr, expr)), enumPrefix)
      }
    }
  }

  def or(s: String): Seq[String] = {
    val decoded: String = s.replaceAll(" +or +", ", ")
    // double check for any left over "o" or "r"
    if (decoded.contains('o') || decoded.contains('r')) {
      throw new IllegalArgumentException(
        s"Found unexpected 'o' and/or 'r' in expression in molecule: $molecule0")
    } else {
      decoded.split(",").toSeq
    }
  }

  def intValues(attr: String, expr: String): Seq[Int] = {
    def traverse(acc: Seq[Int], s: String): Seq[Int] = s.trim match {
      case r"(Seq|List)$x\(([0-9, or]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([0-9, or]+)$vs\)"                => traverse(acc, vs)
      case r"([0-9, or]+)$vs"                                => acc ++ or(vs).map(_.trim.toInt)
      case _                                                 => throw new IllegalArgumentException(
        s"Unrecognized Int expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def longValues(attr: String, expr: String): Seq[Long] = {
    def trim(n: String): Long = n match {
      case _ if n.last == 'L' => n.trim.init.toLong
      case _                  => n.trim.toLong
    }
    def traverse(acc: Seq[Long], s: String): Seq[Long] = s.trim match {
      case r"(Seq|List)$x\(([0-9\., orL]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([0-9\., orL]+)$vs\)"                => traverse(acc, vs)
      case r"([0-9\., orL]+)$vs"                                => acc ++ or(vs).map(trim)
      case _                                                    => throw new IllegalArgumentException(
        s"Unrecognized Long expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def floatValues(attr: String, expr: String): Seq[Float] = {
    def traverse(acc: Seq[Float], s: String): Seq[Float] = s.trim match {
      case r"(Seq|List)$x\(([0-9\., f]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([0-9\., f]+)$vs\)"                => traverse(acc, vs)
      case r"([0-9\., orf]+)$vs"                              => acc ++ or(vs).map(_.trim.toFloat)
      case _                                                  => throw new IllegalArgumentException(
        s"Unrecognized Float expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def doubleValues(attr: String, expr: String): Seq[Double] = {
    def traverse(acc: Seq[Double], s: String): Seq[Double] = s.trim match {
      case r"(Seq|List)$x\(([0-9\., or]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([0-9\., or]+)$vs\)"                => traverse(acc, vs)
      case r"([0-9\., or]+)$vs"                                => acc ++ or(vs).map(_.trim.toDouble)
      case _                                                   => throw new IllegalArgumentException(
        s"Unrecognized Double expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def stringValues(attr: String, expr: String, fulltext: Boolean = false): Seq[String] = {
    def ok(s: String): String = if (fulltext && datomicFulltextNonIndexed.contains(s)) throw new IllegalArgumentException(
      s"Can't use non-indexed standard word `$s` for fulltext search for attribute `$attr` in molecule: $molecule0") else s
    def traverse(acc: Seq[String], s: String): Seq[String] = {
      s match {
        case r"(Seq|List)$a\(([^\)]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
        case r"(Seq|List)$a\(([^\)]+)$vs\)"                => traverse(acc, vs)
        case r""""([^"]+)$v" *(or|,)$x *(.+)$tail"""       => traverse(acc :+ ok(v), tail)
        case r""""([^"]+)$v""""                            => acc :+ ok(v)
        case _                                             => throw new IllegalArgumentException(
          s"Unrecognized String pattern `$s` in expression `$expr` for attribute `$attr` in molecule: $molecule0")
      }
    }
    traverse(Nil, expr)
  }

  def booleanValues(attr: String, expr: String): Seq[Boolean] = {
    def traverse(acc: Seq[Boolean], s: String): Seq[Boolean] = s.trim match {
      case r"(Seq|List)$a\((true|false)$v *,? *(.+)$tail1\) *,? *(.+)$tail2" => traverse(traverse(acc :+ v.trim.toBoolean, tail1), tail2)
      case r"(Seq|List)$a\((true|false)$v\) *,? *(.+)$tail2"                 => traverse(acc :+ v.trim.toBoolean, tail2)
      case r"(Seq|List)$a\((true|false)$v *,? *(.+)$tail1\)"                 => traverse(acc :+ v.trim.toBoolean, tail1)
      case r"(Seq|List)$a\((true|false)$v\)"                                 => acc :+ v.trim.toBoolean
      case r"(true|false)$v *(or|,)$x *(.+)$tail"                            => traverse(acc :+ v.trim.toBoolean, tail)
      case r"(true|false)$v"                                                 => acc :+ v.trim.toBoolean
      case _                                                                 => throw new IllegalArgumentException(
        s"Unrecognized Boolean expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def dateValues(attr: String, expr: String): Seq[Date] = {
    def traverse(acc: Seq[Date], s: String): Seq[Date] = s.trim match {
      case r"(Seq|List)$x\(([^\)]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([^\)]+)$vs\)"                => traverse(acc, vs)
      case r""""([^"]+)$v" *(or|,)$x *(.+)$tail"""       => traverse(acc :+ str2date(v.trim), tail)
      case r""""([^"]+)$v""""                            => acc :+ str2date(v.trim)
      case _                                             => throw new IllegalArgumentException(
        s"Unrecognized Date expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def uuidValues(attr: String, expr: String): Seq[UUID] = {
    def traverse(acc: Seq[UUID], s: String): Seq[UUID] = s.trim match {
      case r"(Seq|List)$a\(([^\)]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$a\(([^\)]+)$vs\)"                => traverse(acc, vs)
      case r""""([^"]+)$v" *(or|,)$x *(.+)$tail"""       => traverse(acc :+ UUID.fromString(v.trim), tail)
      case r""""([^"]+)$v""""                            => acc :+ UUID.fromString(v.trim)
      case _                                             => throw new IllegalArgumentException(
        s"Unrecognized UUID expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def uriValues(attr: String, expr: String): Seq[URI] = {
    def traverse(acc: Seq[URI], s: String): Seq[URI] = s.trim match {
      case r"(Seq|List)$x\(([^\)]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([^\)]+)$vs\)"                => traverse(acc, vs)
      case r""""([^"]+)$v" *(or|,)$x *(.+)$tail"""       => traverse(acc :+ new URI(v), tail)
      case r""""([^"]+)$v""""                            => acc :+ new URI(v.trim)
      case _                                             => throw new IllegalArgumentException(
        s"Unrecognized URI expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def bigIntValues(attr: String, expr: String): Seq[BigInt] = {
    def traverse(acc: Seq[BigInt], s: String): Seq[BigInt] = s.trim match {
      case r"(Seq|List)$x\(([0-9, ]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([0-9, ]+)$vs\)"                => traverse(acc, vs)
      case r"([0-9, or]+)$vs"                              => acc ++ or(vs).map(v => BigInt(v.trim))
      case _                                               => throw new IllegalArgumentException(
        s"Unrecognized BigInt expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  def bigDecimal(attr: String, expr: String): Seq[BigDecimal] = {
    def traverse(acc: Seq[BigDecimal], s: String): Seq[BigDecimal] = s.trim match {
      case r"(Seq|List)$x\(([0-9\., ]+)$vs\) *,? *(.+)$tail" => traverse(traverse(acc, vs), tail)
      case r"(Seq|List)$x\(([0-9\., ]+)$vs\)"                => traverse(acc, vs)
      case r"([0-9\., or]+)$vs"                              => acc ++ or(vs).map(v => BigDecimal(v.trim))
      case _                                                 => throw new IllegalArgumentException(
        s"Unrecognized BigDecimal expression value `$expr` for attribute `$attr` in molecule: $molecule0")
    }
    traverse(Nil, expr)
  }

  // Helper methods -----------------------------------------------------------

  def extractCleanToken: String = {
    val (token, rest) = molecule.trim match {
      case r"([^\.]+)$token\.(.*)$rest\.?" => (token, rest)
      case r"([^\.]+)$token\.?"            => (token, "")
      case r"\.*"                          => throw new IllegalArgumentException(s"Can't extract token from empty molecule")
    }
    molecule = rest.trim
    token.trim
  }

  def cast(tpe: String, value: String) = tpe match {
    case "Int" => value.toInt
    case _     => value
  }
}
