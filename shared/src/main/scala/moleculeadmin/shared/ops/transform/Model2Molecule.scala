package moleculeadmin.shared.ops.transform

import java.util.Date
import moleculeadmin.shared.api.QueryApi
import molecule.ast.model
import molecule.ast.model._
import molecule.util.DateHandling
import moleculeadmin.shared.ops.query.BaseQuery


trait Model2Molecule extends QueryApi with BaseQuery with DateHandling {


  def model2molecule(elements: Seq[Element]): String = onClient(true).model2molecule(elements)

  def model2moleculeServer(elements: Seq[Element]): String = onClient(false).model2molecule(elements)


  private case class onClient(client: Boolean) {

    def c(s: String): String = if (s.contains('_')) s else s.capitalize

    def castNoDecimalEncoding(tpe: String, value: Any): String = (tpe, value) match {
      case ("Date", date: Date)           => "\"" + date2str(date) + "\""
      case ("String" | "UUID" | "URI", v) => "\"" + v + "\""
      case (_, v)                         => v.toString
    }

    def cast2(tpe: String, value: Any): String = if (client)
      model.cast2(tpe, value)
    else
      castNoDecimalEncoding(tpe, value)


    private def v(tpe: String, value: Value) = {
      val v2 = value match {
        case Fulltext(vs)                  => s""".contains("${vs.mkString("\", \"")}")"""
        case Fn("count-distinct", None)    => s"(countDistinct)"
        case Fn("count-distinct", Some(n)) => s"(countDistinct($n))"
        case Distinct                      => s"(distinct)"
        case Fn(fn, None)                  => s"($fn)"
        case Fn(fn, Some(1))               => s"($fn)"
        case Fn(fn, Some(n))               => s"($fn($n))"
        case Eq(Nil)                       => "(Nil)"
        case Eq(vs)                        => "(" + vs.map(cast2(tpe, _)).mkString(", ") + ")"
        case Neq(Nil)                      => "(Nil)"
        case Neq(vs)                       => "(" + vs.map(cast2(tpe, _)).mkString(", ") + ")"
        case Gt(v)                         => ".>(" + cast2(tpe, v) + ")"
        case Ge(v)                         => ".>=(" + cast2(tpe, v) + ")"
        case Lt(v)                         => ".<(" + cast2(tpe, v) + ")"
        case Le(v)                         => ".<=(" + cast2(tpe, v) + ")"
        case _                             => ""
      }
      v2
    }

    def model2molecule(elements: Seq[Element]): String = elements.foldLeft(0, "") {
      case ((_, ""), Atom(ns, `dummy`, _, _, _, _, _, _))    => (0, c(ns))
      case ((_, ""), Generic(ns, e@("e" | "e_"), _, value))  => (0, c(ns) + "." + e + v("Long", value))
      case ((_, ""), Atom(ns, attr, tpe, _, value, _, _, _)) => (0, c(ns) + "." + attr + v(tpe, value))
      case ((_, ""), Bond(ns, refAttr, _, _, _))             => (0, c(ns) + "." + c(refAttr))
      case ((_, m), Atom(_, `dummy`, _, _, _, _, _, _))      => (0, m)
      case ((_, m), Generic(_, e@("e" | "e_"), _, value))    => (0, m + "." + e + v("Long", value))
      case ((_, m), Generic(_, attr, _, _))                  => (0, m + "." + attr)
      case ((_, m), Atom(_, attr, tpe, _, value, _, _, _))   => (0, m + "." + attr + v(tpe, value))
      case ((_, m), ReBond(backRef))                         => (1, m + "._" + c(backRef))
      case ((0, m), Bond(_, refAttr, _, _, _))               => (0, m + "." + c(refAttr))
      case ((1, m), Bond(_, refAttr, _, _, _))               => (0, m + "\n." + c(refAttr))
      case ((rebonding, m), e)                               => (rebonding, m + " ### Unexpected element: " + e)
    }._2
  }

}
