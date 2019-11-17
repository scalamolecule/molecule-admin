package moleculeadmin.shared.ops.query.attr

import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.schema.{Attr, Ns}
import molecule.ast.model._
import moleculeadmin.shared.ops.query.Base


trait ModeOps extends QueryApi with Base {

  def toggleMode(model: Seq[Element], path: Seq[(String, String)], selAttr: String)(implicit nsMap: Map[String, Ns]): Seq[Element] = {
    val ns                      = path.last._2
    val (before, branch, after) = isolateBranch(model, path)
    val (prev, cur, sub)        = isolateAttr(branch, ns, selAttr)

    //    println("=========================================================")
    //    println(selAttr)
    //    println("path: " + path)
    //    println("---------------------")
    //    println("B 1: List(" + before.mkString("\n          ") + ")")
    //    println("B 2: List(" + branch.mkString("\n          ") + ")")
    //    println("B 3: List(" + after.mkString("\n          ") + ")")
    //    println("---------------------")
    //    println("PRE: List(" + prev.mkString("\n          ") + ")")
    //    println("CUR: List(" + cur.mkString("\n          ") + ")")
    //    println("SUB: List(" + sub.mkString("\n          ") + ")")

    val cur2: Seq[Element] = if (cur.isEmpty) {
      selAttr match {
        case "e" => Seq(Generic(ns, "e", "datom", EntValue))
        case _   => Seq(newAttr1(ns, selAttr, nsMap))
      }
    } else {
      cur.head match {
        case g: Generic => g.attr match {
          case "e"                                                   => g.copy(attr = "e_") +: cur.tail
          case "e_" if sub.nonEmpty && sub.head.isInstanceOf[ReBond] => Atom(ns, "Dummy to keep ns open", "", 1, NoValue) +: cur.tail
          case "e_" if cur.size == 1 && sub.isEmpty                  => Seq(Atom(ns, "Dummy to keep ns open", "", 1, NoValue))
          case "e_"                                                  => cur.tail
        }
        case a: Atom    => a.attr.last match {
          // nil --> None
          case '_' if a.value == Fn("not", None) =>
            if (prev.isEmpty && sub.nonEmpty && sub.head.isInstanceOf[ReBond])
              Seq(Atom(ns, "Dummy to keep ns open", "", 1, NoValue))
            else if (prev.isEmpty && sub.isEmpty)
              Seq(Atom(ns, "Dummy to keep ns open", "", 1, NoValue))
            else
              Nil

          // tacit --> optional
          case '_' =>
            val value1 = if (a.enumPrefix.isDefined) EnumVal else VarValue
            Seq(a.copy(attr = a.attr.init + "$", value = value1, keys = Nil))

          // optional --> nil
          case '$' => Seq(a.copy(attr = a.attr.init + "_", value = Fn("not", None), keys = Nil))

          // mandatory --> tacit
          case _ => a.copy(attr = a.attr + "_") +: cur.tail
        }
      }
    }

    val result = prev ++ cur2 ++ sub
    //    println("---------------------")
    //    println("RES: List(" + (before ++ result ++ after).mkString("\n          ") + ")")
    before ++ result ++ after
  }


  def setMode(
    model: Seq[Element],
    path: Seq[(String, String)],
    selAttr: String,
    mode: String
  )(implicit nsMap: Map[String, Ns]): Seq[Element] = {
    val ns                      = path.last._2
    val (before, branch, after) = isolateBranch(model, path)
    val (prev, cur, sub)        = isolateAttr(branch, ns, selAttr)
    val dummyAtom               = Atom(ns, "Dummy to keep ns open", "", 1, NoValue)
    val single                  =
      prev.isEmpty && sub.isEmpty && after.isEmpty ||
        sub.nonEmpty && sub.head.isInstanceOf[ReBond]

    //    println("=========================================================")
    //    println(selAttr + "   " + mode)
    //    println("path: " + path)
    //    println("---------------------")
    //    println("B 1: List(" + before.mkString("\n          ") + ")")
    //    println("B 2: List(" + branch.mkString("\n          ") + ")")
    //    println("B 3: List(" + after.mkString("\n          ") + ")")
    //    println("---------------------")
    //    println("PRE: List(" + prev.mkString("\n          ") + ")")
    //    println("CUR: List(" + cur.mkString("\n          ") + ")")
    //    println("SUB: List(" + sub.mkString("\n          ") + ")")

    val cur2: Seq[Element] = if (cur.isEmpty) {
      newAttr2(ns, selAttr, nsMap, mode, single)
    } else {
      cur.head match {
        case g: Generic =>
          mode match {
            case "mandatory"      => Seq(g.copy(attr = "e"))
            case "tacit"          => Seq(g.copy(attr = "e_"))
            case "none" if single => Seq(dummyAtom)
            case "none"           => Nil
          }

        case a@Atom(_, _, _, _, value1, enumPrefix, _, _) =>
          val value2 = if (enumPrefix.isDefined)
            EnumVal
          else if (value1 == Fn("not", None))
            VarValue
          else
            value1
          val value3 = if (enumPrefix.isDefined) EnumVal else VarValue
          mode match {
            case "mandatory"      => a.copy(attr = selAttr, value = value2) +: cur.tail
            case "tacit"          => a.copy(attr = selAttr + "_", value = value2) +: cur.tail
            case "optional"       => Seq(a.copy(attr = selAttr + "$", value = value3, keys = Nil))
            case "nil"            => Seq(a.copy(attr = selAttr + "_", value = Fn("not", None), keys = Nil))
            case "none" if single => Seq(dummyAtom)
            case "none"           => Nil
          }
      }
    }

    val result = prev ++ cur2 ++ sub

    //    println("---------------------")
    //    println("CUR 2: List(" + cur2.mkString("\n          ") + ")")
    //    println("---------------------")
    //    println("RES: List(" + (before ++ result ++ after).mkString("\n          ") + ")")
    before ++ result ++ after
  }


  private def newAttr1(ns: String, attr: String, nsMap: Map[String, Ns], ext: String = ""): GenericAtom = nsMap(ns).attrs.collectFirst {
    case Attr(_, `attr`, _, "datom", _, _, _, _, _, _, _, _, _)                      => Generic(ns, "e" + ext, "datom", EntValue)
    case Attr(_, `attr`, card, tpe, Some(_), _, _, _, _, _, _, _, _) if ext == "nil" => Atom(ns, attr + "_", tpe, card, Fn("not", None), Some(s":$ns.$attr/"))
    case Attr(_, `attr`, card, tpe, Some(_), _, _, _, _, _, _, _, _)                 => Atom(ns, attr + ext, tpe, card, EnumVal, Some(s":$ns.$attr/"))
    case Attr(_, `attr`, card, tpe, _, _, _, _, _, _, _, _, _) if ext == "nil"       => Atom(ns, attr + "_", tpe, card, Fn("not", None))
    case Attr(_, `attr`, card, tpe, _, _, _, _, _, _, _, _, _)                       => Atom(ns, attr + ext, tpe, card, VarValue)
  }.get


  private def newAttr2(ns: String, attr: String, nsMap: Map[String, Ns], mode: String, single: Boolean): Seq[GenericAtom] = mode match {
    case "mandatory" if attr == "e" => Seq(Generic(ns, "e", "datom", EntValue))
    case "tacit" if attr == "e"     => Seq(Generic(ns, "e_", "datom", EntValue))
    case "mandatory"                => nsMap(ns).attrs.collectFirst {
      case Attr(_, `attr`, card, tpe, Some(_), _, _, _, _, _, _, _, _) => Seq(Atom(ns, attr, tpe, card, EnumVal, Some(s":$ns.$attr/")))
      case Attr(_, `attr`, card, tpe, _, _, _, _, _, _, _, _, _)       => Seq(Atom(ns, attr, tpe, card, VarValue))
    }.get
    case "tacit"                    => nsMap(ns).attrs.collectFirst {
      case Attr(_, `attr`, card, tpe, Some(_), _, _, _, _, _, _, _, _) => Seq(Atom(ns, attr + "_", tpe, card, EnumVal, Some(s":$ns.$attr/")))
      case Attr(_, `attr`, card, tpe, _, _, _, _, _, _, _, _, _)       => Seq(Atom(ns, attr + "_", tpe, card, VarValue))
    }.get
    case "optional"                 => nsMap(ns).attrs.collectFirst {
      case Attr(_, `attr`, card, tpe, Some(_), _, _, _, _, _, _, _, _) => Seq(Atom(ns, attr + "$", tpe, card, EnumVal, Some(s":$ns.$attr/")))
      case Attr(_, `attr`, card, tpe, _, _, _, _, _, _, _, _, _)       => Seq(Atom(ns, attr + "$", tpe, card, VarValue))
    }.get
    case "nil"                      => nsMap(ns).attrs.collectFirst {
      case Attr(_, `attr`, card, tpe, Some(_), _, _, _, _, _, _, _, _) => Seq(Atom(ns, attr + "_", tpe, card, Fn("not", None), Some(s":$ns.$attr/")))
      case Attr(_, `attr`, card, tpe, _, _, _, _, _, _, _, _, _)       => Seq(Atom(ns, attr + "_", tpe, card, Fn("not", None)))
    }.get
    case "none" if single           => Seq(Atom(ns, dummy, "", 1, NoValue, None, List(), List()))
    case "none"                     => Nil
  }
}
