package moleculeadmin.client.app.domain.schema.definition
import moleculeadmin.client.app.element.schema.DefinitionElements
import moleculeadmin.client.app.domain.schema.SchemaState._
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin
import org.scalajs.dom.document
import org.scalajs.dom.raw.Node
import rx.Ctx


abstract class Base(implicit ctx: Ctx.Owner) extends RxBindings with BaseApi with HelpersAdmin with DefinitionElements {

  def hideAttr(part: String, ns: String, attr: String) = { () =>
    processing() = ""
    open() = open.now.map {
      case (`part`, nss1, _) => (part, nss1.filterNot(_ == ns) :+ ns, None)
      case (part1, nss1, _)  => (part1, nss1, None)
    }
    curPart() = curPart.now.filterNot(_ == part) :+ part
    level() = 3
  }

  def hideNs(part: String, ns: String) = { () =>
    processing() = ""
    if (level.now < 3) {
      open() = open.now.map {
        case (`part`, nss1, _) => (part, nss1.filterNot(_ == ns), None)
        case (part1, nss1, _)  => (part1, nss1, None)
      }
    }
    curPart() = curPart.now.filterNot(_ == part) :+ part
    level() = 2
  }

  def hidePart(part: String) = { () =>
    processing() = ""
    if (level.now < 2) {
      open() = open.now.flatMap {
        case (`part`, Nil, _)     => None
        case (part1, nss2, attr1) => Some((part1, nss2, attr1))
      }
      curPart() = curPart.now.filterNot(_ == part)
    }
    level() = 1
  }

  def showAttr(part: String, ns: String, attr: String) = { () =>
    processing() = ""
    open() = open.now.map {
      case (`part`, nss1, _) => (part, nss1.filterNot(_ == ns) :+ ns, Some(attr))
      case (part1, nss1, _)  => (part1, nss1, None)
    }
    curPart() = curPart.now.filterNot(_ == part) :+ part
    level() = 3
  }

  def showNs(part: String, ns0: String) = { () =>
    //      val ns = ns0.low // todo: use as client debugging example!
    val ns = ns0
    processing() = ""
    val open1 = open.now.map {
      case (`part`, nss1, _)    => (part, nss1.filterNot(_ == ns) :+ ns, None)
      case (part1, nss1, attr1) => (part1, nss1, attr1)
    }
    open() = if (open1.exists(_._1 == part)) open1 else {open1 :+ (part, Seq(ns), None)}
    curPart() = curPart.now.filterNot(_ == part) :+ part
    level() = 2
  }

  def showPart(part: String) = { () =>
    processing() = ""
    open() = open.now.filterNot(_._1 == part) :+ (part, Nil, None)
    //      open() = open.now :+ (part, Nil, None)
    curPart() = curPart.now.filterNot(_ == part) :+ part
    level() = 1
  }

  def append(idStr: String, nodes: Node*) = {
    val wrapper = document.getElementById(idStr)
    if (wrapper == null)
      throw new RuntimeException(s"Couldn't find wrap element by id `$idStr`.")
    wrapper.innerHTML = ""
    nodes.foreach(node => wrapper.appendChild(node))
  }
}
