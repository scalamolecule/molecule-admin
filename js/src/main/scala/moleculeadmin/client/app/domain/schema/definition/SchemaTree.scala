package moleculeadmin.client.app.domain.schema.definition
import boopickle.Default._
import moleculeadmin.client.app.domain.schema.SchemaState._
import moleculeadmin.shared.ast.schema.{Attr => Attr_, _}
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class SchemaTree(part0: String, nss0: Seq[Ns])(implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport = PickleState

  def treeAttr(part: String, ns: String, attr: String) = {
    val openNs = open.now.find(_._1 == part).get._2.last
    Rx {
      open().find(_._1 == part).get match {
        case (_, nss, Some(`attr`)) if curPart.now.last == part && nss.last == ns =>
          a(href := "#",
            cls := "list-group-item list-group-item-action border-0",
            paddingLeft := 4, paddingRight := 0, paddingTop := 0, paddingBottom := 0,
            backgroundColor := "#c0e2f0",
            attr,
            onclick := hideAttr(part, ns, attr)
          )
        case _                                                                    =>
          a(href := "#", cls := "list-group-item list-group-item-action border-0",
            paddingLeft := 4, paddingRight := 4, paddingTop := 0, paddingBottom := 0,
            if (curPart.now.nonEmpty && curPart.now.last == part && openNs == ns) () else color := "#888",
            attr,
            onclick := showAttr(part, ns, attr)
          )
      }
    }
  }

  def treeNamespace(part: String, ns: String, attrs: Seq[Attr_]) = {
    val (_, openNss, openAttr) = open.now.find(_._1 == part).get
    Rx {
      if (openNss.contains(ns)) {
        a(href := "#", cls := "list-group-item list-group-item-action border-0",
          paddingLeft := 8, paddingRight := 8, paddingTop := 1, paddingBottom := 8,
          if (curPart.now.nonEmpty && curPart.now.last == part && openNss.last == ns && openAttr.isEmpty) backgroundColor := "#c0e2f0" else (),
          div(whiteSpace.nowrap,
            span(cls := "oi oi-caret-bottom", marginLeft := "-3px", paddingRight := 6, fontSize := "85%"),
            //              b(ns.capitalize)
            b(ns)
          ),
          div(cls := "list-group list-group-flush", marginTop := 2,
            for {
              Attr_(_, attr, _, _, _, _, _, _, _, _, _, _, _) <- attrs
            } yield treeAttr(part, ns, attr)
          ),
          onclick := hideNs(part, ns)
        )
      } else {
        a(href := "#",
          cls := "list-group-item list-group-item-action border-0",
          paddingLeft := 8, paddingRight := 8, paddingTop := 1, paddingBottom := 1,
          div(whiteSpace.nowrap,
            span(cls := "oi oi-caret-right", paddingRight := 8, color := "#999", fontSize := "85%"),
            if (curPart.now.nonEmpty && curPart.now.last == part) () else color := "#888",
            ns
            //              ns.capitalize
          ),
          onclick := showNs(part, ns)
        )
      }
    }
  }

  def render = {
    val part = if (part0.isEmpty) "<partition>" else part0
    val msgPart = err(s"errmsg-$part-part")
    Rx {
      val openPart = open.now.find(_._1 == part)
      val curP = curPart.now
      if (curPart.now.nonEmpty && curPart.now.contains(part) && openPart.nonEmpty) {
        val (_, nss1, _) = openPart.get
        a(href := "#", cls := "list-group-item list-group-item-action",
          paddingLeft := 10, paddingRight := 10, paddingTop := 4, paddingBottom := 10,
          if (curPart.now.nonEmpty && curPart.now.last == part && nss1.isEmpty) backgroundColor := "#c0e2f0" else (),
          b(whiteSpace.nowrap,
            span(cls := "oi oi-caret-bottom", verticalAlign := "-1px", marginLeft := "-3px", paddingRight := 5),
            part
          ),
          div(cls := "list-group list-group-flush", marginTop := 5,
            for {
              Ns(_, ns, _, _, _, attrs0) <- nss0
            } yield treeNamespace(part, ns, attrs0)
          ),
          onclick := hidePart(part)
        )
      } else {
        a(href := "#", cls := "list-group-item list-group-item-action py-1", paddingLeft := 10,
          b(whiteSpace.nowrap,
            span(cls := "oi oi-caret-right", verticalAlign := "-1px", paddingRight := 8, color := "#999"),
            part
          ),
          onclick := showPart(part)
        )
      }
    }
  }
}
