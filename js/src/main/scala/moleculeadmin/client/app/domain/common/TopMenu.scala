package moleculeadmin.client.app.domain.common

import moleculeadmin.client.app.element.common.TopMenuElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.util.Path
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.{Anchor, Select}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._


case class TopMenu(dbs: Seq[String],
                   curDb: String,
                   curPage: String,
                   subMenu: Frag
                  )(implicit val ctx: Ctx.Owner)
  extends TopMenuElements with RxBindings {

  def dbSelector: Select = {
    val selector = select(
      for (db <- dbs) yield option(
        db,
        value := s"$curPage?db=$db",
        if (db == curDb) selected := true else ()
      ),
    ).render
    selector.onchange = _ => document.location.href = selector.value
    selector
  }

  def dbLink(page: String): JsDom.TypedTag[Anchor] = {
    val (page1, href1) = (page.capitalize, page + "?db=" + curDb)
    if (page == curPage)
      _curPageLink(page1, href1)
    else
      _pageLink(page1, href1)
  }

  val views = Map(
    "schema" -> Seq("Definition", "Value", "Sync"),
  )


  def render: dom.Node = _topBar(
    _logo(Path.images("M42.png"), "/"),
    if (curPage == "dbs") Seq(
      "Molecule Admin"
    ) else Seq(
      //      dbLink("transactor"),
      //      dbLink("dbs"),
      _dbSelector(dbSelector),
      dbLink("schema"),
      dbLink("query"),
      dbLink("log"),
      //      pageLink("monitor"),
      _space,
      subMenu
    )
  ).render
}