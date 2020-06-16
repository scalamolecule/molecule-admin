package moleculeadmin.client.app.logic.common


import boopickle.Default._
import moleculeadmin.client.app.html.common.TopMenuElements
import moleculeadmin.shared.util.Path
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.{Anchor, Select}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._
import util.client.rx.RxBindings


case class TopMenu(
  dbs: Seq[String],
  curDb: String,
  curPage: String,
  subMenu: Frag
)(implicit val ctx: Ctx.Owner) extends TopMenuElements with RxBindings {


  def render: dom.Node = _topBar(
    _logo(Path.images("M42.png"), "/"),
    if (curPage == "dbs")
      span(
        span(
          fontWeight.bold,
          "Molecule Admin"
        ),
        span(
          fontStyle.italic,
          " - an administration tool for ",
          a(href := "https://www.datomic.com/on-prem.html", "Datomic"),
          " databases and ",
          a(href := "http://www.scalamolecule.org", "molecules")
        )
      )
    else
      Seq(
        _dbSelector(dbSelector),
        dbLink("schema"),
        dbLink("query"),
        _space,
        subMenu
      )
  ).render


  def dbSelector: Select = {
    val selector = select(
      for (db1 <- dbs) yield option(
        db1,
        value := s"$curPage?db=$db1",
        if (db1 == curDb) selected := true else ()
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
}