package moleculeadmin.client.app.logic.common


import autowire._
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.html.common.TopMenuElements
import moleculeadmin.client.queryWireWS
import moleculeadmin.shared.util.Path
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.{Anchor, Select}
import org.scalajs.dom.raw.{Event, MessageEvent}
import rx.Ctx
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class TopMenu(
  dbs: Seq[String],
  curDb: String,
  curPage: String,
  subMenu: Frag
)(implicit val ctx: Ctx.Owner) extends TopMenuElements with RxBindings {

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

  import moleculeadmin.client.app.logic.query.QueryState._

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
      a(href := "#",
        onclick := { _: Event =>

          //          println("calling queryWireWebSocket().test(1)")
          //            println("------------")
          //          queryWireAjax().testStr("xyz").call().map { j =>
          //            println("result aj " + j)
          //          }
          import scala.scalajs.js.timers.setTimeout

          //          queryWireWebSocket().testInt(1).call().map { res =>
          //            println(s"testInt: " + res)
          //          }
//          queryWireWebSocket().testInt(1).call().map { res =>
//            println(s"testInt: " + res)
//          }
          queryWireWS().testInt(2).call().map { res =>
            println(s"testInt: " + res)

            //          setTimeout(1) {
            println("------------")
            //            queryWireWebSocket().testStr("abc").call().map { res =>
            //              println(s"testStr: " + res)
            //            }
            queryWireWS().testInt(3).call().map { res =>
              println(s"testInt: " + res)
            }
          }
          //          }


        },
        "test1 "
      ),
      //      dbLink("log"),
      //      pageLink("monitor"),
      _space,
      subMenu
    )
  ).render
}