package moleculeadmin.client.app.html.dbs

import org.scalajs.dom.html.Span
import scalatags.JsDom
import scalatags.JsDom.all._


trait DbsElements {



  def _renderStartTransactor: JsDom.TypedTag[Span] = {
    span(
      p("Please start the Datomic transactor in the terminal. For instance like this:"),
      pre(
        """
          |cd <datomic installation folder>
          |bin/transactor -Xmx4g -Xms4g -Ddatomic.txTimeoutMsec=60000 config/samples/free-transactor-template.properties
        """.stripMargin),
      p(
        "See the documentation about ",
        a(href := "http://docs.datomic.com/getting-started/connect-to-a-database.html", "how to connect to Datomic"),
        " or ",
        a(href := "http://docs.datomic.com/dev-setup.html", "how to create a local dev connection to Datomic"))
      //      p(),
      //      renderError(e)
    )
  }


  def _renderError(msg: String, stackTrace: List[String]): JsDom.TypedTag[Span] = {
    span(
      p(b("An error occured:")),
      pre(msg),
      p(),
      p(b("Stacktrace:")),
      ul(
        for (l <- stackTrace) yield li(l.toString)
      )
    )
  }
}
