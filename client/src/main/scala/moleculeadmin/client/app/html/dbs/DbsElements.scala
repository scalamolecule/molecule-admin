package moleculeadmin.client.app.html.dbs

import moleculeadmin.client.app.html.AppElements
import moleculeadmin.shared.ast.db.Db
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html._
import rx._
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import util.client.rx.RxBindings
import scala.{Option => sOption}


trait DbsElements extends RxBindings with AppElements {

  type Msg = sOption[Either[String, String]]


  // Initial errors ------------------------------------------------------------

  def _renderStartTransactor: JsDom.TypedTag[Div] = {
    div(
      p("Please start the Datomic transactor in the terminal. For instance like this:"),
      pre(
        """cd <datomic installation folder>
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

  def _renderError(msg: String, stackTrace: List[String]): JsDom.TypedTag[Div] = {
    div(
      p(b("An error occured:")),
      pre(msg),
      p(),
      p(b("Stacktrace:")),
      ul(
        stackTrace.map(li(_))
      )
    )
  }


  // Helpers -------------------------------------------------------------------

  def _dynamicMsg(tag: String, msg: String, status: Var[Msg])
                 (implicit ctx: Ctx.Owner): Rx.Dynamic[TypedTag[Element]] = Rx {
    val ok  = pre(id := tag, color := Color.okMsg)
    val err = pre(id := tag, color := Color.errMsg)
    status() match {
      case None if msg.nonEmpty                    => ok(msg)
      case None                                    => ok(display.none)
      case Some(Right(msg)) if msg.startsWith("_") => div(_sync(0, 10), msg.tail)
      case Some(Right(msg))                        => ok(msg)
      case Some(Left(msg))                         => err(msg)
    }
  }


  // Layout/table --------------------------------------------------------------

  def _container(body: Div): Div = _containerFluid2(
    paddingLeft := 32,
    paddingTop := 17,
    body
  ).render

  def _table(tableBody: TableSection): TypedTag[Table] = {
    table(cls := "table",
      thead(
        tr(
          th(),
          th("Database"),
          th(),
          th()
        )
      ),
      tableBody
    )
  }

  def _tableRow(
    i: Int,
    name: String,
    pencilCell: JsDom.TypedTag[TableCell],
    actionCell: JsDom.TypedTag[TableCell]
  ): TableRow = {
    tr(
      td(i, color := Color.textDarkGray, paddingRight := 6),
      td(name),
      pencilCell,
      actionCell
    ).render
  }


  // Table cells ---------------------------------------------------------------

  def _pencilCell(open: Boolean, click: () => ()): JsDom.TypedTag[TableCell] = {
    td(
      paddingLeft := 20,
      paddingRight := 20,
      a(
        href := "#",
        cls := "oi oi-pencil",
        color := (if (open) Color.iconGray else Color.iconGreen),
      ),
      cursor.pointer,
      onclick := click
    )
  }

  // link or edit cell..................

  def _linkCell(db: Db): JsDom.TypedTag[TableCell] = {
    val actionLinks = if (db.defFilePath.isEmpty) Nil else Seq(
      a(href := s"/schema?db=${db.name}", "Schema", paddingRight := 15),
      a(href := s"/query?db=${db.name}", "Query")
    )
    td(actionLinks)
  }

  def _editCell(
    db: Db,
    pathInput: Input,
    pathStatus: Var[Msg],
    savePath: () => Boolean,
    checkPath: () => (),
    skipManaging: () => (),

    deleteDbInput: Input,
    deleteDbStatus: Var[Msg],
    deleteDb: () => Boolean
  )(implicit ctx: Ctx.Owner): JsDom.TypedTag[TableCell] = {
    td(
      _pathForm(db, pathInput, pathStatus, savePath, checkPath, skipManaging),
      br,
      _deleteDbForm(db, deleteDbInput, deleteDbStatus, deleteDb),
    )
  }


  // Schema definition file path -----------------------------------------------

  def _pathInput(path: String, i: Int): Input = input(
    cls := "inputs",
    id := "path" + i,
    marginTop := 5,
    marginBottom := 8,
    size := 100,
    if (path.nonEmpty)
      value := path
    else
      placeholder := s"Schema definition file path...",
  ).render

  def _button(
    label: String,
    bType: String = "submit",
    action: () => () = () => (),
  ): TypedTag[Button] = {
    button(
      cls := "btn btn-outline-dark btn-sm",
      marginRight := 10,
      tpe := bType,
      label,
      onclick := action
    )
  }

  def _pathForm(
    db: Db,
    pathInput: Input,
    pathStatus: Var[Msg],
    savePath: () => Boolean,
    checkPath: () => (),
    skipManaging: () => ()
  )(implicit ctx: Ctx.Owner): TypedTag[Div] = {
    val dbName = db.name
    div(
      s"Save full path to schema definition file defining the database `$dbName`:",
      form(
        pathInput,
        br,
        _button("Save path"),
        if (pathInput.value.nonEmpty) Seq(
          _button("Check path", "button", checkPath),
          _button(s"Skip managing db '$dbName'", "button", skipManaging),
        ) else (),
        onsubmit := savePath,
        marginBottom := 10
      ),
      _dynamicMsg("pathMsg" + db.i, db.pathMsg, pathStatus),
    )
  }


  // Delete database -----------------------------------------------------------

  def _deleteDbInput: Input = input(
    cls := "inputs",
    marginTop := 5,
    marginBottom := 8,
    size := 30,
    placeholder := "Database name..."
  ).render

  def _deleteDbForm(
    db: Db,
    deleteDbInput: Input,
    deleteDbStatus: Var[Msg],
    deleteDb: () => Boolean
  )(implicit ctx: Ctx.Owner): TypedTag[Div] = {
    val dbName = db.name
    div(
      div("Delete database", fontStyle.italic),
      div("Please make a database backup before deleting! Enter the " +
        "database name to confirm that you want to delete this database:"),
      form(
        deleteDbInput,
        button(
          cls := "btn btn-outline-dark btn-sm warning",
          tpe := "submit",
          marginLeft := 10,
          s"Delete database '$dbName' (!!)"
        ),
        onsubmit := deleteDb,
        //        marginBottom := 10
      ),
      _dynamicMsg("deleteMsg" + db.i, db.deleteDbMsg, deleteDbStatus),
      marginBottom := 10
    )
  }


  // Create database -----------------------------------------------------------

  val _createDbInput: Input = input(
    cls := "inputs",
    size := 25,
    marginTop := 5,
    marginBottom := 8,
    placeholder := "Name of new database..."
  ).render

  val createDbStatus: Var[Msg] = Var(None)

  def _createDbForm(
    createDbInput: Input,
    createDb: () => Boolean
  )(implicit ctx: Ctx.Owner): TypedTag[Div] = {
    val ok  = pre(id := "createDbMsg", color := Color.okMsg)
    val err = pre(id := "createDbMsg", color := Color.errMsg)
    div(
      form(
        marginTop := 15,
        marginBottom := 5,
        createDbInput,
        button(
          cls := "btn btn-outline-dark btn-sm",
          tpe := "submit",
          marginTop := -2,
          marginLeft := 10,
          "Create new database"
        ),
        onsubmit := createDb,
      ),
      Rx(
        createDbStatus() match {
          case None                                    => ok("")
          case Some(Right(msg)) if msg.startsWith("_") => div(_sync(0, 10), msg.tail)
          case Some(Right(msg))                        => ok(msg)
          case Some(Left(msg))                         => err(msg)
        }
      )
    )
  }
}
