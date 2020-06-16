package moleculeadmin.client.app.logic

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.html.dbs.DbsElements
import moleculeadmin.client.app.logic.SchemaClient._
import moleculeadmin.client.app.logic.common.TopMenu
import moleculeadmin.client.dbsWire
import moleculeadmin.shared.ast.db.Db
import org.scalajs.dom._
import org.scalajs.dom.html._
import rx._
import scalatags.JsDom
import scalatags.JsDom.all._
import util.client.rx.RxBindings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DbsClient")
object DbsClient extends RxBindings with DbsElements {

  implicit val ctx: Ctx.Owner = rx.Ctx.Owner.safe()
  val dbs: Var[Seq[Db]] = Var(Nil)

  @JSExport
  def load(): Unit = {
    document.body.appendChild(TopMenu(Nil, "", "dbs", subMenu).render)
    document.body.appendChild(_container(body))

    dbsWire().dbList.call().map {
      case Left(Seq("transactor down")) => set(_renderStartTransactor)
      case Left(msg :: stackTrace)      => set(_renderError(msg, stackTrace))
      case Right(dbs0)                  => dbs() = dbs0; renderDbList()
    }
  }

  // Dynamic elements
  val body      = _row("Fetching list of databases...").render
  val tableBody = tbody().render

  def set(node: JsDom.TypedTag[Div]): Unit = {
    body.innerHTML = ""
    body.appendChild(node.render)
  }

  def renderDbList(): Unit = {
    body.innerHTML = ""
    body.appendChild(
      div(
        _table(tableBody),
        createNewDb
      ).render
    )
    populateRows()
  }

  def populateRows(): Unit = Rx {
    tableBody.innerHTML = ""
    dbs().foreach { db =>
      tableBody.appendChild(
        _tableRow(db.i, db.name, pencilCell(db), actionCell(db))
      )
    }
  }

  def refresh(db: Db, update: Db => Db): () => Unit = {
    () =>
      // Update dom
      dbs() = dbs.now.map {
        case `db`    => update(db)
        case otherDb => otherDb
      }
      // Re-find input in updated dom
      val in = document.getElementById("path" + db.i).asInstanceOf[Input]
      if (in != null)
        in.focus()

      // Remove msg from previous actions
      createDbStatus() = None
  }

  def pencilCell(db: Db): JsDom.TypedTag[TableCell] = {
    _pencilCell(
      db.open,
      refresh(db, _.copy(open = !db.open, pathMsg = "", deleteDbMsg = ""))
    )
  }

  def actionCell(db: Db): JsDom.TypedTag[TableCell] = {
    if (db.open) editCell(db) else _linkCell(db)
  }

  def editCell(db: Db): JsDom.TypedTag[TableCell] = {
    val pathInput      = _pathInput(db.defFilePath, db.i)
    val pathStatus     = Var[Msg](None)
    val deleteDbInput  = _deleteDbInput
    val deleteDbStatus = Var[Msg](None)
    _editCell(
      db,
      pathInput,
      pathStatus,
      savePath(db, pathStatus, pathInput),
      checkPath(db, pathStatus, pathInput),
      skipManaging(db, pathStatus),
      deleteDbInput,
      deleteDbStatus,
      deleteDb(db, deleteDbStatus, deleteDbInput)
    )
  }

  def editCell2(db: Db): JsDom.TypedTag[TableCell] = {
    val pathInput      = _pathInput(db.defFilePath, db.i)
    val pathStatus     = Var[Msg](None)
    val deleteDbInput  = _deleteDbInput
    val deleteDbStatus = Var[Msg](None)
    td(
      _pathForm(db, pathInput, pathStatus,
        savePath(db, pathStatus, pathInput),
        checkPath(db, pathStatus, pathInput),
        skipManaging(db, pathStatus)
      ),
      br,
      _deleteDbForm(db, deleteDbInput, deleteDbStatus,
        deleteDb(db, deleteDbStatus, deleteDbInput)
      ),
    )
  }

  def savePath(
    db: Db,
    pathStatus: Var[Msg],
    pathInput: Input
  ): () => Boolean = { () =>
    // Client validation
    validateDefFilePath(pathInput.value) match {
      case Left(err) =>
        pathStatus() = Some(Left(err))

      case Right(path) if path == db.defFilePath =>
        pathStatus() = Some(Left("Path already saved."))

      case Right(path) =>
        pathStatus() = Some(Right("_Saving..."))
        // Server validation/saving
        dbsWire().saveDefFilePath(db.name, path).call().foreach {
          case Left(err) => pathStatus() = Some(Left(err))
          case Right(ok) => refresh(db, _.copy(defFilePath = path, pathMsg = ok))()
        }
    }
    false // don't send form
  }

  def checkPath(db: Db, pathStatus: Var[Msg], pathInput: Input): () => Unit = { () =>
    if (pathInput.value != db.defFilePath) {
      pathStatus() = Some(Left("Please save changed path before checking (or refresh)."))
    } else {
      pathStatus() = Some(Right("_Checking..."))
      dbsWire().checkPath(db.name, db.defFilePath).call().foreach {
        case Left(err) => pathStatus() = Some(Left(err))
        case Right(ok) => pathStatus() = Some(Right(ok))
      }
    }
  }

  def skipManaging(db: Db, pathStatus: Var[Msg]): () => Unit = { () =>
    db.defFilePath match {
      case "" => pathStatus() = Some(
        Left(s"Database `${db.name}` is already not managed."))

      case _ =>
        pathStatus() = Some(Right("_Skipping..."))
        dbsWire().skipManaging(db.name).call().foreach {
          case Left(err) => pathStatus() = Some(Left(err))
          case Right(ok) => refresh(db, _.copy(defFilePath = "", pathMsg = ok))()
        }
    }
  }

  def deleteDb(
    db: Db,
    deleteDbStatus: Var[Msg],
    deleteDbInput: Input
  ): () => Boolean = { () =>
    // Client validation
    deleteDbInput.value.trim match {
      case "" => deleteDbStatus() = Some(Left(s"Please enter the database name."))

      case dbName if dbName != db.name =>
        deleteDbStatus() = Some(Left("Name doesn't match this database."))

      case dbName =>
        // Last chance to abort
        if (
          window.confirm(
            s"Do you really want to delete the database '$dbName'?!" +
              s"\nThis can't be undone."
          )
        ) {
          deleteDbStatus() = Some(Right("_Deleting database..."))
          // Server validation/saving
          dbsWire().deleteDb(dbName).call().foreach {
            case Left(err)   => deleteDbStatus() = Some(Left(err))
            case Right(dbs0) =>
              dbs() = dbs0
              _createDbInput.value = ""
              // re-use status at bottom for successful deletion
              createDbStatus() = Some(Right(
                s"Successfully deleted database '$dbName'."))
          }
        } else {
          // aborted
          deleteDbStatus() = None
        }
    }
    false // don't send form
  }

  def createNewDb: JsDom.TypedTag[Div] = {
    val createDb    = { () =>
      val newDbName = _createDbInput.value
      // Client validation
      newDbName.trim match {
        case "" => createDbStatus() = Some(Left(
          s"Please enter a new database name."))

        case _ if dbs.now.exists(_.name == newDbName) =>
          createDbStatus() = Some(Left(
            s"Database name '$newDbName' already exists."))

        case r"[a-zA-Z][a-zA-Z0-9-_]*" =>
          createDbStatus() = Some(Right("_Creating new database..."))

          // Server validation/saving
          dbsWire().createDb(newDbName).call().foreach {
            case Left(err)   => createDbStatus() = Some(Left(err))
            case Right(dbs0) =>
              dbs() = dbs0
              _createDbInput.value = ""
              createDbStatus() = Some(Right(
                s"New database '$newDbName' successfully created."))
          }

        case _ => createDbStatus() = Some(Left(
          s"Database name '$newDbName' not a valid db name. " +
            s"Has to match [a-zA-Z][a-zA-Z0-9-_]*"))
      }
      false // don't send form
    }
    _createDbForm(_createDbInput, createDb)
  }


  def validateDefFilePath(path0: String): Either[String, String] = {
    val path     = path0.trim
    val segments = path.trim.split("/")
    if (path.head != '/' || !path.contains("/")) {
      Left("Please provide a full path to the schema definition file. " +
        s"Invalid path found:\n$path")
    } else if (!segments.last.endsWith("Definition.scala")) {
      Left("Schema definition file should have a name " +
        "like '<db-name>Definition.scala'. " +
        s"\nInvalid file name/path entered:\n$path")
    } else {
      // We have a plausible path
      Right(path)
    }
  }
}
