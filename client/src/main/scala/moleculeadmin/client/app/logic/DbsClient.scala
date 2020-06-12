package moleculeadmin.client.app.logic

import autowire._
import boopickle.Default._
import util.client.rx.RxBindings
import moleculeadmin.client.app.html.dbs.DbsElements
import moleculeadmin.client.app.logic.SchemaClient._
import moleculeadmin.client.app.logic.common.TopMenu
import moleculeadmin.client.dbsWire
import moleculeadmin.shared.api.DbsApi
import org.scalajs.dom._
import org.scalajs.dom.html.Table
import rx._
import scalatags.JsDom
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.{Option => sOption}


@JSExportTopLevel("DbsClient")
object DbsClient extends RxBindings with DbsElements {

  implicit val ctx = rx.Ctx.Owner.safe()

  type Data = (Int, String, sOption[Boolean], sOption[String], Var[String], Var[Boolean])

  val dbs: Var[Seq[Data]] = Var(Nil)

  def init(dbs0: DbsApi#Dbs): Unit = {
    dbs() = dbs0.zipWithIndex.foldLeft(Seq.empty[Data], true) {
      case ((acc, first), ((db, None, path), i)) if first =>
        (acc :+ (i, db, None, path, Var(""), Var(true)), false)

      case ((acc, first), ((db, isM, path), i)) =>
        (acc :+ (i, db, isM, path, Var(""), Var(false)), first)
    }._1
  }

  def validateDefFilePath(path0: String): Either[String, String] = {
    val path     = path0.trim
    val segments = path.trim.split("/")

    // Validate fastest evaluable mistakes first
    if (path.head != '/' || !path.contains("/")) {
      Left("Please provide a full path to the Schema definition file" +
        s"\nInvalid path: $path")

    } else if (!segments.last.endsWith("Definition.scala")) {
      Left("Schema definition file should be a file with a name like '<something>Definition.scala' " +
        s"\nInvalid path: $path")

    } else {
      // We have a plausible path
      Right(path)
    }
  }


  def actions(
    db: String,
    isMolecular: sOption[Boolean],
    defFilePath: sOption[String],
    msg: Var[String],
    first: Var[Boolean]
  ): Seq[JsDom.Modifier] = {

    (isMolecular, defFilePath) match {
      case (Some(false), _)         => Seq("(Non-molecule database)")
      case (Some(true), Some(path)) => Seq(
        path, // Validated path
        Rx {
          pre(if (msg().isEmpty) display.none else ())(msg())
        },
        button(
          cls := "btn btn-outline-dark btn-sm",
          "New path",
          marginLeft := "10px",
          onclick := { () =>
            dbsWire().reset(db).call() map init
          }
        ),
        button(
          cls := "btn btn-outline-dark btn-sm",
          "Re-parse",
          marginLeft := "5px",
          onclick := { () =>
            // Client validation
            validateDefFilePath(path) match {
              case Left(error)  => msg() = error
              case Right(path1) =>
                // Server validation
                dbsWire().saveDefFilePath(db, path1).call().map {
                  case Left(error) => msg() = error
                  case Right(_)    => msg() = "Successfully re-synced schema definition file"
                }
            }
          }
        ),
        br,
        a(href := s"/schema?db=$db", "Schema"), " | ",
        a(href := s"/query?db=$db", "Query")
      )
      case (_, path)                => {
        val input_ = input(
          cls := "inputs",
          size := 80,
          if (path.isDefined)
            value := path.get
          else
            placeholder := s"Schema definition file path...",
          autofocus := Rx(first())
        ).render

        Seq(
          form(
            input_,
            button(
              cls := "btn btn-outline-dark btn-sm",
              tpe := "submit",
              marginLeft := "5px",
              "Save path"
            ),
            Rx {
              pre(if (msg().isEmpty) display.none else ())(msg())
            },
            onsubmit := { () =>
              // Client validation
              validateDefFilePath(input_.value) match {
                case Left(error)  => msg() = error
                case Right(path1) =>
                  // Server validation
                  dbsWire().saveDefFilePath(db, path1).call().map {
                    case Left(error)       => msg() = error
                    case Right(updatedDbs) => init(updatedDbs)
                  }
              }
              false // don't send form
            }
          )
        )
      }
    }
  }

  def dbTable(dbs0: DbsApi#Dbs): JsDom.TypedTag[Table] = {
    init(dbs0)
    table(cls := "table table-bordered",
      thead(
        tr(
          th(),
          th("Database"),
          th("Actions")
        )
      ),
      Rx {
        tbody(
          for ((i, db, isM, defFile, err, first) <- dbs()) yield {
            tr(
              td(i + 1),
              td(db),
              td(actions(db, isM, defFile, err, first))
            )
          }
        )
      }
    )
  }


  @JSExport
  def load(): Unit = {

    document.body.appendChild(TopMenu(Nil, "", "dbs", subMenu).render)

    dbsWire().dbList().call().map { result =>
      val content = result match {
        case Left(Seq("transactor not running")) => _renderStartTransactor
        case Left(msg :: stackTrace)             => _renderError(msg, stackTrace)
        case Right(dbs0)                         => dbTable(dbs0)
      }
      document.body.appendChild(
        _containerFluid2(
          paddingLeft := 32,
          paddingTop := 20,
          _row(
            content.render
          )
        ).render
      )
    }
  }
}
