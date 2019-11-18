package moleculeadmin.client.app.domain.schema.definition
import boopickle.Default._
import moleculeadmin.client.app.domain.schema.SchemaState._
import rx.{Ctx, Rx}
import scalatags.JsDom.all._


case class Breadcrumb(db: String, part: String, ns: String, attr: String)
                     (implicit val ctx: Ctx.Owner) extends Base {

  type keepBooPickleImport = PickleState

  def render = Rx(
    table(cls := "breadcrumb",
      if (attr.nonEmpty)
        tr(
          td(
            h5(
              i(cls := "fas fa-database", color := "#666", paddingRight := 8),
              a(href := "#", db)
            ),
            onclick := { () =>
              open() = Nil
              curPart() = Nil
            }
          ),
          td(
            h5(
              img(src := "/versionedAssets/icon/64/png/016-business-3.png", height := 16, width := 23, opacity := "0.55", marginBottom := 3, paddingRight := 7),
              a(href := "#", part)
            ),
            onclick := { () =>
              open() = open.now.map {
                case (`part`, _, _)       => (part, Nil, None)
                case (part1, nss1, attr1) => (part1, nss1, attr1)
              }
            }
          ),
          td(
            h5(
              img(src := "/versionedAssets/icon/64/png/034-black-3.png", height := 16, width := 25, opacity := "0.6", marginBottom := 2, paddingRight := 9),
              a(href := "#", ns)
              //                  a(href := "#", ns.capitalize)
            ),
            onclick := { () =>
              open() = open.now.map {
                case (`part`, nss1, _) => (part, nss1, None)
                case (part1, nss1, _)  => (part1, nss1, None)
              }
            }
          ),
          td(
            h5(span("•", paddingRight := 9), attr)
          )
        )
      else if (ns.nonEmpty)
        tr(
          td(
            h5(
              i(cls := "fas fa-database", color := "#666", paddingRight := 8),
              a(href := "#", db)
            ),
            onclick := { () =>
              open() = Nil
              curPart() = Nil
            }
          ),
          td(
            h5(
              img(src := "/versionedAssets/icon/64/png/016-business-3.png", height := 16, width := 23, opacity := "0.55", marginBottom := 3, paddingRight := 7),
              a(href := "#", part)
            ),
            onclick := { () =>
              open() = open.now.map {
                case (`part`, _, _)       => (part, Nil, None)
                case (part1, nss1, attr1) => (part1, nss1, attr1)
              }
            }
          ),
          td(
            h5(
              img(src := "/versionedAssets/icon/64/png/034-black-3.png", height := 16, width := 25, opacity := "0.6", marginBottom := 2, paddingRight := 9),
              ns
              //                  ns.capitalize
            )
          )
        )
      else if (part.nonEmpty)
        tr(
          td(
            h5(
              i(cls := "fas fa-database", color := "#666", paddingRight := 8),
              a(href := "#", db)
            ),
            onclick := { () =>
              open() = Nil
              curPart() = Nil
            }
          ),
          td(
            h5(img(src := "/versionedAssets/icon/64/png/016-business-3.png", height := 16, width := 23, opacity := "0.55", marginBottom := 3, paddingRight := 7), part)
          )
        )
      else
        tr(
          td(
            h5(i(cls := "fas fa-database", color := "#666", paddingRight := 8), db)
          )
        )
    )
  )
}
