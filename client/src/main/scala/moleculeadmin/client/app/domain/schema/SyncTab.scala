package moleculeadmin.client.app.domain.schema
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.autowire.schemaWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.api.BaseApi
import moleculeadmin.shared.ast.schema.FlatSchema
import moleculeadmin.shared.ops.schema.Sync
import rx.{Ctx, Rx, Var}
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global


case class SyncTab(db: String)(implicit val ctx: Ctx.Owner)
  extends RxBindings with BaseApi with AppElements {

  type keepBooPickleImport_SyncTab = PickleState

  val metaSchema = Var[FlatSchema](null)
  val defSchema  = Var[FlatSchema](null)
  val liveSchema = Var[FlatSchema](null)
  val syncing    = Var[Boolean](false)
  val fetching   = Var[Boolean](false)


  def errorPart(baseParts: Seq[(String, Boolean)], testParts: Seq[(String, Boolean)], base: String, test: String) = li(
    p("Partitions out of sync:", color.red, fontWeight.bold),
    table(cls := "table", width.auto)(
      thead(
        tr(
          th("Partition name", br, s"($base)"),
          th(width := 30.px),
          th("Partition name", br, s"($test)")
        )
      ),
      Rx(
        tbody(
          tr(
            td(
              baseParts.map {
                case (ns, false) => div(ns, color.red, fontWeight.bold)
                case (ns, true)  => div(ns)
              }
            ),
            td(),
            td(
              testParts.map {
                case (ns, false) => div(ns, color.red, fontWeight.bold)
                case (ns, true)  => div(ns)
              }
            )
          )
        )
      )
    )
  )


  def errorNs(data: Seq[(String, Seq[(String, Boolean)], Seq[(String, Boolean)])], base: String, test: String) = li(
    p("Namespaces out of sync:", color.red, fontWeight.bold),
    table(cls := "table", style := "width: auto")(
      thead(
        tr(
          th("Partition", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Namespace name", br, s"($base)"),
          th(style := "width:30px;"),
          th("Namespace name", br, s"($test)")
        )
      ),
      tbody(
        for ((part, baseNss, testNss) <- data) yield {
          tr(
            td(part),
            td(),
            td(
              baseNss.map {
                case (ns, false) => div(ns, color.red, fontWeight.bold)
                case (ns, true)  => div(ns)
              }
            ),
            td(),
            td(
              testNss.map {
                case (ns, false) => div(ns, color.red, fontWeight.bold)
                case (ns, true)  => div(ns)
              }
            )
          )
        }
      )
    )
  )


  def errorAttr(data: Seq[(String, String, Seq[(String, Boolean)], Seq[(String, Boolean)])], base: String, test: String) = li(
    p("Attribute names out of sync:", color.red, fontWeight.bold),
    table(cls := "table", style := "width: auto")(
      thead(
        tr(
          th("Partition", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Namespace", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Attribute name", br, s"($base)"),
          th(style := "width:30px;"),
          th("Attribute name", br, s"($test)")
        )
      ),
      tbody(
        for ((part, ns, baseAttrs, testAttrs) <- data) yield {
          tr(
            td(part),
            td(),
            td(ns),
            td(),
            td(
              baseAttrs.map {
                case (attr, false) => div(color.red, fontWeight.bold, attr)
                case (attr, true)  => div(attr)
              }
            ),
            td(),
            td(
              testAttrs.map {
                case (attr, false) => div(color.red, fontWeight.bold, attr)
                case (attr, true)  => div(attr)
              }
            )
          )
        }
      )
    )
  )


  def errorAttrCard(data: Seq[(String, String, Seq[(String, Int, Int, Boolean, String)])], base: String, test: String) = li(
    p("Attribute cardinalities out of sync:", color.red, fontWeight.bold),
    table(cls := "table", style := "width: auto")(
      thead(
        tr(
          th("Partition", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Namespace", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Attribute", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Cardinality", br, s"($base)"),
          th(style := "width:30px;"),
          th("Cardinality", br, s"($test)"),
          th(style := "width:20px;"),
          th("Comments", style := "vertical-align: top")
        )
      ),
      tbody(
        for ((part, ns, attrs) <- data) yield {
          tr(
            td(part),
            td(),
            td(ns),
            td(),
            td(
              attrs.map {
                case (attr, _, _, true, _)  => div(attr)
                case (attr, _, _, false, _) => div(style := "font-weight: bold; color:red;", attr)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, baseCard, _, true, _)  => div(baseCard)
                case (_, baseCard, _, false, _) => div(style := "font-weight: bold; color:red;", baseCard)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, _, testCard, true, _)  => div(testCard)
                case (_, _, testCard, false, _) => div(style := "font-weight: bold; color:red;", testCard)
              }
            ),
            td(),
            td(attrs.map(a => div(style := "white-space: nowrap", a._5, br))) // comment
          )
        }
      )
    )
  )


  def errorAttrType(data: Seq[(String, String, Seq[(String, String, String, Boolean, String)])], base: String, test: String) = li(
    p("Attribute types out of sync:", color.red, fontWeight.bold),
    table(cls := "table", style := "width: auto")(
      thead(
        tr(
          th("Partition", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Namespace", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Attribute", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Type", br, s"($base)"),
          th(style := "width:30px;"),
          th("Type", br, s"($test)"),
          th(style := "width:20px;"),
          th("Comments", style := "vertical-align: top")
        )
      ),
      tbody(
        for ((part, ns, attrs) <- data) yield {
          tr(
            td(part),
            td(),
            td(ns),
            td(),
            td(
              attrs.map {
                case (attr, _, _, true, _)  => div(attr)
                case (attr, _, _, false, _) => div(style := "font-weight: bold; color:red;", attr)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, baseTpe, _, true, _)  => div(baseTpe)
                case (_, baseTpe, _, false, _) => div(style := "font-weight: bold; color:red;", baseTpe)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, _, testTpe, true, _)  => div(testTpe)
                case (_, _, testTpe, false, _) => div(style := "font-weight: bold; color:red;", testTpe)
              }
            ),
            td(),
            td(attrs.map(a => div(style := "white-space: nowrap", a._5, br))) // comment
          )
        }
      )
    )
  )


  def errorAttrRef(data: Seq[(String, String, Seq[(String, String, String, String, Boolean)])]) = li(
    p("Reference namespaces out of sync:", color.red, fontWeight.bold),
    table(cls := "table", style := "width: auto")(
      thead(
        tr(
          th("Partition", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Namespace", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Attribute", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Type", style := "vertical-align: top"),
          th(style := "width:20px;"),
          th("Ref ns", br, "(definition file)"),
          th(style := "width:30px;"),
          th("Ref ns", br, "(meta database)")
        )
      ),
      tbody(
        for ((part, ns, attrs) <- data) yield {
          tr(
            td(part),
            td(),
            td(ns),
            td(),
            td(
              attrs.map {
                case (attr, _, _, _, true)  => div(attr)
                case (attr, _, _, _, false) => div(color.red, fontWeight.bold, attr)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, tpe, _, _, true)  => div(tpe)
                case (_, tpe, _, _, false) => div(color.red, fontWeight.bold, tpe)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, _, baseRef, _, true)  => div(baseRef, br)
                case (_, _, baseRef, _, false) => div(color.red, fontWeight.bold, baseRef)
              }
            ),
            td(),
            td(
              attrs.map {
                case (_, _, _, defRef, true)  => div(defRef, br)
                case (_, _, _, defRef, false) => div(color.red, fontWeight.bold, defRef)
              }
            )
          )
        }
      )
    )
  )

  def render = Rx {
    div(
      marginTop := 20,
      marginLeft := 40,
      "Schemas need to be in sync between",
      ul(
        li("Live Datomic database"),
        li("Schema definition file"),
        li("Molecule Admin"),
      ),
      p(),
      Rx(
        table(
          tr(
            td(
              button(
                "Check sync",
                //          if (syncing() ) i(cls := "oi oi-delete oi-spin", color := "#de5454") else sync()),
                onclick := { () =>
                  syncing() = false // force re-calculation to initiate sync
                  fetching() = true

                  // Use fresh schemas
                  schemaWire().getSchemas(db).call().foreach { case (liveSchema0, defSchema0, moleculeAdminSchema0, _) =>
                    liveSchema() = liveSchema0
                    defSchema() = defSchema0
                    metaSchema() = moleculeAdminSchema0
                    syncing() = true
                    fetching() = false
                  }
                }
              )
            ),
            td(
              if (fetching()) span(whiteSpace.nowrap, span(paddingRight := 0, _sync(15)), "(might be re-compiling schema def file...)") else div()
            )
          )
        )
      ),
      p(),
      Rx(
        if (syncing()) {
          val (live, deff, meta) = ("live database", "definition file", "meta database")
          val syncDef            = Sync(liveSchema.now, defSchema.now)
          val syncMeta           = Sync(defSchema.now, metaSchema.now)
          div(
            ul(
              li("Checking definition file against Live database...")
            )(
              syncDef.part match {
                case Some((liveParts, defParts)) => errorPart(liveParts, defParts, live, deff)
                case None                        => Seq(li("Partitions OK", style := "color: #009e00")) ++ (

                  syncDef.ns match {
                    case errData if errData.nonEmpty => Seq(errorNs(errData, live, deff))
                    case Nil                         => Seq(li("Namespaces OK", style := "color: #009e00")) ++ (

                      syncDef.attr match {
                        case errData if errData.nonEmpty => Seq(errorAttr(errData, live, deff))
                        case Nil                         => Seq(li("Attribute names OK", style := "color: #009e00")) ++ (

                          syncDef.attrCard match {
                            case errData if errData.nonEmpty => Seq(errorAttrCard(errData, live, deff))
                            case Nil                         => Seq(li("Attribute cardinalities OK", style := "color: #009e00")) ++ (

                              syncDef.attrType match {
                                case errData if errData.nonEmpty => Seq(errorAttrType(errData, live, deff))
                                case Nil                         => Seq(li("Attribute types OK", style := "color: #009e00")) ++ Seq(

                                  li("Definition file in sync with live database", style := "color: #009e00; font-weight: bold;"),
                                  li(" "),
                                  li("Checking meta database against definition file...")
                                ) ++ (

                                  syncMeta.part match {
                                    case Some((defParts, metaParts)) => Seq(errorPart(defParts, metaParts, deff, meta))
                                    case None                        => Seq(li("Partitions OK", style := "color: #009e00")) ++ (

                                      syncMeta.ns match {
                                        case errData if errData.nonEmpty => Seq(errorNs(errData, deff, meta))
                                        case Nil                         => Seq(li("Namespaces OK", style := "color: #009e00")) ++ (

                                          syncMeta.attr match {
                                            case errData if errData.nonEmpty => Seq(errorAttr(errData, deff, meta))
                                            case Nil                         => Seq(li("Attribute names OK", style := "color: #009e00")) ++ (

                                              syncMeta.attrCard match {
                                                case errData if errData.nonEmpty => Seq(errorAttrCard(errData, deff, meta))
                                                case Nil                         => Seq(li("Attribute cardinalities OK", style := "color: #009e00")) ++ (

                                                  syncMeta.attrType match {
                                                    case errData if errData.nonEmpty => Seq(errorAttrType(errData, deff, meta))
                                                    case Nil                         => Seq(li("Attribute types OK", style := "color: #009e00")) ++ (

                                                      syncMeta.metaAttrRef match {
                                                        case errData if errData.nonEmpty => Seq(errorAttrRef(errData))
                                                        case Nil                         => Seq(li("Reference namespaces OK", style := "color: #009e00")) ++ Seq(

                                                          li("Meta database in sync with definition file", style := "color: #009e00; font-weight: bold;")
                                                        )
                                                      })
                                                  })
                                              })
                                          })
                                      })
                                  })
                              })
                          })
                      })
                  })
              }
            )
          )
        } else div()
      )
    )
  }
}
