package moleculeadmin.client.app.logic.query.builder

import util.client.rx.RxBindings
import moleculeadmin.client.app.html.query.SchemaDropdownElements
import moleculeadmin.client.app.logic.query.QueryState.{db, modelElements, newQueryBuildup}
import moleculeadmin.shared.ast.metaSchema._
import molecule.ast.model._
import moleculeadmin.shared.ops.query.SchemaOps
import org.scalajs.dom.html.UList
import org.scalajs.dom.raw.HTMLElement
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class SchemaDropdown(metaSchema: MetaSchema, selection: String)
                         (implicit val ctx: Ctx.Owner)
  extends RxBindings with SchemaOps with SchemaDropdownElements {

  def nsUls(
    nss: Seq[MetaNs],
    dropdownType: JsDom.TypedTag[UList]
  ): JsDom.TypedTag[UList] = dropdownType(
    for (MetaNs(_, ns, nsFull, _, _, attrs0) <- nss) yield {
      // Add entity id
      val attrs = MetaAttr(0, "e", 1, "datom", Nil, None, Nil, None, None, None, None, None, Nil) +: attrs0
      _submenu(
        a(href := "#", ns,
          onclick := { () =>
            Rx(modelElements() = Seq(Atom(nsFull, dummy, "", 0, NoValue)))
          }
        ),
        _menu(
          for (MetaAttr(_, attr, car, attrType, enums, _, _, _, _, _, _, _, _) <- attrs) yield {
            val manyAsterisk = if (car > 2) " **" else if (car == 2) " *" else ""
            li(
              a(href := "#",
                attr + manyAsterisk,
                onclick := { () =>
                  Rx {
                    newQueryBuildup = true
                    modelElements() = {
                      if (attr == "e")
                        Seq(Generic(nsFull, "e", "datom", EntValue))
                      else if (enums.nonEmpty)
                        Seq(Atom(nsFull, attr, attrType, car, EnumVal, Some(s":$nsFull.$attr/")))
                      else
                        Seq(Atom(nsFull, attr, attrType, car, VarValue))
                    }
                  }
                }
              )
            )
          }
        )
      )
    }
  )

  val partitions: Seq[MetaPart] = getFilteredSchema(metaSchema, selection).parts

  def schemaWithPartitions: JsDom.TypedTag[UList] = _topMenu(
    for (MetaPart(_, part, _, _, nss) <- partitions) yield {
      _submenu(
        a(href := "#", part),
        nsUls(nss, _menu)
      )
    }
  )

  def schemaWithoutPartitions: JsDom.TypedTag[UList] =
    nsUls(partitions.head.nss, _topMenu)


  def dynRender: Rx.Dynamic[JsDom.TypedTag[HTMLElement]] = Rx {
    if (metaSchema.parts.isEmpty) {
      div(
        s"Couldn't find partitions for database `$db` in meta_Partitions. ", br,
        s"Has definition file path for database `$db` been saved (check on `Dbs` page)?"
      )
    } else if (partitions.isEmpty) {
      div(
        s"'$db' database has no data yet or it hasn't yet been registered in meta db.", br,
        s"Please generate a fresh value count in 'Schema' -> 'Value' -> 'Update value counts' (if the database is not empty).", br,
        s"Alternatively you can click the 'a' selector to show all available attributes of the database."
      )
    } else if (metaSchema.parts.head.name != "db.part/user") {
      schemaWithPartitions(float.none)
    } else {
      schemaWithoutPartitions(float.none)
    }
  }
}