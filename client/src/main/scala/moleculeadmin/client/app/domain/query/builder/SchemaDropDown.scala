package moleculeadmin.client.app.domain.query.builder
import moleculeadmin.client.app.element.query.SchemaDropdownElements
import moleculeadmin.client.app.domain.query.QueryState.{db, modelElements, newQueryBuildup}
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.schema._
import molecule.ast.model._
import moleculeadmin.shared.ops.query.SchemaOps
import org.scalajs.dom.html.UList
import rx.{Ctx, Rx}
import scalatags.JsDom
import scalatags.JsDom.all._


case class SchemaDropDown(schema: MetaSchema, selection: String)
                         (implicit val ctx: Ctx.Owner)
  extends RxBindings with SchemaOps with SchemaDropdownElements {

  def nsUls(nss: Seq[Ns], dropdownType: JsDom.TypedTag[UList]): JsDom.TypedTag[UList] = dropdownType(
    for (Ns(_, ns, nsFull, _, _, attrs0) <- nss) yield {
      // Add entity id
      val attrs = Attr(0, "e", 1, "datom", None, None, None, None, None, None, None, None, Nil) +: attrs0
      _submenu(
        a(href := "#", ns,
          onclick := { () =>
            Rx(modelElements() = Seq(Atom(nsFull, dummy, "", 0, NoValue)))
          }
        ),
        _menu(
          for (Attr(_, attr, car, attrType, enums, _, _, _, _, _, _, _, _) <- attrs) yield {
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

  val partitions = getFilteredSchema(schema, selection).parts

  def schemaWithPartitions: JsDom.TypedTag[UList] = _topMenu(
    for (Part(_, part, _, _, nss) <- partitions) yield {
      _submenu(
        a(href := "#", part),
        nsUls(nss, _menu)
      )
    }
  )

  def schemaWithoutPartitions: JsDom.TypedTag[UList] = nsUls(partitions.head.nss, _topMenu)


  def dynRender = Rx {
    //    println("SchemaDropDown...")
    if (schema.parts.isEmpty) {
      div(
        s"Couldn't find partitions for database `$db` in meta_Partitions. ", br,
        s"Has definition file path for database `$db` been saved (check on `Dbs` page)?"
      )
    } else if (partitions.isEmpty) {
      div(
        s"'$db' database has no data yet or it hasn't yet been registered in meta db.", br,
        s"Please generate a fresh value count in 'Schema' -> 'Value' -> 'Update value counts' (if the database is not empty).", br,
        s"Alternatively you can click the 'A' selector to show all available attributes of the database."
      )
    } else if (schema.parts.head.name != "db.part/user") {
      schemaWithPartitions
    } else {
      schemaWithoutPartitions
    }
  }
}