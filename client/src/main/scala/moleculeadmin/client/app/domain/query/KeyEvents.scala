package moleculeadmin.client.app.domain.query
import moleculeadmin.client.app.domain.query.keyEvents._
import moleculeadmin.shared.ast.schema.Ns
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import org.scalajs.dom.document
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.raw.KeyboardEvent
import rx.Ctx


trait KeyEvents
  extends ColOps with MoleculeOps
    with Paging
    with QueryBuilding
    with SubMenuToggling
    with Inserting
    with Editing {

  def registerKeyEvents(implicit ctx: Ctx.Owner, nsMap: Map[String, Ns]): Unit = {
    document.onkeydown = { e: KeyboardEvent =>
      val shift = e.getModifierState("Shift")
      val ctrl  = e.getModifierState("Control")
      val alt   = e.getModifierState("Alt")
      val cmd   = e.getModifierState("Meta")
      val mod   = shift || ctrl || alt || cmd

      if (document.activeElement == document.body) {
        if (!mod) {
          e.key match {
            case "Escape"              => toggleOffAll()
            case "l"                   => toggleQueryListMenu()
            case "n"                   => addInsertNewDataRow0(e)
            case "v"                   => toggleViewsMenu()
            case "g"                   => toggleGroupedMenu()
            case "q"                   => toggleQueryBuilder
            case k if queryListOpen    => queryList(e, k)
            case k if groupedOpen      => grouped(e, k)
            case k if viewsOpen        => views(e, k)
            case k if queryBuilderOpen => queryBuilder(k)
            case " "                   => noBottomScroll(e)
            case _                     => ()
          }
        } else if (shift) {
          e.key match {
            case "?" => toggleShortcutsMenu()
            case _   => ()
          }
        } else
          paging(e, ctrl, alt, cmd)

      } else if (document.activeElement.isInstanceOf[TableCell]) {
        e.key match {
          case x if insertMode     => x match {
            case "Escape"         => abortInsert()
            case "Enter" if shift => multilineSoftNewLine(e)
            case "Enter" if ctrl  => multilineAddItem(e)
            case "Enter"          => insertNewRow(e)
            case _                => ()
          }
          case "Escape"            => blur()
          case "ArrowUp" if ctrl   => cellUp()
          case "ArrowDown" if ctrl => cellDown()
          case "Backspace"         => deleteItem(e)
          case "Enter" if shift    => multilineSoftNewLine(e)
          case "Enter" if ctrl     => multilineAddItem(e)
          case "Enter"             => saveEdit(e)
          case _                   => ()
        }
      }
    }
  }
}
