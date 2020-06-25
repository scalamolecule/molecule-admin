package moleculeadmin.client.app.logic.query

import moleculeadmin.client.app.logic.query.keyEvents._
import moleculeadmin.shared.ast.schema.Ns
import moleculeadmin.shared.ops.query.{ColOps, MoleculeOps}
import org.scalajs.dom.document
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.raw.KeyboardEvent
import rx.Ctx
import moleculeadmin.client.app.logic.query.QueryState._


trait KeyEvents
  extends ColOps
    with MoleculeOps
    with Paging
    with MarkerToggling
    with QueryBuilding
    with SubMenuToggling
    with Inserting
    with Undoing
    with Editing {

  def registerKeyEvents(implicit ctx: Ctx.Owner, nsMap: Map[String, Ns]): Unit = {
    document.onkeydown = { e: KeyboardEvent =>
      val shift = e.getModifierState("Shift")
      val ctrl  = e.getModifierState("Control")
      val alt   = e.getModifierState("Alt")
      val cmd   = e.getModifierState("Meta")
      val mod   = shift || ctrl || alt || cmd

      if (document.activeElement == document.body) {
        // Browsing ..........................
        if (!mod) {
          e.key match {
            case "Escape"   => toggleOffAll()
            case "l"        => toggleQueryListMenu()
            case "n"        => addInsertNewDataRow0(e)
            case "u"        => toggleUndo()
            case "v"        => toggleViewsMenu()
            case "g"        => toggleGroupedMenu()
            case "q"        => toggleQueryBuilder
            case "d"        => toggle("tableData")
            case "s"        => if (e.repeat) toggling = true else toggleStar()
            case "f"        => if (e.repeat) toggling = true else toggleFlag()
            case "c"        => if (e.repeat) toggling = true else toggleCheck()
            case "PageUp"   => prevPage
            case "PageDown" => nextPage
            case "Home"     => firstPage
            case "End"      => lastPage

            case k if queryListOpen    => queryList(e, k)
            case k if groupedOpen      => grouped(e, k)
            case k if viewsOpen        => views(e, k)
            case k if queryBuilderOpen => queryBuilder(k)
            case " "                   => noScrollToBottom(e)
            case _                     => ()
          }
        } else if (shift) {
          e.key match {
            case "?" => toggleShortcutsMenu()
            case _   => ()
          }
        } else {
          e.key match {
            case "z" if cmd => undoLastClean
            case _          => paging(e, ctrl, alt, cmd)
          }
        }

      } else if (document.activeElement.isInstanceOf[TableCell]) {
        // Editing ..........................
        e.key match {
          case x if insertMode                   => x match {
            case "Escape"         => abortInsert()
            case "Enter" if shift => multilineSoftNewLine(e)
            case "Enter" if ctrl  => multilineAddItem(e)
            case "Enter"          => insertNewRow(e)
            case "Tab"            => continueInserting()
            case _                => ()
          }
          case "Escape"                          => blur()
          case "ArrowUp" if ctrl && alt && cmd   => first(e)
          case "ArrowUp" if ctrl && alt          => pageUp(e)
          case "ArrowUp" if ctrl                 => cellUp(e)
          case "ArrowDown" if ctrl && alt && cmd => last(e)
          case "ArrowDown" if ctrl && alt        => pageDown(e)
          case "ArrowDown" if ctrl               => cellDown(e)
          case "ArrowLeft" if ctrl && alt        => startOfRow(e)
          case "ArrowLeft" if ctrl               => cellLeft(e)
          case "ArrowRight" if ctrl && alt       => endOfRow(e)
          case "ArrowRight" if ctrl              => cellRight(e)
          case "PageUp"                          => pageUp(e)
          case "PageDown"                        => pageDown(e)
          case "Home"                            => first(e)
          case "End"                             => last(e)
          case "Backspace"                       => deleteItem(e)
          case "Enter" if shift                  => multilineSoftNewLine(e)
          case "Enter" if ctrl                   => multilineAddItem(e)
          case "Enter"                           => cellDown(e)
          case "Tab" if shift                    => cellLeft(e)
          case "Tab"                             => cellRight(e)
          case "z" if cmd                        => undoLastClean
          case _                                 => ()
        }
      }
    }

    document.onkeyup = { e: KeyboardEvent =>
      if (toggling && document.activeElement == document.body) {
        e.key match {
          case "s" | "f" | "c" => toggling = false
          case _               =>
        }
      }
    }
  }
}
