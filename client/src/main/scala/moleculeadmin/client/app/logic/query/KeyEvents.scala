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
      val shift            = e.getModifierState("Shift")
      val ctrl             = e.getModifierState("Control")
      val alt              = e.getModifierState("Alt")
      val cmd              = e.getModifierState("Meta")
      val modifiersPressed = shift || ctrl || alt || cmd
      val repeat           = e.repeat

      if (document.activeElement == document.body) {
        // Browsing ..........................
        if (!modifiersPressed) {
          e.key match {
            case "PageDown" if repeat => throttle(e, () => prevPage, () => nextPage)
            case "PageUp" if repeat   => throttle(e, () => prevPage, () => nextPage)
            case "PageDown"           => nextPage
            case "PageUp"             => prevPage
            case "End"                => lastPage
            case "Home"               => firstPage

            case "Escape" => toggleOffAll()
            case "l"      => toggleQueryListMenu()
            case "n"      => addInsertNewDataRow0(e)
            case "u"      => toggleUndo()
            case "v"      => toggleViewsMenu()
            case "g"      => toggleGroupedMenu()
            case "q"      => toggleQueryBuilder
            case "d"      => toggle("tableData")
            case "e"      => toggleView(2) // Entity view

            case k if queryListOpen    => queryList(e, k)
            case k if groupedOpen      => grouped(e, k)
            case k if viewsOpen        => views(e, k)
            case k if queryBuilderOpen => queryBuilder(k)

            // keys 1-3 after view selectors by number
            case "s" | "1" => if (e.repeat) togglers(0) = true else toggleStar()
            case "f" | "2" => if (e.repeat) togglers(1) = true else toggleFlag()
            case "c" | "3" => if (e.repeat) togglers(2) = true else toggleCheck()

            case " " => noScrollToBottom(e)
            case _   => ()
          }
        } else if (shift) {
          e.key match {
            case "?" => toggleShortcutsMenu()
            case _   => ()
          }
        } else {
          // modifiers used
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
      if (
        document.activeElement == document.body &&
          (togglers(0) || togglers(1) || togglers(2))
      ) {
        e.key match {
          case "s" | "1" => togglers(0) = false
          case "f" | "2" => togglers(1) = false
          case "c" | "3" => togglers(2) = false
          case _         =>
        }
      }
    }
  }
}
