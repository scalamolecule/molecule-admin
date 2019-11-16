package moleculeadmin.client.app.domain.query
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.shared.ast.schema.Ns
import org.scalajs.dom.html.{TableCell, UList}
import org.scalajs.dom.raw.{HTMLInputElement, KeyboardEvent}
import org.scalajs.dom.{Node, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.all._
import scala.scalajs.js.Date
import scala.scalajs.js.timers.setTimeout


trait KeyEvents {

  def toggle(id: String): Unit = {
    val el = document.getElementById("submenu-" + id)
    if (el != null) {
      val style = el.getAttribute("style")
      if (style.endsWith("display:block;")) {
        // make sure element is really turned off
        el.setAttribute("style", style.dropRight(14))
      } else {
        el.setAttribute("style", style + "display:block;")
      }
    }
  }

  def toggleOff(id: String): Unit = {
    val el = document.getElementById("submenu-" + id)
    if (el != null) {
      val style = el.getAttribute("style")
      if (style.endsWith("display:block;")) {
        // make sure element is really turned off
        el.setAttribute("style", style.dropRight(14))
      }
    }
  }

  def toggleCached(): Unit = toggle("cache")
  def toggleFavorites(): Unit = toggle("favorites")
  def toggleShortcuts(): Unit = toggle("shortcuts")

  def toggleQuery(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx(
    if (selection.now == "q") {
      if (minimized) {
        selection() = "m"
      } else {
        baseSelection match {
          case "a" => selection() = "a"
          case "v" => selection() = "v"
          case "r" => selection() = "r"
        }
      }
    } else {
      selection() = "q"
    }
  )

  def toggleMinimize(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] = Rx(
    if (selection.now == "m") {
      minimized = false
      baseSelection match {
        case "a" => selection() = "a"
        case "v" => selection() = "v"
        case "r" => selection() = "r"
      }
    } else {
      minimized = true
      selection() = "m"
    }
  )


  def toggleAttrSelectionA(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] =
    Rx {baseSelection = "a"; selection() = "a"}

  def toggleAttrSelectionR(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] =
    Rx {baseSelection = "r"; selection() = "r"}

  def toggleAttrSelectionV(implicit ctx: Ctx.Owner): Rx.Dynamic[Unit] =
    Rx {baseSelection = "v"; selection() = "v"}


  def actualRowCount: Int = if (filters.now.isEmpty)
    rowCount else cachedFilterIndex.length

  def isFirst: Boolean = offset.now == 0
  def isLast: Boolean = offset.now + limit.now >= actualRowCount
  def remainingRows: Int = actualRowCount - offset.now
  def curLastRow: Int = {
    val remainingRows1 = remainingRows
    offset.now + (if (remainingRows1 < limit.now) remainingRows1 else limit.now)
  }

  def chunkSize: Int = {
    val curRowCount1 = actualRowCount
    curRowCount1 / (if (curRowCount1 < 10000) 50 else 100) / limit.now
  }

  def hasChunkBefore: Boolean =
    chunkSize > 0 && offset.now > limit.now * chunkSize

  def hasChunkAfter: Boolean =
    chunkSize > 0 && remainingRows > limit.now * chunkSize


  def firstPage(implicit ctx: Ctx.Owner): Unit =
    if (!isFirst) offset() = 0

  def prevChunk(implicit ctx: Ctx.Owner): Unit =
    if (hasChunkBefore)
      offset() = offset.now - limit.now * chunkSize else firstPage

  def prevPage(implicit ctx: Ctx.Owner): Unit =
    if (!isFirst) offset() = offset.now - limit.now

  def nextPage(implicit ctx: Ctx.Owner): Unit =
    if (!isLast) offset() = offset.now + limit.now

  def nextChunk(implicit ctx: Ctx.Owner): Unit =
    if (hasChunkAfter)
      offset() = offset.now + limit.now * chunkSize else lastPage

  def lastPage(implicit ctx: Ctx.Owner): Unit =
    if (!isLast) offset() = {

      val curRowCount1 = actualRowCount
      curRowCount1 - curRowCount1 % limit.now
    }

  def toggleSnippets(implicit ctx: Ctx.Owner): Unit = {
    document.getElementById("checkbox-snippet-0")
      .asInstanceOf[HTMLInputElement].checked = !showSnippets.now
    showSnippets() = !showSnippets.now
  }

  def useFavorite(i: Int)
    (implicit ctx: Ctx.Owner, nsMap: Map[String, Ns]) =
    if (i < favorites.now.size) {
      new Callbacks("").useFavorite(favorites.now.sortBy(_.molecule).apply(i))
    } else {
      // Print soft error message to browser console
      window.alert(s"Only ${favorites.now.size} favorite molecules saved.")
    }

  def useCache(i: Int)(implicit ctx: Ctx.Owner): Unit =
    if (i < queryCache.now.size) {
      modelElements() = queryCache.now.sortBy(_.molecule).apply(i).modelElements
    } else {
      window.alert(s"Only ${queryCache.now.size} queries cached.")
    }


  def cacheOpen: Boolean = {
    val cachedElement = document.getElementById("submenu-cache")
    cachedElement != null &&
      cachedElement.getAttribute("style").endsWith("display:block;")
  }
  def favoritesOpen: Boolean = {
    val favoritesElement = document.getElementById("submenu-favorites")
    favoritesElement != null &&
      favoritesElement.getAttribute("style").endsWith("display:block;")
  }


  def registerKeyEvents(implicit ctx: Ctx.Owner, nsMap: Map[String, Ns]) = Rx {
    var firstNumber  = -1
    var secondNumber = -1
    val numberMs     = 300 // todo: setting?

    var beginning   = 0.0
    var i           = 0
    var j           = 0
    var t1          = 0.0
    var t2          = 0.0
    var delta       = 0.0
    var tProcess    = 0.0
    var keyRepeatMs = 0L
    var avg         = 0.0
    var ratio       = 0.0
    var throttle    = 3

    document.onkeydown = { e: KeyboardEvent =>
      val mod = Seq("Control", "Alt", "Meta", "Shift").exists(m => e.getModifierState(m))
      if (document.activeElement == document.body) {
        if (!mod) {
          e.key match {
            case "Escape"                          =>
              curEntity() = 0L
              toggleOff("favorites")
              toggleOff("cache")
              toggleOff("shortcuts")
            case "c"                               => toggleOff("favorites"); toggleOff("shortcuts"); toggleCached()
            case "f"                               => toggleOff("cache"); toggleOff("shortcuts"); toggleFavorites()
            case "s"                               => toggleSnippets
            case "q" if modelElements.now.nonEmpty => toggleQuery
            case "m" if modelElements.now.nonEmpty => toggleMinimize
            case "a" if selection.now != "a"       => baseSelection = "a"; selection() = "a"
            case "v" if selection.now != "v"       => baseSelection = "v"; selection() = "v"
            case "r" if selection.now != "r"       => baseSelection = "r"; selection() = "r"
            case n if favoritesOpen                => n match {
              case "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0" =>
                if (firstNumber == -1) {
                  firstNumber = n.toInt
                  setTimeout(numberMs) {
                    val index = if (secondNumber == -1)
                      firstNumber - 1
                    else
                      firstNumber * 10 + secondNumber - 1
                    if (index >= 0)
                      useFavorite(index)
                    firstNumber = -1
                    secondNumber = -1
                  }
                } else if (secondNumber == -1) {
                  secondNumber = n.toInt
                }
              case _                                                         => ()
            }
            case n if cacheOpen                    => n match {
              case "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0" =>
                // todo: refactor out redundant functionality for favorites/cached
                if (firstNumber == -1) {
                  firstNumber = n.toInt
                  setTimeout(numberMs) {
                    val index = if (secondNumber == -1)
                      firstNumber - 1
                    else
                      firstNumber * 10 + secondNumber - 1
                    if (index > 0)
                      useCache(index)
                    firstNumber = -1
                    secondNumber = -1
                  }
                } else if (secondNumber == -1) {
                  secondNumber = n.toInt
                }
              case _                                                         => ()
            }
            case _                                 => ()
            //            case other => println(other)
          }

        } else if (e.getModifierState("Shift")) {
          e.key match {
            case "?" =>
              toggleOff("favorites")
              toggleOff("cache");
              toggleShortcuts
            case _   => ()
          }

        } else if (
          e.getModifierState("Control") &&
            e.getModifierState("Alt") &&
            e.getModifierState("Meta")
        ) {
          e.key match {
            case "ArrowLeft"  => firstPage
            case "ArrowRight" => lastPage
            case _            => ()
          }

        } else if (e.getModifierState("Control") && e.getModifierState("Alt")) {
          e.key match {
            case "ArrowLeft"  => prevChunk
            case "ArrowRight" => nextChunk
            case _            => ()
          }

        } else if (e.getModifierState("Control")) {
          if (e.repeat) {

            // Throttle paging for smooth rendering

            if (beginning < 10000000) {
              beginning = new Date().getTime
            }

            i += 1
            if (keyRepeatMs == 0 && i == 3) {
              // Measure duration of 2 key repeats and apply a rough
              // factor of 40% for processing leaving 60% time to rendering
              keyRepeatMs = ((new Date().getTime - beginning) / 2 * 0.4).round
              println("keyRepeatInterval " + keyRepeatMs)
            }

            if (i % throttle == 0) {
              t1 = new Date().getTime
              e.key match {
                case "ArrowLeft"  => prevPage
                case "ArrowRight" =>
                  nextPage
                  j += 1
                case _            => ()
              }

              t2 = new Date().getTime
              delta = t2 - t1
              tProcess = tProcess + delta
              avg = (tProcess / j).round
              ratio = avg / keyRepeatMs
              throttle = ratio.ceil.toInt
                            println(s"  $j  $delta    $avg    $ratio    $throttle    " + (t2 - beginning))
            }
          } else {
            e.key match {
              case "ArrowLeft"  => prevPage
              case "ArrowRight" => nextPage
              case _            => ()
            }
          }
        }

      } else if (document.activeElement.isInstanceOf[TableCell]) {

        // Cell editing
        e.key match {
          case "Escape" =>
            document.activeElement.asInstanceOf[HTMLInputElement].blur()

          case "ArrowUp" if e.getModifierState("Control") =>
            val curCell = document.activeElement
            val curRow  = curCell.parentNode
            val prevRow = curRow.previousSibling
            if (prevRow != null) {
              val colNo     = curCell.id.substring(4, 6).trim.toInt
              val cellAbove = prevRow.childNodes.item(colNo)
              selectContent(cellAbove.asInstanceOf[HTMLInputElement])
            }

          case "ArrowDown" if e.getModifierState("Control") =>
            val curCell = document.activeElement
            val curRow  = curCell.parentNode
            val nextRow = curRow.nextSibling
            if (nextRow != null) {
              val colNo     = curCell.id.substring(4, 6).trim.toInt
              val cellBelow = nextRow.childNodes.item(colNo)
              selectContent(cellBelow.asInstanceOf[HTMLInputElement])
            }

          case "Backspace" =>
            val curCell = document.activeElement
            // Avoid deleting item code
            if (curCell.getAttribute("class") == "items") {
              // Find caret position in cell
              val range         = window.getSelection.getRangeAt(0)
              val preCaretRange = range.cloneRange()
              preCaretRange.selectNodeContents(curCell)
              preCaretRange.setEnd(range.endContainer, range.endOffset)
              val caretOffset = preCaretRange.toString.length
              if (caretOffset == 0) {
                // Prevent deleting first item
                // (would break code structure that we depend on)
                e.preventDefault()
              }
            }

          case "Enter" if e.getModifierState("Shift") =>
            val curCell = document.activeElement
            val card    = curCell.getAttribute("card")
            val cls     = curCell.getAttribute("class")
            if (!(
              card != null && (card == "2" || card == "3") ||
                cls != null && (cls == "str" || cls == "filter" || cls == "edit"))
            ) {
              // Prevent internal line shifts in non-String cells
              e.preventDefault()
              window.alert(
                """Shift-Return can only be used to create line shifts for:
                  |- Cardinality-many attributes
                  |- Cardinality-one String attributes
                  |""".stripMargin)
            }

          case "Enter" if e.getModifierState("Control") =>
            val curCell = document.activeElement
            // Create new empty String item (since we have multiline values)
            if (curCell.getAttribute("class") == "items") {
              // Prevent line shift
              e.preventDefault()
              // add list item
              val ul = curCell.firstChild
              ul.appendChild(li().render)
              // Set caret in new item
              val range = document.createRange
              val sel   = window.getSelection
              range.setStart(ul.childNodes.item(ul.childNodes.length - 1), 0)
              range.collapse(true)
              sel.removeAllRanges
              sel.addRange(range)
            }

          case "Enter" =>
            // prevent creating new line within cell
            e.preventDefault()
            val curCell = document.activeElement
            val curRow  = curCell.parentNode
            val nextRow = curRow.nextSibling
            editCellId = curCell.id
            if (nextRow != null) {
              val colNo     = curCell.id.substring(4, 6).trim.toInt
              val cellBelow = nextRow.childNodes.item(colNo)
              // Select content of cell below
              // Fires blur-callback (save) on current cell
              selectContent(cellBelow.asInstanceOf[HTMLInputElement])
              if (editCellId.isEmpty) {
                // Re-select content in original cell if invalid data
                selectContent(curCell)
              }
            } else {
              curCell.asInstanceOf[HTMLInputElement].blur()
            }
            editCellId = ""

          case other =>
          //            println("other key: " + other)
        }
      }
    }
  }

  def selectContent(elem: Node): Unit = {
    val range = document.createRange()
    range.selectNodeContents(elem);
    val sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(range);
  }
}
