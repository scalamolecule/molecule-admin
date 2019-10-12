package moleculeadmin.client.app.domain.query
import autowire._
import boopickle.Default._
import moleculeadmin.client.app.element.query.SnippetElements
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.autowire.queryWire
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.schema
import moleculeadmin.shared.lib.molecule
import moleculeadmin.shared.lib.molecule.ast.model.Model
import moleculeadmin.shared.lib.molecule.ops.QueryOps._
import moleculeadmin.shared.lib.molecule.transform.{Model2Query, Query2String}
import moleculeadmin.shared.ops.query.ModelOps
import moleculeadmin.shared.ops.query.builder.TreeOps
import org.scalajs.dom.document
import org.scalajs.dom.html.{Element, TableCell, TableRow}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{td, tr, _}
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global


case class SnippetRender(db: String)(implicit val ctx: Ctx.Owner)
  extends Callbacks(db) with RxBindings with SnippetElements with ModelOps with TreeOps {
  type keepBooPickleImport2 = PickleState


  def moleculeSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    val lines      = curMolecule.now.split("\n")
    val rows       = lines.length + 2
    val cols       = lines.map(_.length).max + 25
    val alreadyFav = favorites().exists(_.molecule == curMolecule.now)
    _moleculeSnippet(rows, cols, curMolecule.now,
      addFavCallback(curMolecule.now), alreadyFav)
  }

  def favoritesSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    _favoritesSnippet(
      favorites().sortBy(_.molecule),
      curMolecule.now,
      useFavCallback,
      retractFavCallback
    )
  }

  def cacheSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    // Clear query cache when max rows is changed
    maxRows()
    _cacheSnippet(
      queryCache().map(_.molecule).sorted,
      curMolecule.now,
      favorites().map(_.molecule),
      resetCacheCallback,
      useCachedCallback,
      addFavCallback,
      removeCachedCallback
    )
  }


  def datalogSnippet: TypedTag[Element] = {
    if (emptyNamespaces(modelElements.now).nonEmpty) {
      div(
        "To render Datalog query, please select attr/ref in empty namespaces:",
        ul(for (ns <- emptyNamespaces(modelElements.now)) yield li(ns))
      )
    } else {
      val q = molecule.transform.Model2Query(Model(modelElements.now))._1
      _codeSnippet(
        "Datalog",
        "",
        molecule.transform.Query2String(q).multiLine(50),
        if (q.i.rules.nonEmpty) {
          div(
            p(),
            q.i.rules.map(Query2String(q).p(_)).mkString("RULES:\n[", "\n ", "]")
          )
        } else (),
        if (q.i.inputs.nonEmpty) {
          div(
            p(),
            q.inputs.zipWithIndex.map(r => (r._2 + 1) + "  " + r._1)
              .mkString("INPUTS:\n", "\n", "")
          )
        } else ()
      )
    }
  }


  private def getAttrCell(attr: String,
                          cellType: String,
                          valueCellId: String,
                          unexpandedValueCell: TypedTag[TableCell],
                          txs: Boolean
                         ): TypedTag[TableCell] = {
    (if (txs) td() else th()) (
      attr,
      if (curAttrs.contains(attr)) cls := "selectedAttr" else (),
      cellType match {
        case "ref" | "refSet" =>
          onmouseover := { () =>
            val expandedValueCell = document.getElementById(valueCellId)
            expandedValueCell.parentNode
              .replaceChild(unexpandedValueCell.render, expandedValueCell)
          }
        case _                => ()
      }
    )
  }

  private def getValueCell(cellType: String,
                           valueCellId: String,
                           v: String,
                           txs: Boolean,
                           level: Int,
                           asserted: Boolean = true
                          ): TypedTag[TableCell] = cellType match {
    case "str" => td(
      if (asserted) () else cls := "retracted",
      _str2frags(v))

    case "num" => td(
      cls := (if (asserted) "num" else "num retracted"),
      v
    )

    case "date" => td(
      if (asserted) () else cls := "retracted",
      truncateDateStr(v)
    )

    case "ref" => td(
      cls := (if (asserted) "eid" else "eid retracted"),
      id := valueCellId,
      a(href := "#", v,
        onmouseover := { () =>
          // Recursively open entity
          addEntityRows(valueCellId, v.toLong, txs, level + 1)
        })
    )

    case "enum" => td(
      if (asserted) () else cls := "retracted",
      v.split('/')(1)
    )

    case "other" => td(
      if (asserted) () else cls := "retracted",
      v
    )

    case "strSet" => td(
      if (asserted) () else cls := "retracted",
      //          if (txs && level == 0) _str2frags(v) else
      expandingList(
        v.split("__~~__").toSeq.sorted.map(s => li(_str2frags(s))), true))

    case "numSet" => td(
      cls := (if (asserted) "num" else "num retracted"),
      //          if (txs && level == 0) v else
      expandingList(
        v.split("__~~__").toSeq.map(_.toDouble).sorted.map(n => li(n))))

    case "refSet" => {
      val valueElement = if (txs && level == 0) {
        // Separate row for each value returned from tx lookup
        a(href := "#", v, onmouseover := { () =>
          // Recursively open entity
          addEntityRows(valueCellId, v.toLong, txs, level + 1)
        })
      } else {
        table(
          v.split("__~~__").toSeq.zipWithIndex.map {
            case (eid, i) =>
              val eidElementId = valueCellId + "-" + i
              tr(td(id := eidElementId, cls := "eid",
                a(href := "#", eid, onmouseover := { () =>
                  addEntityRows(eidElementId, eid.toLong, txs, level + 1)
                })))
          }
        )
      }
      td(
        id := valueCellId,
        cls := (if (asserted) "eid" else "retracted"),
        valueElement
      )
    }

    case "dateSet" => td(
      if (asserted) () else cls := "retracted",
      expandingList(
        v.split("__~~__").toSeq.sorted.map(s => li(truncateDateStr(s))))
    )

    case "enumSet" =>
      if (txs && level == 0)
        td(v.split('/')(1))
      else
        td(
          v.split("__~~__").toSeq.sorted
            .flatMap(enumAttr => Seq(span(enumAttr.split('/')(1)), br)).init)

    case "otherSet" => // Boolean, UUID, URI
      if (txs && level == 0)
        td(v)
      else
        td(expandingList(
          v.split("__~~__").toSeq.sorted.map(s => li(s))))

    case map if map.endsWith("Map") =>
      if (txs && level == 0) {
        val List(k, v1) = v.split("@", 2).toList
        td(
          if (asserted) () else cls := "retracted",
          table(cls := "mapPairs",
            cellType match {
              case "dateMap" => mapRow(k, td(truncateDateStr(v1)))
              case "strMap"  => mapRow(k, td(_str2frags(v1)))
              case _         => mapRow(k, td(v1))
            }
          )
        )
      } else {
        val rawPairs = v.split("__~~__").toSeq
          .map { pair =>
            val List(k, v) = pair.split("@", 2).toList
            (k, v)
          }.sortBy(_._1)

        cellType match {
          case "strMap" => mapCell(rawPairs,
            (v1: String) => td(_str2frags(v1)), asserted)

          case "dateMap" => mapCell(rawPairs,
            (v1: String) => td(truncateDateStr(v1)), asserted)

          case _ => mapCell(rawPairs,
            (v1: String) => td(v1), asserted)

        }
      }

    case _ => td(v)
  }


  def addTxRows(parentElementId: String, tx: Long, level: Int): Unit = {
    val snippetElement = document.getElementById(parentElementId)
    if (snippetElement != null) {
      queryWire().getTxData(db, tx, enumAttrs).call().foreach { data =>
        snippetElement.innerHTML = ""
        var i        = 0
        var ePrev    = 0L
        var eCur     = 0L
        var eCount   = 0
        var attrPrev = ""
        data.foreach { case (e, attr, v, asserted) =>
          i += 1
          if (e != ePrev) {
            eCur = e
            eCount += 1
          }
          val cellType   = snippetCellTypes(attr)
          val vElementId = parentElementId + attr + v.take(20)

          val entityCell = if (i == 1)
            td(s"${curT.now} / $e", cls := "txChosen")
          else
            th(e, cls := Rx(if (e == curEntity()) "eidChosen" else "eid"),
              onmouseover := { () => curEntity() = e })

          val valueCell = getValueCell(cellType, vElementId, v, true, level, asserted)
          val attr1     = if (attrPrev != attr || ePrev != e) attr else ""
          val attrCell  = getAttrCell(attr1, cellType, vElementId, valueCell, true)
          snippetElement.appendChild(
            tr(
              if (i == 1)
                cls := "first"
              else if (i > 1 && eCount % 2 == 1)
                cls := "even"
              else
                (),
              if (e != ePrev) entityCell else td(),
              attrCell,
              valueCell
              //              valueCell(if (asserted) () else cls := "retracted")
            ).render
          )

          ePrev = e
          attrPrev = attr
        }
      }
    }
  }

  def txSnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    curTx() match {
      case 0                         => // no entity id marked yet
      case tx if showTransaction.now =>
        val snippet = document.getElementById("txSnippetTable")
        if (snippet == null) {
          // Start fresh
          curTx() = 0
        } else {
          addTxRows("txSnippetTable", tx, 0)
        }
      case _                         => // don't update non-present txSnippet
    }
    _txSnippet("Point on tx id...")
  }

  def addEntityRows(parentElementId: String, eid: Long, txs: Boolean, level: Int): Unit = {
    val snippetElement = document.getElementById(parentElementId)
    if (snippetElement != null) {
      queryWire().touchEntity(db, eid).call().foreach { data =>
        snippetElement.innerHTML = ""
        data.foreach { case (attr, v) =>
          val cellType   = snippetCellTypes(attr)
          val vElementId = parentElementId + attr + level
          val valueCell  = getValueCell(cellType, vElementId, v, txs, level)
          val attrCell   = getAttrCell(attr, cellType, vElementId, valueCell, txs)
          snippetElement.appendChild(
            tr(
              attrCell,
              valueCell
            ).render
          )
        }
      }
    }
  }

  def entitySnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    curEntity() match {
      case 0                     => // no entity id marked yet
      case eid if showEntity.now =>
        val snippet = document.getElementById("entitySnippetTable")
        if (snippet == null) {
          // Start fresh
          curEntity() = 0
        } else {
          addEntityRows("entitySnippetTable", eid, false, 0)
        }
      case _                     => // don't update non-present entitySnippet
    }
    _entitySnippet("Point on entity id...")
  }


  def addEntityHistoryRows(parentElementId: String, eid: Long, sort: String): Unit = {
    val snippetElement = document.getElementById(parentElementId)
    if (snippetElement != null) {
      queryWire().getEntityHistory(db, eid, enumAttrs).call().foreach { data =>
        snippetElement.innerHTML = ""
        var i      = 0
        var txPrev = 0L

        if (sort == "tx") {
          var txCur   = 0L
          var txCount = 0
          data.sortBy(t => (t._1, t._5, t._4, t._6)).foreach {
            case (t, tx, txInstant, asserted, attr, v) => {
              i += 1
              if (tx != txPrev) {
                txCur = tx
                txCount += 1
              }
              val cellType   = snippetCellTypes(attr)
              val vElementId = parentElementId + " " + attr + " " + i

              val txCell    = td(
                s"$t / $tx",
                cls := Rx(if (tx == curTx()) "txChosen" else "tx"),
                onmouseover := { () =>
                  curT() = t
                  curTx() = tx
                  curTxInstant() = txInstant
                }
              )
              val valueCell = getValueCell(cellType, vElementId, v, true, 0, asserted)
              val attrCell  = getAttrCell(attr, cellType, vElementId, valueCell, true)
              snippetElement.appendChild(
                tr(
                  if (txCount % 2 == 0)
                    cls := "even"
                  else
                    (),
                  if (tx != txPrev) txCell else td(),
                  if (tx != txPrev) td(txInstant) else td(),
                  attrCell,
                  //                  valueCell
                  valueCell(if (asserted) () else cls := "retracted")

                ).render
              )
              txPrev = tx
            }
          }

        } else {
          var attrCur   = ""
          var attrCount = 0
          var attrPrev  = ""
          data.sortBy(t => (t._5, t._1, t._4, t._6)).foreach {
            case (t, tx, txInstant, asserted, attr, v) => {
              i += 1
              if (attr != attrPrev) {
                attrCur = attr
                attrCount += 1
              }
              val cellType   = snippetCellTypes(attr)
              val vElementId = parentElementId + " " + attr + " " + i
              val txCell     = td(
                s"$t / $tx",
                cls := Rx(if (tx == curTx()) "txChosen" else "tx"),
                onmouseover := { () =>
                  curT() = t
                  curTx() = tx
                  curTxInstant() = txInstant
                }
              )
              val valueCell  = getValueCell(cellType, vElementId, v, true, 0, asserted)
              val attrCell   = getAttrCell(attr, cellType, vElementId, valueCell, true)
              snippetElement.appendChild(
                tr(
                  if (attrCount % 2 == 0)
                    cls := "even"
                  else
                    (),
                  txCell,
                  td(txInstant),
                  attrCell(fontWeight.bold),
                  valueCell

                ).render
              )
              txPrev = tx
              attrPrev = attr
            }
          }
        }
      }
    }
  }


  def entityHistorySnippet: Rx.Dynamic[TypedTag[Element]] = Rx {
    (curEntity(), entityHistorySort()) match {
      case (0, _)                               => // no entity id marked yet
      case (eid, sort) if showEntityHistory.now =>
        val snippet = document.getElementById("entityHistoryEid")
        if (snippet == null) {
          // Start fresh
          curEntity() = 0
        } else {
          snippet.innerHTML = ""

          val byTx = if (sort == "tx")
            span("tx")
          else
            a(href := "#", "tx", onclick := { () =>
              entityHistorySort() = "tx"
            })

          val byAttr = if (sort == "attr")
            span("attr", paddingRight := 20)
          else
            a(href := "#", "attr", paddingRight := 20, onclick := { () =>
              entityHistorySort() = "attr"
            })

          snippet.appendChild(
            span(byTx, " | ", byAttr, eid.toString).render
          )
          addEntityHistoryRows("entityHistorySnippetTable", eid, sort)
        }
      case _                                    => // don't update non-present entitySnippet
    }
    _entityHistorySnippet("Point on entity id...")
  }

  def modelSnippet: TypedTag[Element] =
    _codeSnippet("Molecule Model", "scala", Model(modelElements.now).toString())

  def querySnippet: TypedTag[Element] =
    if (emptyNamespaces(modelElements.now).nonEmpty)
      div(
        "To render Molecule Query, please select attr/ref in empty namespaces:",
        ul(for (ns <- emptyNamespaces(modelElements.now)) yield li(ns))
      )
    else
      _codeSnippet("Molecule Query", "scala",
        Model2Query(Model(modelElements.now))._1.toString)

  def columnsSnippet = Rx {
    // Re-draw when columns change
    columns()
    _codeSnippet("Columns", "scala", columns.now.mkString("List(\n  ", ",\n  ", ")"), hljs)
  }

  def tree1Snippet: TypedTag[Element] =
    _codeSnippet("Tree with attr names only", "scala", tree.now.toString)
  def tree2Snippet: TypedTag[Element] =
    _codeSnippet("Tree with attr definitions", "scala", tree.now.code)
  def tree3Snippet: TypedTag[Element] =
    _codeSnippet("Full Tree", "scala", tree.now.code2)


  def rxElement: Rx.Dynamic[TypedTag[Element]] = Rx {
    //    println("SnippetRender...")
    if (modelElements().nonEmpty && showSnippets()) {
      _snippets(
        if (showMolecule()) moleculeSnippet else (),
        if (showFavorites()) favoritesSnippet else (),
        if (showCache()) cacheSnippet else (),
        if (showDatalog()) datalogSnippet else (),
        if (showTransaction()) txSnippet else (),
        if (showEntity()) entitySnippet else (),
        if (showEntityHistory()) entityHistorySnippet else (),

        if (showModel()) modelSnippet else (),
        if (showQuery()) querySnippet else (),
        if (showColumns()) columnsSnippet else (),
        if (showTree1()) tree1Snippet else (),
        if (showTree2()) tree2Snippet else (),
        if (showTree3()) tree3Snippet else ()
      )
    } else span()
  }
}
