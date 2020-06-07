package moleculeadmin.client.app.html.query.datatable

import java.net.URI
import java.util.UUID
import util.client.rx.RxBindings
import moleculeadmin.client.app.logic.query.QueryState.groupEditId
import moleculeadmin.client.app.html.query.SchemaDropdownElements
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.styles.Color
import org.scalajs.dom
import org.scalajs.dom.html._
import org.scalajs.dom.raw
import org.scalajs.dom.{ClipboardEvent, Event, MouseEvent, document, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{span, _}
import scala.scalajs.js.timers.setTimeout
import scala.util.{Failure, Success, Try}


trait HeadElements extends ColOps
  with BodyElements
  with SchemaDropdownElements with RxBindings {

  def _openCloseQueryBuilder(closed: Boolean, onclck: () => Unit): TypedTag[Element] = td(
    cls := "header open-close",
    i(cls := "fas fa-angle-double-" + (if (closed) "right" else "left")),
    textAlign.center,
    onclick := onclck
  )

  def _rowNumberCell: TypedTag[TableCell] = td(
    cls := "header attr",
    "n",
    verticalAlign.middle,
    textAlign.center
  )

  private def checkValue(v: String, tpe: String): Try[Any] = {
    tpe match {
      case "String"       => Success(v)
      case "Int"          => Try(v.toInt)
      case "Long" | "ref" => Try(v.toLong)
      case "Float"        => Try(v.toFloat)
      case "Double"       => Try(v.toDouble)
      case "Boolean"      => Try(v.toBoolean)
      case "Date"         => Try(expandDateStr(v))
      case "UUID"         => Try(UUID.fromString(v))
      case "URI"          => Try(new URI(v))
      case "BigInt"       => Try(v.matches("\\d+"))
      case "BigDecimal"   => Try(v.matches("\\d+(\\.\\d+)?"))
    }
  }

  type NsData = (String, String, Int, String, Seq[(String, String, Boolean, String)])
  type JoinMaker = (String, String, Int, String, String, String, Boolean, String) => Unit

  private def refMenu(
    joinAttrs: Seq[NsData],
    joinMaker: JoinMaker = null
  ): Seq[TypedTag[Element]] = {
    if (joinAttrs.isEmpty) {
      Nil
    } else {
      val nss = joinAttrs.map {
        case (nsFull, refAttr, refCard, refNs, attrs) =>
          div(
            cls := "dropdown-submenu",
            a(href := "#", cls := "dropdown-item", s"$refAttr ($refNs)"),
            _menu(
              paddingTop := 10,
              attrs.map { case (attrName, attrType, isEnum, opt) =>
                val fullRefAttr     = s":$nsFull/$refAttr"
                val fullValueAttr   = s":$refNs/$attrName"
                val valueFrag: Frag =
                  if (opt == "uniqueIdentity") {
                    s"$attrName (unique identity)"
                  } else if (opt == "uniqueValue") {
                    s"$attrName (unique value)"
                  } else {
                    val joinInput = input(
                      tpe := "text",
                      marginLeft := 5,
                    ).render
                    form(
                      onsubmit := { () => false },
                      onchange := { () =>
                        val value = joinInput.value
                        // Basic client validation before submitting
                        checkValue(value, attrType) match {
                          case Success(_)         =>
                            println(s"Creating `$fullRefAttr` joins to attribute `$fullValueAttr` with value `$value`...")
                            joinMaker(nsFull, refAttr, refCard, refNs, attrName, attrType, isEnum, value)
                          case Failure(exception) =>
                            window.alert(s"Invalid input for attribute `$fullValueAttr`:\n" + exception)
                            joinInput.select()
                        }
                      },
                      attrName,
                      joinInput
                    )
                  }

                label(
                  cls := "dropdown-item",
                  marginBottom := 3,
                  textAlign.right,
                  valueFrag
                )
              }
            )
          )
      }
      Seq(
        div(cls := "dropdown-divider", margin := "3px 0"),
        div(cls := "dropdown-submenu",
          padding := "3px 0px",
          a(href := "#", cls := "dropdown-item", "Make joins to"),
          _menu(
            marginTop := "-25px",
            nss
          )
        )
      )
    }
  }

  def editExprItem(
    default: Boolean,
    editExprId: String,
    editExpr: String,
    retractEditExpr: () => Unit,
    pickEditExpr: () => Unit
  ): TypedTag[Anchor] = {
    a(
      href := "#",
      id := editExprId,
      cls := "dropdown-item",
      paddingTop := 5,
      if (default) () else _xRetract(retractEditExpr)(
        float.left,
        marginTop := 3,
        marginRight := -15,
        cursor.pointer
      ),
      pre(
        editExpr,
        display.`inline-block`,
        marginBottom := 0,
        marginLeft := 18
      ),
      onclick := pickEditExpr
    )
  }

  private def attrMenu(
    attribute: String,
    postfix: String,
    expr: String,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retractEntities: MouseEvent => Unit,
    retractValues: MouseEvent => Unit,
    togglers: Seq[MouseEvent => Unit] = Nil,
    joinAttrs: Seq[NsData] = Nil,
    joinMaker: JoinMaker = null,
    editDropdownId: String = "",
    editExprItems: List[Frag] = Nil,
  ): TypedTag[UList] = {
    val items = if (attribute == "e") {
      Seq(
        a(href := "#", cls := "dropdown-item", "Star", onclick := togglers.head),
        a(href := "#", cls := "dropdown-item", "Flag", onclick := togglers(1)),
        a(href := "#", cls := "dropdown-item", "Check", onclick := togglers(2)),
        div(cls := "dropdown-divider", margin := "3px 0"),
        a(href := "#", cls := "dropdown-item", "Unstar", onclick := togglers(3)),
        a(href := "#", cls := "dropdown-item", "Unflag", onclick := togglers(4)),
        a(href := "#", cls := "dropdown-item", "Uncheck", onclick := togglers(5)),
        div(cls := "dropdown-divider", margin := "3px 0"),
        a(href := "#", cls := "dropdown-item", "Unstar all", onclick := togglers(6)),
        a(href := "#", cls := "dropdown-item", "Unflag all", onclick := togglers(7)),
        a(href := "#", cls := "dropdown-item", "Uncheck all", onclick := togglers(8)),
      ) ++
        refMenu(joinAttrs, joinMaker) ++
        Seq(
          div(cls := "dropdown-divider", margin := "3px 0"),
          a(href := "#", cls := "dropdown-item", "Retract entities", onclick := retractEntities)
        )
    } else if (expr == "edit") {
      Seq(
        a(href := "#", cls := "dropdown-item", "Save", onclick := save),
        a(href := "#", cls := "dropdown-item", "Cancel edit", onclick := cancel),
        div(cls := "dropdown-divider", marginTop := 3, marginBottom := 6),
      ) ++ editExprItems
    } else {
      Seq(
        a(href := "#", cls := "dropdown-item", "Edit", onclick := edit),
        a(href := "#", cls := "dropdown-item", "Retract values", onclick := retractValues)
      )
    }

    ul(
      cls := "attr nav nav-pills",
      width := "100%",
      padding := 0,
      cursor.default,
      li(
        cls := "dropdown",
        width := "100%",
        padding := "1px 6px 1px 6px",
        attribute,
        if (postfix.isEmpty) () else _pf(postfix),
        if (expr.nonEmpty) span(cls := "expr", expr) else (),
        div(
          cls := "dropdown-menu",
          id := editDropdownId,
          minWidth := 80,
          padding := "3px 0px",
          marginTop := 0,
          marginLeft := -1,
          borderRadius := 0,
          items
        )
      )
    )
  }

  def _pf(postfix: String): Frag = span(postfix, cls := "postfix")

  def _attrHeaderSortable(
    syncId: String,
    attribute: String,
    postfix: String,
    expr: String,
    sortDir: String,
    sortPos: Int,
    sort: MouseEvent => Unit,
    editable: Boolean,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retractEntities: MouseEvent => Unit,
    retractValues: MouseEvent => Unit,
    togglers: Seq[MouseEvent => Unit],
    joinAttrs: Seq[NsData],
    joinMaker: JoinMaker,
    editDropdownId: String,
    editExprItems: List[Frag],
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val attrCell = {
      if (expr == "orig") {
        td(
          paddingTop := 1,
          paddingLeft := 6,
          attribute,
          if (postfix.isEmpty) () else _pf(postfix)
        )
      } else if (expr == "edit") {
        td(
          paddingTop := 2,
          paddingLeft := 6,
          padding := 0,
          attrMenu(
            attribute, postfix, expr, edit, save, cancel,
            retractEntities, retractValues,
            editDropdownId = editDropdownId,
            editExprItems = editExprItems,
          ),
        )
      } else if (nonMenuExprs.contains(expr)) {
        // non-editable aggr/tx
        td(
          paddingTop := 1,
          paddingLeft := 6,
          attribute,
          if (postfix.isEmpty) () else _pf(postfix),
          span(cls := "expr", expr),
        )
      } else if (attribute == "e") {
        td(
          padding := 0,
          attrMenu(
            attribute, postfix, expr, edit, save, cancel,
            retractEntities, retractValues, togglers, joinAttrs, joinMaker)
        )
      } else if (editable) {
        td(
          paddingTop := 2,
          paddingLeft := 6,
          padding := 0,
          attrMenu(
            attribute, postfix, expr, edit, save, cancel,
            retractEntities, retractValues)
        )
      } else {
        td(
          paddingTop := 1,
          paddingLeft := 6,
          attribute,
          if (postfix.isEmpty) () else _pf(postfix),
          span(cls := "expr", expr),
          cursor.pointer,
          onclick := { () =>
            window.alert(
              "Namespace must have an entity id `e` column first to allow editing."
            )
          }
        )
      }
    }

    val spinCell = td(
      width := "5%",
      Rx(if (groupEditId() == syncId) _sync() else span())
    )

    val sortCell = td(
      cursor.pointer,
      width := "25%",
      minWidth := 26,
      sortDir match {
        case "asc"  => _sortIcon("oi oi-caret-top", sortPos)
        case "desc" => _sortIcon("oi oi-caret-bottom", sortPos)
        case _      => span(
          cls := "oi oi-elevator",
          paddingRight := 6,
          float.right,
          color := "#bbbbbb"
        )
      },
      onclick := sort
    )

    td(
      cls := "header",
      table(
        width := "100%",
        tr(
          attrCell(width := "70%"),
          spinCell,
          sortCell
        )
      )
    )
  }

  def _sortIcon(clazz: String, sortPos: Int): TypedTag[Span] = span(
    float.right,
    paddingRight := 4,
    color := Color.icon,
    whiteSpace.nowrap,
    span(cls := clazz, verticalAlign.middle,
      paddingLeft := 0,
    ),
    if (sortPos == 0) () else span(sortPos)
  )

  def _attrHeader(
    attribute: String,
    postfix: String,
    expr: String,
    editable: Boolean,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retractEntities: MouseEvent => Unit,
    retractValues: MouseEvent => Unit,
    editDropdownId: String,
    editExprItems: List[Frag],
  ): TypedTag[TableCell] = {
    if (expr == "orig" || !editable) {
      td(
        cls := "header",
        verticalAlign.middle,
        paddingLeft := 6,
        paddingRight := 6,
        attribute,
        if (postfix.isEmpty) () else _pf(postfix)
      )
    } else {
      td(
        cls := "header",
        attrMenu(
          attribute, postfix, expr, edit, save, cancel,
          retractEntities, retractValues,
          editDropdownId = editDropdownId,
          editExprItems = editExprItems
        )
      )
    }
  }

  def _markFilterCell(filterCell: raw.Element, on: Boolean) = {
    val color = if (on) Color.filter else Color.white
    filterCell.setAttribute("style", s"background-color: $color")
  }

  def _attrFilterCell(
    editable: String,
    filterId: String,
    filterExpr: String,
    applyFilter: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val htmlFilter: Seq[Frag] = if (filterExpr.contains("\n"))
      filterExpr.split("\n").toSeq.flatMap(s => Seq(StringFrag(s), br)).init
    else
      Seq(filterExpr)

    val bgColor = if (filterExpr.nonEmpty) Color.filter else Color.white
    td(
      cls := "header input" + editable,
      id := filterId,
      contenteditable := true,
      backgroundColor := bgColor,
      htmlFilter,
      oninput := applyFilter
    )
  }

  def _attrEditCell(
    filterId: String,
    lambdaRaw: String,
    applyLambda: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableCell] = {
    val lambdaHtml: Seq[Frag] =
      lambdaRaw.split("\n").toSeq.flatMap(s => Seq(StringFrag(s), br)).init
    td(
      cls := "header input groupEdit",
      id := filterId,
      contenteditable := true,
      lambdaHtml,
      onblur := applyLambda,
      onpaste := { e: ClipboardEvent =>
        // Paste raw text and no html soup
        e.preventDefault()
        dom.document.getElementById(filterId).innerHTML =
          e.clipboardData.getData("text/plain").replace("\n", "<br>")
      },
    )
  }
}
