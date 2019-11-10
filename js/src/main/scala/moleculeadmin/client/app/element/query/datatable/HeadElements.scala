package moleculeadmin.client.app.element.query.datatable
import moleculeadmin.client.app.domain.query.QueryState.processing
import moleculeadmin.client.app.element.AppElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html.{Element, Span, TableHeaderCell, UList}
import org.scalajs.dom.{MouseEvent, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{padding, _}


trait HeadElements extends ColOps with AppElements with RxBindings {

  def _openCloseQueryBuilder(closed: Boolean, onclck: () => Unit): TypedTag[Element] = th(
    cls := "hover",
    i(cls := "fas fa-angle-double-" + (if (closed) "right" else "left")),
    onclick := onclck
  )

  def _rowNumberCell: TypedTag[TableHeaderCell] = th(
    "n",
    verticalAlign.middle,
    textAlign.center
  )

  def _sortIcon(clazz: String, sortPos: Int): TypedTag[Span] = span(
    float.right,
    color := Color.icon,
    whiteSpace.nowrap,
    span(cls := clazz, verticalAlign.middle,
      paddingLeft := 0,
    ),
    if (sortPos == 0) () else span(sortPos)
  )

  private def attrMenu(
    attribute: String,
    card: Int,
    expr: String,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retract: MouseEvent => Unit,
  ): TypedTag[UList] = {
    val items = if (attribute == "e") {
      Seq(
        a(href := "#", cls := "dropdown-item", "Star"),
        a(href := "#", cls := "dropdown-item", "Flag"),
        a(href := "#", cls := "dropdown-item", "Tick"),
        div(cls := "dropdown-divider", margin := "3px 0"),
        a(href := "#", cls := "dropdown-item", "Unstar"),
        a(href := "#", cls := "dropdown-item", "Unflag"),
        a(href := "#", cls := "dropdown-item", "Untick"),
      )
    } else if (expr == "edit") {
      Seq(
        a(href := "#", cls := "dropdown-item", "Save", onclick := save),
        //        div(cls := "dropdown-divider", margin := "3px 0"),
        a(href := "#", cls := "dropdown-item", "Cancel edit", onclick := cancel),
      )
    } else {
      Seq(
        a(href := "#", cls := "dropdown-item", "Edit", onclick := edit),
        a(href := "#", cls := "dropdown-item", "Retract", onclick := retract)
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
        padding := "1px 6px 2px 6px",
        attribute,
        if (expr.nonEmpty) span(cls := "expr", expr) else (),
        div(
          cls := "dropdown-menu",
          id := "submenu-x",
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

  def _attrHeader(
    attribute: String,
    card: Int,
    expr: String,
    editable: Boolean,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retract: MouseEvent => Unit,
  ): TypedTag[TableHeaderCell] = {
    if (expr == "orig" || !editable) {
      th(
        verticalAlign.middle,
        paddingLeft := 6,
        paddingRight := 6,
        attribute,
        noEdit
      )
    } else {
      th(attrMenu(attribute, card, expr, edit, save, cancel, retract))
    }
  }

  def _attrHeaderSortable(
    attribute: String,
    card: Int,
    expr: String,
    sortDir: String,
    sortPos: Int,
    sortCallback: MouseEvent => Unit,
    editable: Boolean,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retract: MouseEvent => Unit,
  ): TypedTag[TableHeaderCell] = {
    val headerCell = {
      if (expr == "orig") {
        td(
          attribute
        )
      } else if (expr == "edit") {
        td(
          padding := 0,
          attrMenu(attribute, card, expr, edit, save, cancel, retract),
          //          br,
          //          span(
          //            paddingLeft := 6,
          //            //            a( href:="#", marginRight := 6, fontSize := "10px", cls := "badge badge-success", "0"),
          ////            a(color := "black", href := "#", marginRight := 6, fontSize := "10px", cls := "badge badge-secondary", "90"),
          //            a(color := "black", backgroundColor := "#d2d2d2", fontWeight.normal, href := "#", marginRight := 6, fontSize := "10px", cls := "badge badge-secondary", "90"),
          //            a(color := "black", backgroundColor := "#ffbb56", fontWeight.normal, href := "#", marginRight := 6, fontSize := "10px", cls := "badge badge-warning", "0"),
          //            a(color := "black", backgroundColor := "#ff7878", fontWeight.normal, href := "#", marginRight := 6, fontSize := "10px", cls := "badge badge-danger", "0"),
          //          )

          onchange := { () => processing() = "" }
        )
      } else if (nonMenuExprs.contains(expr)) {
        // non-editable aggr/tx
        td(
          attribute,
          span(cls := "expr", expr),
        )
      } else if (editable) {
        td(
          padding := 0,
          attrMenu(attribute, card, expr, edit, save, cancel, retract)
        )
      } else {
        // Potentially editable if entity id chosen
        td(
          attribute,
          span(cls := "expr", expr),
          cursor.pointer,
          onclick := { () =>
            window.alert("Namespace must have an entity id `e` column first to allow editing.")
          }
        )
      }
    }
    val sortCell   = td(
      cursor.pointer,
      sortDir match {
        case "asc"  => _sortIcon("oi oi-caret-top", sortPos)
        case "desc" => _sortIcon("oi oi-caret-bottom", sortPos)
        case _      => span(
          cls := "oi oi-elevator",
          paddingTop := 3,
          float.right,
          color := "#bbbbbb"
        )
      },
      onclick := sortCallback
    )
    th(
      table(
        cls := "neutral",
        tr(headerCell, sortCell)
      )
    )
  }


  def _attrFilterCell(
    filterId: String,
    rawFilter: String,
    doFilter: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableHeaderCell] = {
    val htmlFilter: Seq[Frag] = if (rawFilter.contains("\n"))
      rawFilter.split("\n").toSeq.flatMap(s => Seq(StringFrag(s), br)).init
    else
      Seq(rawFilter)
    th(
      cls := "filter",
      id := filterId,
      contenteditable := true,
      htmlFilter,
      oninput := doFilter
    )
  }

  def _attrLambdaCell(
    filterId: String,
    lambdaRaw: String,
    applyLambda: () => Unit,
    skipSpin: () => Unit,
  )(implicit ctx: Ctx.Owner): TypedTag[TableHeaderCell] = {
    val lambdaHtml: Seq[Frag] =
      lambdaRaw.split("\n").toSeq.flatMap(s => Seq(StringFrag(s), br)).init
    th(
      cls := "edit",
      id := filterId,
      contenteditable := true,
      lambdaHtml,
      onblur := applyLambda,
      onfocus := skipSpin,
      Rx(if (processing() == filterId) _sync(15) else span())
    )
  }

}
