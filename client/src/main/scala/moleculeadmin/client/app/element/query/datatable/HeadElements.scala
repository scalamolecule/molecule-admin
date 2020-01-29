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
    textAlign.center,
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
    postfix: String,
    expr: String,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retract: MouseEvent => Unit,
    markers: Seq[MouseEvent => Unit] = Nil,
  ): TypedTag[UList] = {
    val items = if (attribute == "e") {
      Seq(
        a(href := "#", cls := "dropdown-item", "Star", onclick := markers.head),
        a(href := "#", cls := "dropdown-item", "Flag", onclick := markers(1)),
        a(href := "#", cls := "dropdown-item", "Check", onclick := markers(2)),
        div(cls := "dropdown-divider", margin := "3px 0"),
        a(href := "#", cls := "dropdown-item", "Unstar", onclick := markers(3)),
        a(href := "#", cls := "dropdown-item", "Unflag", onclick := markers(4)),
        a(href := "#", cls := "dropdown-item", "Uncheck", onclick := markers(5)),
        div(cls := "dropdown-divider", margin := "3px 0"),
        a(href := "#", cls := "dropdown-item", "Unstar all", onclick := markers(6)),
        a(href := "#", cls := "dropdown-item", "Unflag all", onclick := markers(7)),
        a(href := "#", cls := "dropdown-item", "Uncheck all", onclick := markers(8)),
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
        if (postfix.isEmpty) () else _pf(postfix),
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

  def _pf(postfix: String): Frag = span(postfix, cls := "postfix")

  def _attrHeaderSortable(
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
    retract: MouseEvent => Unit,
    markers: Seq[MouseEvent => Unit] = Nil,
  ): TypedTag[TableHeaderCell] = {
    val headerCell = {
      if (expr == "orig") {
        td(
          attribute,
          if (postfix.isEmpty) () else _pf(postfix)
        )
      } else if (expr == "edit") {
        td(
          padding := 0,
          attrMenu(attribute, postfix, expr, edit, save, cancel, retract),
          onchange := { () => processing() = "" }
        )
      } else if (nonMenuExprs.contains(expr)) {
        // non-editable aggr/tx
        td(
          attribute,
          if (postfix.isEmpty) () else _pf(postfix),
          span(cls := "expr", expr),
        )
      } else if (attribute == "e") {
        td(
          padding := 0,
          attrMenu(attribute, postfix, expr, edit, save, cancel, retract, markers)
        )
      } else if (editable) {
        td(
          padding := 0,
          attrMenu(attribute, postfix, expr, edit, save, cancel, retract)
        )
      } else {
        td(
          attribute,
          if (postfix.isEmpty) () else _pf(postfix),
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
      onclick := sort
    )
    th(
      table(
        cls := "neutral",
        tr(headerCell, sortCell)
      )
    )
  }

  def _attrHeader(
    attribute: String,
    postfix: String,
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
        if (postfix.isEmpty) () else _pf(postfix),
        noEdit
      )
    } else {
      th(attrMenu(attribute, postfix, expr, edit, save, cancel, retract))
    }
  }


  def _attrFilterCell(
    filterId: String,
    filterExpr: String,
    applyFilter: () => Unit
  )(implicit ctx: Ctx.Owner): TypedTag[TableHeaderCell] = {
    val htmlFilter: Seq[Frag] = if (filterExpr.contains("\n"))
      filterExpr.split("\n").toSeq.flatMap(s => Seq(StringFrag(s), br)).init
    else
      Seq(filterExpr)
    th(
      cls := "filter",
      id := filterId,
      contenteditable := true,
      htmlFilter,
      oninput := applyFilter
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
