package moleculeadmin.client.app.element.query.datatable

import java.net.URI
import java.util.UUID
import moleculeadmin.client.app.domain.query.QueryState.processing
import moleculeadmin.client.app.element.query.SchemaDropdownElements
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ops.query.ColOps
import moleculeadmin.shared.styles.Color
import org.scalajs.dom.html._
import org.scalajs.dom.{MouseEvent, window}
import rx.{Ctx, Rx}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scala.util.{Failure, Success, Try}


trait HeadElements extends ColOps with SchemaDropdownElements with RxBindings {

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

  def _rowNumberCell2: TypedTag[TableCell] = td(
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

  //  def nsUls(nss: Seq[Ns]): Seq[TypedTag[Element]] = {
  //    nss.map(ns =>
  //      div(cls := "dropdown-submenu",
  //        a(href := "#", cls := "dropdown-item", ns.nameFull),
  //        _menu(
  //          ns.attrs.map(at => a(href := "#", cls := "dropdown-item", at.name))
  //        )
  //      )
  //    )
  //  }

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

  private def refMenu(
    joinAttrs: Seq[(String, String, String, Seq[(String, String, Boolean, String)])],
    joinMaker: (String, String, String, String, String, Boolean, String) => Unit = null
  ): Seq[TypedTag[Element]] = {
    if (joinAttrs.isEmpty) {
      Nil
    } else {
      val nss = joinAttrs.map {
        case (ns, refAttr, refNs, attrs) =>
          div(
            cls := "dropdown-submenu",
            a(href := "#", cls := "dropdown-item", s"$refAttr ($refNs)"),
            _menu(
              paddingTop := 10,
              attrs.map { case (attrName, attrType, isEnum, opt) =>
                val fullRefAttr = s":$ns/$refAttr"
                val fullValueAttr = s":$refNs/$attrName"
                val joinInput = input(
                  tpe := "text",
                  marginLeft := 5,
                ).render
                label(
                  cls := "dropdown-item",
                  marginBottom := 3,
                  textAlign.right,
                  form(
                    onsubmit := { () => false },
                    onchange := { () =>
                      if (opt == "uniqueIdentity") {
                        window.alert(s"Can't add joins to unique identity attribute `$fullValueAttr`")
                      } else if (opt == "uniqueValue") {
                        window.alert(s"Can't add joins to unique value attribute `$fullValueAttr`")
                      } else {
                        val value = joinInput.value
                        // Basic client validation before submitting
                        checkValue(value, attrType) match {
                          case Success(_)         =>
                            println(s"Creating `$fullRefAttr` joins to attribute `$fullValueAttr` with value `$value`...")
                            joinMaker(ns, refAttr, refNs, attrName, attrType, isEnum, value)
                          case Failure(exception) =>
                            window.alert(s"Invalid input for attribute `$fullValueAttr`:\n" + exception)
                            joinInput.select()
                        }
                      }
                    },
                    attrName,
                    joinInput
                  )
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

    // When composite queries are implemented in MoleculeAdmin we can reference
    // any attribute and can then take values directly from metaSchema:
    //    if (metaSchema.parts.head.name == "db.part/user") {
    //      metaSchema.parts.head match {
    //        case Part(_, _, _, _, nss) =>
    //          nsUls(nss)
    //      }
    //    } else {
    //      Nil
    //    }
  }

  private def attrMenu(
    attribute: String,
    postfix: String,
    expr: String,
    edit: MouseEvent => Unit,
    save: MouseEvent => Unit,
    cancel: MouseEvent => Unit,
    retract: MouseEvent => Unit,
    togglers: Seq[MouseEvent => Unit] = Nil,
    joinAttrs: Seq[(String, String, String, Seq[(String, String, Boolean, String)])] = Nil,
    joinMaker: (String, String, String, String, String, Boolean, String) => Unit = null
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
      ) ++ refMenu(joinAttrs, joinMaker)
    } else if (expr == "edit") {
      Seq(
        a(href := "#", cls := "dropdown-item", "Save", onclick := save),
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
    togglers: Seq[MouseEvent => Unit] = Nil,
    joinAttrs: Seq[(String, String, String, Seq[(String, String, Boolean, String)])] = Nil,
    joinMaker: (String, String, String, String, String, Boolean, String) => Unit = null
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
          attrMenu(attribute, postfix, expr, edit, save, cancel, retract,
            togglers, joinAttrs, joinMaker)
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
    val sortCell = td(
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
