package moleculeadmin.client.app.element.query
import moleculeadmin.client.jsdom.DropdownMenu
import moleculeadmin.client.app.element.AppElements
import scalatags.JsDom.all._
import org.scalajs.dom.{Element, html}
import org.scalajs.dom.html.{Div, Input, Select, Span}
import scalatags.JsDom
import moleculeadmin.shared.lib.molecule.ast.model.{Eq, Fn, Value}

trait AttrOptElements extends AppElements {

  def _doc(s: String) = div(s, marginTop := 5)


  def _hr = hr(
    marginTop := 10,
    marginBottom := 10,
  )

  def _radio(name1: String, id1: String, value1: String, label1: String, onclick1: () => Unit, checked1: Boolean = false): JsDom.TypedTag[Div] = div(
    cls := "form-check form-check-inline",
    marginBottom := 0,
    input(
      cls := "form-check-input position-static",
      tpe := "radio",
      name := s"$name1",
      id := s"$id1",
      marginLeft := 0,
      value := s"$value1",
      if (checked1) checked := true else (),
      onclick := onclick1
    ),
    label(cls := "form-check-label", `for` := s"$id1", s"$label1")
  )

  def _inputValue(id1: String, valueOrPlaceholder: scalatags.generic.AttrPair[Element, String]): JsDom.TypedTag[Input] = input(
    tpe := "text",
    cls := "form-control",
    id := id1,
    padding := "0rem 0.4rem",
    height := "initial",
    fontSize := "13px",
    size := 25,
    borderTopLeftRadius := "0",
    borderBottomLeftRadius := "0",
    valueOrPlaceholder
  )


  def _inputNum(label: String, id1: String, fn: String, attrValue: Value): Input = input(
    tpe := "text",
    id := id1,
    cls := "form-control",
    padding := "0rem 0.4rem",
    width := "38px",
    height := "initial",
    flex := "none",
    fontSize := "13px",
    attrValue match {
      case Fn(`fn`, Some(n)) => value := n.toString
      case _                 => placeholder := " n"
    }
  ).render


  def _aggrButton(labelStr: String, id1: String, input: Input, marked: String, onclick1: () => Unit): JsDom.TypedTag[Div] = div(
    cls := "input-group mb-3",
    div(
      cls := "input-group-prepend",
      display := "block",
      fontSize := "13px",
      label(
        `for` := id1,
        id := id1 + "-label",
        cls := "input-group-text" + marked,
        padding := "0rem 0.4rem",
        fontSize := "13px",
        onclick := onclick1,
        labelStr,
      )
    ),
    input
  )

  def _operandSelector(op: String, id1: String, attrType: String, options1: Seq[JsDom.TypedTag[html.Option]], onchange1: () => Unit) = select(
    cls := "custom-select prepend" + op,
    id := id1,
    padding := "0.175rem 1.75rem 0.175rem 0.75rem",
    width := (if (attrType == "String") "30%" else "20%"),
    height := "100%",
    flex := "none",
    fontSize := "13px",
    options1,
    onchange := onchange1
  ).render


  def _inputGroup(opId: String, valueId: String, inputValue: html.Input): JsDom.TypedTag[Div] = div(
    cls := "input-group",
    div(
      cls := "input-group-prepend",
      id := opId,
      label(
        "=",
        cls := "input-group-text",
        `for` := valueId,
        padding := "0.1rem 0.4rem",
        fontSize := "13px"
      )
    ),
    inputValue
  )

  def _inputGroupMb3(operandSelector: Select, inputValue: html.Input): JsDom.TypedTag[Div] = div(
    cls := "input-group mb-3",
    operandSelector,
    inputValue
  )

  def _topValueCheckBox(id1: String, attrValue: Value, value1: String): JsDom.TypedTag[Input] = input(
    tpe := "checkbox",
    cls := s"form-check-input",
    id := id1,
    value := value1,
    attrValue match {
      case Eq(vs) if vs.map(_.toString).contains(value1) => checked := true
      case _                                             => ()
    }
  )

  def _topValue(checkBox: JsDom.TypedTag[Input], value1: String, display1: String, count: String, onclick1: () => Unit): JsDom.TypedTag[Div] = div(
    cls := s"form-check",
    margin := 0,
    checkBox,
    label(
      cls := s"form-check-label bg",
      width := "100%",
      if (value1.trim.isEmpty) s""""$display1"""" else display1,
      span(
        count,
        float.right,
        marginLeft := "10px",
        color.gray, fontFamily := "courier",
        whiteSpace.nowrap
      ),
      onclick := onclick1
    )
  )

}
