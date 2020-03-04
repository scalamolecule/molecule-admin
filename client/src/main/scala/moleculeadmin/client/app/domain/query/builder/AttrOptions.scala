package moleculeadmin.client.app.domain.query.builder

import molecule.ast.model._
import moleculeadmin.client.app.domain.query.QueryState._
import moleculeadmin.client.app.element.query.AttrOptElements
import moleculeadmin.client.jsdom.DropdownMenu
import moleculeadmin.client.rxstuff.RxBindings
import moleculeadmin.shared.ast.schema._
import moleculeadmin.shared.ops.query.attr.{AttrOps, ModeOps}
import org.scalajs.dom.html.{Div, Input, LI, Select}
import org.scalajs.dom.raw.{Event, HTMLElement}
import org.scalajs.dom.{MouseEvent, document, html, window}
import rx.{Ctx, Rx}
import scalatags.JsDom._
import scalatags.JsDom.all.{div, label, span, _}


case class AttrOptions(
  i: Int,
  refAttr: String,
  nsFull: String,
  attr: String,
  car: Int,
  attrType: String,
  attrValue: Value,
  enums: Option[Set[String]],
  options: Option[Set[String]],
  doc: Option[String],
  topValues: Seq[TopValue],
  selAttrs: Seq[GenericAtom],
  attrClass: String,
  path: Seq[(String, String)],
  manyAsterisk: String,
  additives: Seq[String]
)(implicit val ctx: Ctx.Owner)
  extends RxBindings
    with ModeOps with AttrOps with DropdownMenu with AttrOptElements {

  // Form validation flags
  var validInput = true
  var curFn      = ""

  val fulltext: Boolean = if (attrType == "String")
    options.getOrElse(Set.empty[String]).contains("fulltext") else false

  def id_(id: String): String = s"$path-$nsFull-$attr-$id-$i"


  def keepSettingsOpen: Unit = {
    val settings: HTMLElement =
      document.getElementById(id_("settings")).asInstanceOf[HTMLElement]
    settings.setAttribute("style", "display:block")
    settings.addEventListener("mouseleave", (_: MouseEvent) =>
      settings.setAttribute("style", "")
    )
  }

  // Inputs --------------------------------------------------------------------

  val inputValue: Input = _inputValue(id_("value"),
    attrValue match {
      case Fulltext(vs) => value := vs.map(_.toString).mkString(" || ")
      case Eq(vs)       => value := vs map renderValue mkString ", "
      case Neq(vs)      => value := vs map renderValue mkString ", "
      case Gt(v)        => value := renderValue(v)
      case Lt(v)        => value := renderValue(v)
      case Ge(v)        => value := renderValue(v)
      case Le(v)        => value := renderValue(v)
      case _            => placeholder :=
        s"        $attr: " + (if (attrType == "datom") "Long" else attrType)
    }
  ).render

  val inputValue2: Input = _inputValue(id_("value"),
    attrValue match {
      case Fulltext(vs) => value := vs.map(_.toString).mkString(" || ")
      case Eq(vs)       => value := vs map renderValue mkString ", "
      case Neq(vs)      => value := vs map renderValue mkString ", "
      case Gt(v)        => value := renderValue(v)
      case Lt(v)        => value := renderValue(v)
      case Ge(v)        => value := renderValue(v)
      case Le(v)        => value := renderValue(v)
      case _            => placeholder :=
        s"        $attr: " + (if (attrType == "datom") "Long" else attrType)
    }
  ).render

  val inputMin   : Input = _inputNum(id_("min"), "min", attrValue)
  val inputMax   : Input = _inputNum(id_("max"), "max", attrValue)
  val inputRandom: Input = _inputNum(id_("random"), "rand", attrValue)
  val inputSample: Input = _inputNum(id_("sample"), "sample", attrValue)

  val operandSelector: Select = {
    val ops0 = Seq("=", "!=", "<", ">", "<=", ">=")
    val ops  = if (fulltext) "Word =" +: ops0 else ops0

    val selectedOperand                     = attrValue match {
      case Fulltext(_) => "Word ="
      case Eq(_)       => "="
      case Neq(_)      => "!="
      case Lt(_)       => "<"
      case Gt(_)       => ">"
      case Le(_)       => "<="
      case Ge(_)       => ">="
      case _           => if (fulltext) "Word =" else "="
    }
    val options: Seq[TypedTag[html.Option]] = ops.map {
      case `selectedOperand` => option(selectedOperand, selected := true)
      case op                => option(op)
    }

    _operandSelector(
      marked("op", attrValue), id_("operand"), attrType, options, { () =>
        keepSettingsOpen
        inputValue.select()
      }
    )
  }


  // Actions -------------------------------------------------------------------

  def update(): Unit = {
    def updateInput(fn: String, input: Input): Unit =
      upsertAttr(
        modelElements.now, path, attr, attrType, car, enums, fn, input.value
      ) match {
        case Left(err)           => window.alert(err); input.select()
        case Right(updatedModel) => modelElements() = updatedModel
      }
    val op = operandSelector.value
    curFn match {
      case "min"    => updateInput("min", inputMin)
      case "max"    => updateInput("max", inputMax)
      case "rand"   => updateInput("rand", inputRandom)
      case "sample" => updateInput("sample", inputSample)
      case "value"  => updateInput(op, inputValue)
      case _        => ()
    }
  }

  def resetForm(): Unit = {
    inputValue.value = ""
    inputMin.value = ""
    inputMax.value = ""
  }


  // Validation ----------------------------------------------------------------

  inputValue.onkeyup = { e: Event =>
    curFn = "value"
    if (document.getElementById(id_("mode-none")).asInstanceOf[Input].checked) {
      document.getElementById(id_("mode-mandatory"))
        .asInstanceOf[Input].checked = true
    }
    if (inputValue.value.nonEmpty) {
      validInput =
        validate(operandSelector.value, attrType, inputValue.value) match {
          case Some(err) => window.alert(err); false
          case None      => inputValue.focus(); true
        }
    }
  }

  def validateAggrNum(
    fn: String,
    msg: String,
    input: Input
  ): Event => Unit = { e: Event =>
    curFn = fn
    if (input.value.nonEmpty && !input.value.isInt) {
      input.value = ""
      window.alert(
        s"Please enter the number of rows to fetch $msg values of attribute `$attr`"
      )
      input.focus()
    }
  }

  inputMin.onkeyup =
    validateAggrNum("min", "having the lowest", inputMin)

  inputMax.onkeyup =
    validateAggrNum("max", "having the highest", inputMax)

  inputRandom.onkeyup =
    validateAggrNum("rand", "with possibly duplicate random", inputRandom)

  inputSample.onkeyup =
    validateAggrNum("sample", "with unique sample", inputSample)


  // Input sections ------------------------------------------------------------

  def modes(): TypedTag[Div] = {
    def mode(m: String, label: String, actions: () => Unit): TypedTag[Div] = {
      val checked = attrClass.split('-').last == m
      _radio("mode", id_(s"mode-$m"), m, label, actions, checked)
    }

    def closeInputs: Unit =
      document
        .getElementById(id_("inputs"))
        .setAttribute("style", "display:none")

    if (attrType == "datom") {
      div(
        mode("none", "None", { () =>
          resetForm()
          modelElements() = setMode(modelElements.now, path, attr, "none")
        }),
        mode("mandatory", "Mandatory", { () =>
          modelElements() = setMode(modelElements.now, path, attr, "mandatory")
          keepSettingsOpen
        }),
        if (inputValue.value.nonEmpty) {
          mode("tacit", "Tacit", { () =>
            modelElements() = setMode(modelElements.now, path, attr, "tacit")
            keepSettingsOpen
            inputValue.select()
          })
        } else a(
          href := "##",
          "(Tacit)",
          onclick := { () =>
            window.alert("Tacit entity id(s) only allowed if one or more " +
              "entity ids are applied.")
            keepSettingsOpen
            inputValue.select()
          }
        )
      )
    } else {
      div(
        mode("none", "None", { () =>
          resetForm()
          modelElements() = setMode(modelElements.now, path, attr, "none")
        }),
        mode("mandatory", "Mandatory", { () =>
          modelElements() = setMode(modelElements.now, path, attr, "mandatory")
          keepSettingsOpen
          inputValue.select() // Todo: why doesn't this work?
        }),
        mode("tacit", "Tacit", { () =>
          modelElements() = setMode(modelElements.now, path, attr, "tacit")
          keepSettingsOpen
        }),
        mode("optional", "Optional", { () =>
          resetForm()
          modelElements() = setMode(modelElements.now, path, attr, "optional")
          keepSettingsOpen
          closeInputs
        }),
        mode("nil", "Nil", { () =>
          resetForm()
          modelElements() = setMode(modelElements.now, path, attr, "nil")
          keepSettingsOpen
          closeInputs
        })
      )
    }
  }

  def compareValue(): TypedTag[Div] = div(_hr, attrType match {
    case "datom" | "UUID" | "URI" =>
      _inputGroup(id_("operand"), id_("value"), inputValue)
    case _                        =>
      _inputGroupMb3(operandSelector, inputValue)
  })

  def top25(): TypedTag[Div] = div(
    //    _hr,
    if (car == 1) () else div(
      cls := "input-group mb-3",
      _radio("card-semantics", id_("card-semantics-or"), "or", "OR", { () => }, true),
      _radio("card-semantics", id_("card-semantics-and"), "and", "AND", { () => })
    ),
    Rx {
      div(
        marginTop := -5,
        for ((TopValue(c, v0, labelOpt), j) <- topValues.zipWithIndex) yield {
          val (v, display): (String, String) = attrType match {
            case "String" if enums.nonEmpty => (labelOpt.get, labelOpt.get)
            case _                          => (v0, labelOpt.getOrElse(v0))
          }

          val topValueCheckBox0 = _topValueCheckBox(id_("top25-" + j), attrValue, v)

          def updateTopValue() = { () =>
            val sep       = attrType match {
              case "String" => "||"
              case _        => ","
            }
            val newValues = attrValue match {
              case Eq(vs) if vs.map(_.toString).contains(v.toString) =>
                vs.filterNot(_.toString == v).mkString(sep)

              case Eq(vs) if !topValueCheckBox0.render.checked =>
                (vs :+ v).mkString(sep)

              case _ => v
            }
            modelElements() =
              upsertAttr(
                modelElements.now, path, attr, attrType, car, enums, "=", newValues
              ) match {
                case Right(updatedModel) => updatedModel
                case Left(err)           => throw new RuntimeException("auch! " + err)

              }
            keepSettingsOpen
          }
          // Add onchange action
          val topValueCheckBox = topValueCheckBox0(onchange := updateTopValue())

          _topValue(topValueCheckBox, v, display, thousands(c), updateTopValue())
        }
      )
    }
  )

  def toggleFn(fn: String, input: Option[Input] = None): () => Unit = { () =>
    attrValue match {
      case Fn(`fn`, _) => modelElements() =
        upsertAttr(
          modelElements.now, path, attr, attrType, car, enums, "", ""
        ).right.get
      case Distinct    => modelElements() =
        upsertAttr(
          modelElements.now, path, attr, attrType, car, enums, "", ""
        ).right.get
      case _           =>
        upsertAttr(
          modelElements.now, path, attr, attrType, car, enums, fn, ""
        ) match {
          case Left(err)           => window.alert(err)
          case Right(updatedModel) => modelElements() = updatedModel
        }
    }
    keepSettingsOpen
    if (input.nonEmpty) input.get.select()
  }

  def button2(label: String, fn: String): TypedTag[Div] =
    _btn2(label, id_(fn), marked(fn, attrValue), toggleFn(fn))

  def checkBox(show: String, fn: String): TypedTag[Div] = div(
    cls := "form-check",
    paddingRight := 15,
    input(
      tpe := "checkbox",
      cls := "form-check-input",
      id := id_(fn),
      value := fn,
      if (additives contains fn) checked := true else (),
      onchange := { () =>
        modelElements() =
          upsertAttr(
            modelElements.now, path, attr, attrType, car, enums, fn, ""
          ).right.get
      }
    ),
    label(
      cls := "form-check-label",
      `for` := id_(fn),
      marginLeft := -3,
      show
    )
  )

  def aggrInputs(): TypedTag[Div] = {
    def aggrButton(label: String, fn: String, input: Input): TypedTag[Div] =
      _aggrButton(label, id_(fn), input, marked(fn, attrValue), toggleFn(fn))
    div(
      _hr,
      _row(
        _rowColAuto(aggrButton("Min", "min", inputMin), paddingRight := "0"),
        _rowColAuto(aggrButton("Max", "max", inputMax), paddingRight := "0")

      ),
      _row(
        _rowColAuto(aggrButton("Random", "rand", inputRandom), paddingRight := "0"),
        _rowColAuto(aggrButton("Sample", "sample", inputSample), paddingRight := "0"),
        if (car == 1)
          _rowColAuto(button2("Distinct", "distinct"), paddingRight := "0")
        else
          (),
        marginBottom := "-15px"
      )
    )
  }

  def count(): TypedTag[Div] = div(
    _hr,
    button2("Count", "count")
  )

  def aggrCounts(): TypedTag[Div] = div(
    _hr,
    span(
      display.flex,
      checkBox("Count", "count"),
      checkBox("Count distinct", "count-distinct"),
    )
  )

  def aggrNumbers(): TypedTag[Div] = div(
    span(
      display.flex,
      marginTop := 8,
      checkBox("Sum", "sum"),
      checkBox("Avg", "avg"),
      checkBox("Median", "median"),
      checkBox("Variance", "variance"),
      checkBox("Stddev", "stddev"),
    )
  )

  def txGenerics(): TypedTag[Div] = div(
    _hr,
    span(
      display.flex,
      checkBox("t", "t"),
      checkBox("tx", "tx"),
      checkBox("txInstant", "txInstant"),
    )
  )


  // Build variations of input dropdown depending on attr type -----------------

  val inputs: TypedTag[Div] = div(id := id_("inputs"))

  def render: TypedTag[LI] = _submenu(attrClass)(
    a(
      href := "#",
      attr + manyAsterisk,
      onclick := { () =>
        modelElements() = toggleMode(modelElements.now, path, attr)
      },
    ),
    _menu("settings")(
      id := id_("settings"),
      form(
        onsubmit := { () => false },
        onchange := { () =>
          if (validInput) {
            update()
            keepSettingsOpen
          }
        },
        modes(),
        if (doc.isDefined) _doc(doc.get) else (),
        attrType match {
          case _ if car == 3 && topValues.nonEmpty =>
            inputs(compareValue(), top25(), txGenerics())

          case _ if car == 3 =>
            inputs(compareValue(), txGenerics())

          case "datom" if attrClass == "attr-tacit" =>
            inputs(compareValue())

          case "datom" =>
            inputs(compareValue(), count())

          case "UUID" =>
            inputs(compareValue(), aggrCounts(), txGenerics())

          case "URI" =>
            inputs(aggrCounts(), txGenerics())

          case "Boolean" if topValues.nonEmpty =>
            inputs(hr, top25(), aggrCounts(), txGenerics())

          case "Boolean" =>
            inputs(aggrCounts(), txGenerics())

          case n if isNumber(n) && topValues.nonEmpty =>
            inputs(compareValue(), top25(), aggrInputs(), aggrCounts(),
              aggrNumbers(), txGenerics())

          case n if isNumber(n) =>
            inputs(compareValue(), aggrInputs(), aggrCounts(),
              aggrNumbers(), txGenerics())

          case "String" if enums.isDefined && topValues.nonEmpty =>
            inputs(hr, top25(), aggrCounts(), txGenerics())

          case "String" if enums.isDefined =>
            inputs(aggrCounts(), txGenerics())

          case _ if topValues.nonEmpty =>
            inputs(compareValue(), top25(), aggrInputs(), aggrCounts(), txGenerics())

          case _ =>
            inputs(compareValue(), aggrInputs(), aggrCounts(), txGenerics())

        }
      )
    )
  )
}