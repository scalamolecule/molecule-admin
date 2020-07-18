package moleculeadmin.client.app.logic.schema.definition

import autowire._
import boopickle.Default._
import moleculeadmin.client.app.logic.schema.SchemaState._
import moleculeadmin.client.schemaWire
import moleculeadmin.shared.ast.schema.{Attr => Attr_, _}
import org.scalajs.dom.raw.{HTMLInputElement, HTMLSelectElement, HTMLTextAreaElement}
import org.scalajs.dom.{Element, document, window}
import rx.{Ctx, Rx, Var}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.generic.{Attr, AttrPair}
import scala.concurrent.ExecutionContext.Implicits.global


case class AttributeForm(db: String,
                         schema1: MetaSchema,
                         part: String,
                         ns: String,
                         pos: Int,
                         attr: String,
                         card: Int,
                         attrType0: String,
                         enums: Option[Set[String]],
                         refNs: Option[String],
                         options: Option[Set[String]],
                         doc: Option[String],
                         attrs: Seq[Attr_],
                         attrGroup0: Option[String])
                        (implicit val ctx: Ctx.Owner) extends Base {


  def render = {
    val update = pos > 0
    def v(str: String): AttrPair[Element, String] = if (update && str.nonEmpty) value := str else Attr("empty").empty

    val curInput   = Var[String]("")
    val lastOption = Var[String]("")

    val strOptions = Seq("Select..", "uniqueValue", "uniqueIdentity", "noHistory", "fulltext")
    val valOptions = Seq("Select..", "uniqueValue", "uniqueIdentity", "noHistory")
    val refOptions = Seq("Select..", "uniqueValue", "uniqueIdentity", "noHistory", "isComponent")

    def renderTypeSelector(tpes: Seq[String], tpe: String, additionalElements: JsDom.all.HtmlTag*): Unit = append(
      "typeWrapper",
      select(id := s"attr-type",
        tpes.map {
          case t@("bi" | "biEdge") => option(value := t, t, disabled := true)
          case t                   => option(value := t, t, if (t == tpe) selected := true else ())
        }
      ).render +: additionalElements.map(_.render): _*
    )

    def renderRefNsSelector(curRefNs: String, top: Int = 10) = select(
      id := s"attr-refNs",
      marginTop := s"${top}px",
      for {
        Part(_, part1, _, _, nss) <- schema1.parts
        Ns(_, ns1, _, _, _, _) <- nss
      } yield {
        val refNs1 = part match {
          case "db.part/user" => ns1
          case _              => part1 + "_" + ns1
        }
        option(value := refNs1, refNs1, if (refNs1 == curRefNs) selected := true else ())
      }
    )

    def renderEnumsTextArea(curEnums0: Option[Set[String]] = None, curEnumsStr: Option[String] = None) = {
      val enumValues = if (curEnums0.isDefined)
        curEnums0.get.toSeq.sorted
      else if (curEnumsStr.isDefined)
        curEnumsStr.get.split("\n").toSeq.sorted
      else
        Nil
      textarea(
        id := "attr-enums", marginTop := 10, cols := 20, rows := enumValues.size + 6, placeholder := "enum\nvalues..",
        enumValues.mkString("\n")
      )
    }

    def renderOptions(opts: Seq[String], curOpts: Seq[String]) = append("optsWrapper",
      if (curOpts.isEmpty) {
        select(id := s"attr-opts",
          opts.map(opt =>
            option(value := opt, opt)
          )
        ).render
      } else {
        div(
          opts.tail.map(opt =>
            div(
              cls := "form-check",
              input(tpe := "checkbox", cls := "form-check-input", id := s"attr-opts-$opt", name := "attr-opts", value := opt,
                if (curOpts contains opt) checked := true else (),
                onchange := { () => lastOption() = opt }
              ),
              label(cls := "form-check-label", `for` := s"attr-opts-$opt", width := "100%", opt)
            )
          )
        ).render
      }
    )

    def getOpts: Seq[String] = {
      val opts = document.getElementsByName("attr-opts")
      if (opts.length > 0) {
        (0 until opts.length).flatMap { i =>
          val elem = opts.item(i).asInstanceOf[HTMLInputElement]
          if (elem.checked)
            elem.value match {
              case "uniqueValue" if lastOption.now == "uniqueIdentity" => None
              case "uniqueIdentity" if lastOption.now == "uniqueValue" => None
              case otherOpt                                            => Some(otherOpt)
            }
          else
            None
        }
      } else {
        document.getElementById("attr-opts").asInstanceOf[HTMLSelectElement].value match {
          case "Select.." => Nil
          case v          => Seq(v)
        }
      }
    }

    def getType: String = if (document.getElementById("attr-type") != null)
      document.getElementById("attr-type").asInstanceOf[HTMLSelectElement].value
    else
      attrType0


    val attrGroupHeader = input(
      id := "attr-group", size := 60, v(attrGroup0.getOrElse("")),
      onfocus := { () =>
        document.getElementById("attr-group-radio-header").asInstanceOf[HTMLInputElement].checked = true
      }
    ).render
    val attrGroup       = {
      val curIndex = attrGroup0 match {
        case Some("")  => 1
        case Some(txt) => 2
        case None      => 0
      }
      div(
        div(cls := "form-check",
          input(tpe := "radio", cls := "form-check-input", id := s"attr-group-radio-none", name := "attr-group-radio", value := "none",
            if (attrGroup0.isEmpty) checked := true else ()
          ),
          label(cls := "form-check-label", `for` := "attr-group-radio-none", width := "100%", "No header")
        ),
        div(cls := "form-check",
          input(tpe := "radio", cls := "form-check-input", id := s"attr-group-radio-empty", name := "attr-group-radio", value := "empty",
            if (attrGroup0.isDefined && attrGroup0.get.isEmpty) checked := true else ()
          ),
          label(cls := "form-check-label", `for` := "attr-group-radio-empty", width := "100%", "Empty header")
        ),
        div(cls := "form-check",
          input(tpe := "radio", cls := "form-check-input", id := s"attr-group-radio-header", name := "attr-group-radio", value := "header",
            if (attrGroup0.isDefined && attrGroup0.get.nonEmpty) checked := true else ()
          ),
          label(cls := "form-check-label", `for` := "attr-group-radio-header", width := "100%",
            onclick := { () =>
              attrGroupHeader.select()
            },
            "Header: ", attrGroupHeader
          )
        )
      ).render
    }

    def getAttrGroup: Option[String] = {
      val attrGroupRadios = document.getElementsByName("attr-group-radio")
      val optNone         = attrGroupRadios.item(0).asInstanceOf[HTMLInputElement]
      val optEmpty        = attrGroupRadios.item(1).asInstanceOf[HTMLInputElement]
      if (optNone.checked)
        None
      else if (optEmpty.checked)
        Some("")
      else
        Some(attrGroupHeader.value.trim)
    }

    val attrName = input(id := "attr-name", size := 15, v(attr)).render

    val attrCard = select(
      id := "attr-card",
      //        tpe := "radio",
      option(value := 1, "one"),
      option(value := 2, "many"),
      option(value := 3, "map")
    ).render
    attrCard.selectedIndex = if (update) card - 1 else 0

    val attrType = if (update)
      attrType0 match {
        case "ref"  => div("Ref to ", renderRefNsSelector(refNs.get, 0)).render
        case "Enum" => div("Enum with values:", br, renderEnumsTextArea(enums)).render
        case _      => div(attrType0).render
      }
    else
      select(id := "attr-type",
        allTypes.map {
          case t@("bi" | "biEdge") => option(value := t, t, disabled := true) // todo: implement bidirectional schema attributes
          case t                   => option(value := t, t, if (t == attrType0) selected := true else ())
        }
      ).render

    val attrOpts = if (update) {
      val opts    = if (card == 3) valOptions else attrType0 match {
        case "String"                => strOptions
        case "ref" | "biEdge" | "bi" => refOptions
        case _                       => valOptions
      }
      val curOpts = options.getOrElse(Nil).toSeq
      div(
        opts.tail.map(v =>
          div(cls := "form-check",
            input(tpe := "checkbox", cls := "form-check-input", id := s"attr-opts-$v", name := "attr-opts", value := v,
              if (curOpts contains v) checked := true else (),
              onchange := { () => lastOption() = v }
            ),
            label(cls := "form-check-label", `for` := s"attr-opts-$v", width := "100%", v)
          )
        )
      ).render
    } else {
      select(id := "attr-opts", strOptions.map(opt => option(value := opt, opt))).render
    }

    val attrDoc = input(id := "attr-descr", size := 40, v(doc.getOrElse(""))).render

    val attrCount = attrs.size
    val attrPos   = select(id := "attr-pos",
      option(value := 1, "First"),
      if (update) {
        attrs.filterNot(_.name == attr).map {
          case a if a.pos == pos - 1 => option(value := a.pos + 1, a.name, selected := true)
          case a if a.pos > pos      => option(value := a.pos, a.name)
          case a                     => option(value := a.pos + 1, a.name)
        }
      } else {
        attrs.map {
          case a if a.pos == attrCount => option(value := a.pos + 1, a.name, selected := true)
          case a                       => option(value := a.pos + 1, a.name)
        }
      }
    ).render

    val submitButton = button(
      id := "attrFormSubmit",
      cls := "btn btn-outline-dark btn-sm",
      tpe := "submit",
      if (update) "Update attribute" else "Create attribute"
    ).render


    val submitErr   = err("attr-err")
    val attrNameErr = err("attr-name-err")
    val attrDocErr  = err("attr-doc-err")

    def helpSymbol(txt: String) =
      i(
        cls := "fas fa-question-circle pointer", color := "#aaa",
        onclick := { () => window.alert(txt) }
      )
    val helpTypeCreate = helpSymbol("""Bidirectional types ("bi", "biEdge") not yet implemented in Molecule-Admin.""".stripMargin)
    val helpTypeUpdate = helpSymbol(if (attrType0 == "ref")
      """Only the referenced namespace of a ref attribute can be altered.
        |Basic attribute types can't be altered in Datomic. If you need another type:
        |1. create new attribute with that type
        |2. assert converted data with that attribute
        |3. delete current attribute""".stripMargin
    else
      """Attribute types can't be altered in Datomic. If you need another type:
        |1. create new attribute with that type
        |2. assert converted data with that attribute
        |3. delete current attribute""".stripMargin)
    val helpOptions    = helpSymbol("Index option not shown since all attributes in Molecule are indexed by default.")


    def valid(): Boolean = {
      val attrName1 = attrName.value.trim

      val nameErr = if (attrName1.isEmpty)
        "Please enter an attribute name"
      else if (attrName1.head.isUpper)
        "Attribute name should start with lowercase letter."
      else if (attrName1.head.isDigit)
        "Attribute name can't start with a digit."
      else if (attrName1.contains(' '))
        "Attribute name has to be a single word (no spaces)."
      else if (!attrName1.matches("[a-z][a-zA-Z0-9]*"))
        "Attribute name can only contain standard english letters a-zA-Z and digits 0-9."
      else if (reservedAttrNames.contains(attrName1))
        "Reserved attribute names:<br>" + reservedAttrNames.mkString("<br>")
      else if (attr != attrName1 && attrs.exists(_.name == attrName1))
        s"Attribute name `$attrName1` already exists. Please enter another name."
      else
        ""

      val docErr = if (attrDoc.value.trim.contains("\""))
        "Attribute description (doc) can't contain double quotes (\"). Please use single quotes (') instead."
      else ""

      if (nameErr.nonEmpty) {
        processing() = ""
        attrNameErr.innerHTML = nameErr
        submitButton.disabled = false
        attrName.select()
        false
      } else if (docErr.nonEmpty) {
        processing() = ""
        attrDocErr.innerHTML = docErr
        submitButton.disabled = false
        attrDoc.select()
        false
      } else {
        attrNameErr.innerHTML = ""
        attrDocErr.innerHTML = ""
        true
      }
    }


    form(
      if (update)
        _row(
          paddingBottom := 20,
          _rowColAuto2(
            "Attribute group header",
            br, attrGroup
          )
        ) else (),
      _row(
        _rowColAuto2(
          if (update) "Attribute" else "New Attribute",
          br, attrName,
          br, attrNameErr
        ),
        _rowColAuto2(
          div("Card", paddingLeft := 5),
          div(id := "cardWrapper", attrCard),
          onchange := { () => curInput() = "card" }
        ),
        _rowColAuto2(
          if (update) div("Type ", helpTypeUpdate) else div("Type ", paddingLeft := 5, helpTypeCreate),
          div(id := "typeWrapper", attrType),
          onchange := { () => curInput() = "type" }
        ),
        _rowColAuto2(
          div("Options ", paddingLeft := 5, color := "#777", helpOptions),
          div(id := "optsWrapper", attrOpts),
          onchange := { () => curInput() = "opts" }
        ),
        _rowColAuto2(
          "Description", color := "#777",
          br, attrDoc,
          br, attrDocErr
        ),
        if (attrs.nonEmpty) {
          _rowColAuto2(
            div("After", paddingLeft := 5),
            attrPos
          )
        } else (),
        _rowColAuto2(
          br, div(whiteSpace.nowrap, submitButton, Rx(if (processing() == "attr") _sync(15) else span())),
          br, submitErr
        )
      ),


      // Onchange dynamic re-render -------------------------------------------------------------------------------------

      onchange := { () =>
        valid()
        // println("onchange")
        if (Seq("card", "type", "opts") contains curInput.now) {
          // Dynamically get the values (can't use reference to select since the DOM element is replaced
          val c = document.getElementById("attr-card").asInstanceOf[HTMLSelectElement].value
          val t = getType

          (c, t) match {
            case ("map", "biEdge") =>
              renderTypeSelector(mapTypes, t)
              renderOptions(valOptions, getOpts)
            case ("map", "bi")     =>
              renderTypeSelector(mapTypes, t)
              renderOptions(valOptions, getOpts)
            case ("map", "ref")    =>
              renderTypeSelector(mapTypes, t)
              renderOptions(valOptions, getOpts)
            case ("map", _)        =>
              renderTypeSelector(mapTypes, t)
              renderOptions(valOptions, getOpts)
            case (c1, "biEdge")    =>
              renderTypeSelector(allTypes, t)
              renderOptions(refOptions, getOpts)
            case (c1, "bi")        =>
              renderTypeSelector(allTypes, t)
              renderOptions(refOptions, getOpts)
            case (c1, "ref")       =>
              val refNsInput = document.getElementById("attr-refNs")
              val newRefNs   = if (refNsInput != null) refNsInput.asInstanceOf[HTMLSelectElement].value else ""
              if (update)
                div("Ref to ", renderRefNsSelector(newRefNs, 0)).render
              else
                renderTypeSelector(allTypes, t, br, renderRefNsSelector(newRefNs))
              renderOptions(refOptions, getOpts)
            case (_, "String")     =>
              renderTypeSelector(allTypes, t)
              renderOptions(strOptions, getOpts)
            case (_, "Enum")       =>
              val enumInput   = document.getElementById("attr-enums")
              val curEnumsStr = if (enumInput != null) enumInput.asInstanceOf[HTMLTextAreaElement].value else ""
              if (update)
                div("Enum with values:", br, renderEnumsTextArea(None, Some(curEnumsStr))).render
              else
                renderTypeSelector(allTypes, t, br, renderEnumsTextArea())
              renderOptions(valOptions, getOpts)

            case (_, _) =>
              renderTypeSelector(allTypes, t)
              renderOptions(valOptions, getOpts)
          }
        }
      },


      // Submit -------------------------------------------------------------------------------------

      onsubmit := { () =>
        // Avoid resubmission
        submitButton.disabled = true

        // activate spinner
        processing() = "attr"

        if (valid()) {
          // Dynamically get the values (can't use reference to select since the DOM elements are being substituted
          val card1       = document.getElementById("attr-card").asInstanceOf[HTMLSelectElement].value.toInt
          val tpe: String = getType

          val enums: Seq[String] = if (document.getElementById("attr-enums") != null)
            document.getElementById("attr-enums").asInstanceOf[HTMLTextAreaElement].value.split('\n').toSeq
          else
            Nil

          val refNs: Option[String] = if (document.getElementById("attr-refNs") != null)
            Some(document.getElementById("attr-refNs").asInstanceOf[HTMLSelectElement].value)
          else
            None

          val doc           = if (attrDoc.value.nonEmpty) Some(attrDoc.value) else None
          val submittedAttr = attrName.value.trim


          // todo: example of println-debugging to client console...
          //            println("-------------------------------")
          //            println("attr : " + attrName.value)
          //            println("card : " + card1)
          //            println("type : " + tpe)
          //            println("enums: " + enums)
          //            println("refNs: " + refNs)
          //            println("opts : " + curOpts)
          //            println("doc  : " + doc)
          //            println("pos  : " + attrPos.value)
          //            println("-------------------------------")


          // Backend processing -------------------------------------------------------------------------------------

          if (update) {
            schemaWire().updateAttribute(
              schema1, db, part, ns, attr, attrName.value.trim, card1, tpe, enums, refNs, getOpts, doc, attrPos.value.toInt, getAttrGroup
            ).call().foreach {
              case Left(errMsg) =>
                processing() = ""
                submitErr.innerHTML = errMsg
                submitButton.disabled = false
                attrName.select()

              case Right(updatedSchema) =>
                processing() = ""
                // close attribute and go back to namespace with attribute list
                open() = open.now.map {
                  case (`part`, nss1, _) => (part, nss1.filterNot(_ == ns) :+ ns, None)
                  case (part1, nss1, _)  => (part1, nss1, None)
                }
                schema() = updatedSchema
            }
          } else {
            schemaWire().createAttribute(
              schema1, db, part, ns, attrName.value.trim, card1, tpe, enums, refNs, getOpts, doc, attrPos.value.toInt
            ).call().foreach {
              case Left(errMsg) =>
                processing() = ""
                submitErr.innerHTML = errMsg
                submitButton.disabled = false
                attrName.select()

              case Right(updatedSchema) =>
                processing() = ""
                schema() = updatedSchema
                attrName.select()
            }
          }
        }

        false // don't send form / reload page - all rendering is triggered by Rx variables being updated
      } // onsubmit
    )
  }
}
