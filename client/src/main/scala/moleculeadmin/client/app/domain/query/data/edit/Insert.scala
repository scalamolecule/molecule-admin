package moleculeadmin.client.app.domain.query.data.edit

import moleculeadmin.client.app.domain.query.QueryState.columns
import moleculeadmin.client.app.domain.query.data.TypeValidation
import moleculeadmin.client.app.domain.query.keyEvents.BaseKeyEvents
import moleculeadmin.client.app.element.query.datatable.BodyElements
import moleculeadmin.shared.ast.query.Col
import org.scalajs.dom.html.TableCell
import org.scalajs.dom.window
import scala.collection.mutable.ListBuffer


trait Insert extends BaseKeyEvents with BodyElements with TypeValidation {

  protected var keepInserting = false
  protected var error         = false

  protected def editableCols: Seq[Col] = {
    columns.now.flatMap {
      case col if col.attr == "e"        => None
      case Col(_, _, _, _, _, _, _, _, _, _, _,
      "t" | "tx" | "txInstant", _, _, _) => None
      case col                           => Some(col)
    }
  }

  protected def extract(col: Col, cell: TableCell): Seq[String] = {
    val (attr, attrType, card, enums, mandatory) =
      (col.attr, col.attrType, col.card, col.enums, !col.opt)

    val html = cell.innerHTML

    def err(msg: String): Nothing = {
      error = true
      cell.focus()
      window.alert(msg)
      throw new IllegalArgumentException(msg)
    }

    def validate(str: String): String = {
      if (!valid(attrType, str)) {
        err(s"Invalid `$attr` value of type `$attrType`:\n$str")
      } else {
        str
      }
    }

    card match {
      case 1 =>
        val v = _html2str(html)
        if (v.nonEmpty) {
          Seq(validate(v))
        } else if (mandatory) {
          err(s"Mandatory `$attr` card-one value can't be empty")
        } else {
          Nil
        }

      case 2 =>
        val strs = if (attrType == "String")
          html.substring(8, html.length - 10).split("</li><li>").toList
        else
          html.split("<br>").toList
        val vs   = if (attrType == "String" && enums.isEmpty) {
          strs.map(_
            .replaceAllLiterally("&nbsp;", " ")
            .replaceAllLiterally("&lt;", "<")
            .replaceAllLiterally("&gt;", ">")
            .replaceAllLiterally("&amp;", "&")
            .replaceAllLiterally("<br>", "\n")
            .trim)
            .filter(_.nonEmpty)
            .map(_html2str)
        } else {
          strs.flatMap(
            _.split("<br>")
              .map(_
                .replaceAllLiterally("&nbsp;", " ")
                .replaceAllLiterally("&lt;", "<")
                .replaceAllLiterally("&gt;", ">")
                .replaceAllLiterally("&amp;", "&")
                .trim)
              .filter(_.nonEmpty)
          )
        }
        if (vs.nonEmpty) {
          vs.distinct.map(validate)
        } else if (mandatory) {
          err(s"Mandatory `$attr` card-many value can't be empty")
        } else {
          Nil
        }

      case 3 =>
        val strs = if (attrType == "String")
          html.substring(8, html.length - 10).split("</li><li>").toList
        else
          html.split("<br>").toList

        val vs = if (attrType == "String" && enums.isEmpty) {
          strs
            .map(_
              .replaceAllLiterally("&nbsp;", " ")
              .replaceAllLiterally("&lt;", "<")
              .replaceAllLiterally("&gt;", ">")
              .replaceAllLiterally("&amp;", "&")
              .replaceAllLiterally("<br>", "\n")
              .trim)
            .filter(_.nonEmpty)
            .map(_html2str)
        } else {
          strs.flatMap(
            _.split("<br>")
              .map(_
                .replaceAllLiterally("&nbsp;", " ")
                .replaceAllLiterally("&lt;", "<")
                .replaceAllLiterally("&gt;", ">")
                .replaceAllLiterally("&amp;", "&")
                .trim)
              .filter(_.nonEmpty)
          )
        }
        if (vs.nonEmpty) {
          var keys = new ListBuffer[String]
          vs.map { pair =>
            if (!pair.contains("->"))
              err("Key/value should be separated by '->'")

            val k = pair.substring(0, pair.indexOf("->")).trim
            val v = pair.substring(pair.indexOf("->") + 2).trim

            if (keys.contains(k))
              err(s"Disallowed duplicate key `$k`")
            keys += k

            if (attrType == "Date")
              k + "__~~__" + truncateDateStr(validate(v))
            else
              k + "__~~__" + validate(v)
          }
        } else if (mandatory) {
          err(s"Mandatory `$attr` map value can't be empty")
        } else {
          Nil
        }
    }
  }

  def valuePairs(rowValues: Seq[Seq[String]]): Seq[String] = {
    val cols = editableCols
    val maxLength = cols.map(c => (c.nsFull + c.attr).length).max
    cols.zipWithIndex.map {
      case (col, i) =>
        val attr   = s"${col.nsFull}/${col.attr}"
        val indent = " " * (maxLength - attr.length + 1)
        col.card match {
          case 1 => attr + indent + " = " + rowValues(i).head
          case 2 => attr + indent + " = " + rowValues(i).toSet
          case 3 => attr + indent + " = " + rowValues(i).map { str =>
            val List(k, v) = str.split("__~~__", 2).toList
            k -> v
          }.toMap
        }
    }
  }
}
