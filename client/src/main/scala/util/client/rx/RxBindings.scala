package util.client.rx

import org.scalajs.dom.{Element, html}
import rx._
import scalatags.JsDom.all._
import scalatags.generic
import scalatags.stylesheet.Cls
import scala.language.implicitConversions
import scala.util.{Failure, Success}

/**
 * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom
 */
trait RxBindings {

  /**
   * Wraps reactive strings in spans, so they can be referenced/replaced
   * when the Rx changes.
   */
  implicit def rx2span[T](r: Rx[T])(implicit f: T => Frag, ctx: Ctx.Owner): Frag =
    htmltag2frag(Rx(span(r())))

  /**
   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
   * to propagate changes into the DOM via the element's ID. Monkey-patches
   * the Obs onto the element itself so we have a reference to kill it when
   * the element leaves the DOM (e.g. it gets deleted).
   */
  implicit def htmltag2frag(r: Rx[HtmlTag])(implicit ctx: Ctx.Owner): Frag =
    bind(r,
      (r1: Rx[HtmlTag]) => r1.toTry match {
        case Success(v) => v.render
        //      case Success(v) => span(v).render
        case Failure(e) => span(e.getMessage, backgroundColor := "red").render
      }
    )

  implicit def stylesheetcls2frag(r: Rx[Cls])(implicit ctx: Ctx.Owner): Frag =
    bind(r,
      (r1: Rx[Cls]) => r1.toTry match {
        case Success(v) => div(v).render
        case Failure(e) => span(e.getMessage, backgroundColor := "red").render
      }
    )

  private def bind[T](
    r: Rx[T],
    getSafeRxValue: Rx[T] => html.Element
  )(implicit ctx: Ctx.Owner): Frag = {
    var last = getSafeRxValue(r)
    r.trigger {
      val newLast = getSafeRxValue(r)
      Option(last.parentElement).foreach(_.replaceChild(newLast, last))
      last = newLast
    }
    bindNode(last)
  }

  implicit def observeAttrValue[T: AttrValue](implicit ctx: Ctx.Owner)
  : generic.AttrValue[Element, Rx.Dynamic[T]] =
    (t: Element, a: Attr, r: Rx.Dynamic[T]) => {
      r.trigger {implicitly[AttrValue[T]].apply(t, a, r.now)} // observer
    }

  implicit def observeStyleValue[T: StyleValue](implicit ctx: Ctx.Owner)
  : generic.StyleValue[Element, Rx.Dynamic[T]] =
    (t: Element, s: Style, r: Rx.Dynamic[T]) => {
      r.trigger {implicitly[StyleValue[T]].apply(t, s, r.now)} // observer
    }
}

object RxBindings extends RxBindings
