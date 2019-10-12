package moleculeadmin.client.inspiration

import domimplicits._
import org.scalajs.dom
import org.scalajs.dom.raw._

import scala.scalajs.js

object elements {


//  object WindowResize {
//    def register(f: () => Unit) = ???
//    def handle(e: dom.Event): Unit = {
//      val allElements: NodeList = dom.document.getElementsByClassName("resize-callback-cls")
//      for (k <- allElements) k.asInstanceOf[js.Dynamic].resizeCallback()
//    }
//    dom.window.addEventListener("resize", handle _)
//  }

  def initCanvas(graphCanvas: dom.html.Canvas) = {
    graphCanvas.style.display = "block"
//    graphCanvas.style.width = slice.pixelWidth.toString
    graphCanvas.height = (24 * dom.window.devicePixelRatio).toInt
    graphCanvas.style.height = 24.toString
  }
}
