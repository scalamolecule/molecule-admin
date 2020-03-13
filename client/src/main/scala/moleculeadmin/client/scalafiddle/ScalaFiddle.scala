package moleculeadmin.client.scalafiddle

import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import moleculeadmin.client.scalafiddle.ast._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.annotation.JSExportTopLevel

/**
 * Complete source code including imports of a JS object that produces a lambda
 * that can convert an attribute value according to the right hand side code
 * input from the user.
 *
 * Code is sent via Ajax to ScalaFiddle compileServer that compiles it to JS
 * and returns the JS code.
 *
 * We then evaluate the JS code and retrieve the produced lambda function that
 * we can then cast to the expected type in Scala and apply to the values of
 * the group edit column values. Voila!
 *
 * ScalaFiddle must be started first in separate processes from the
 * ScalaFiddle-core project dir:
 *
 * $ cd <..>/scalafiddle-core
 * $ sbt "~; router/reStart; compilerServer/reStart"
 *
 * @param scalaCode Scala source code for JS object
 * @tparam TransferType Lambda result type.
 *
 *                      For each cardinality TransferType will be:
 *
 *                      card 1: js.UndefOr[String]
 *                      card 2: js.Array[AttrTransferType]
 *                      card 3: js.Dictionary[AttrTransferType]
 *
 *                      .. where AttrTransferType is a JS-compatible type that
 *                      the attribute value is being converted to/from to cross
 *                      the boundary to/from JS safely:
 *
 *                      String, Int, Boolean, LocalDateTime, UUID or URI
 */
case class ScalaFiddle[TransferType](scalaCode: String) {

  type ResultPair = js.Tuple2[TransferType, String]

  def lambda2: Future[js.Tuple2[Any, Any] => ResultPair] = getLambda[js.Tuple2[Any, Any] => ResultPair]
  def lambda3: Future[js.Tuple3[Any, Any, Any] => ResultPair] = getLambda[js.Tuple3[Any, Any, Any] => ResultPair]
  def lambda4: Future[js.Tuple4[Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple4[Any, Any, Any, Any] => ResultPair]
  def lambda5: Future[js.Tuple5[Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple5[Any, Any, Any, Any, Any] => ResultPair]
  def lambda6: Future[js.Tuple6[Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple6[Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda7: Future[js.Tuple7[Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple7[Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda8: Future[js.Tuple8[Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple8[Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda9: Future[js.Tuple9[Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple9[Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda10: Future[js.Tuple10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda11: Future[js.Tuple11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda12: Future[js.Tuple12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda13: Future[js.Tuple13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda14: Future[js.Tuple14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda15: Future[js.Tuple15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda16: Future[js.Tuple16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda17: Future[js.Tuple17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda18: Future[js.Tuple18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda19: Future[js.Tuple19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda20: Future[js.Tuple20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda21: Future[js.Tuple21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]
  def lambda22: Future[js.Tuple22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair] = getLambda[js.Tuple22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => ResultPair]


  private def getLambda[Lambda]: Future[Lambda] = {

    // post actual source code and get compilation result
    val startTime = System.currentTimeMillis()
    Ajax.post(
      url = "http://localhost:8880/compile?opt=fast",
      data = scalaCode
    ).map { res =>
      val compileTime = System.currentTimeMillis() - startTime
      println(s"Compilation took $compileTime ms")
      println(s"----- Scala code: -----")
      println(numbered(scalaCode))

      val compResponse = readCompilationResponse(res.responseText)
      val jsCode       = compResponse.jsCode.getOrElse("No js code...")
      //      println(s"jsCode --------------\n" + jsCode)
      if (compResponse.annotations.nonEmpty) {
        val EditorAnnotation(row, _, text, _) = compResponse.annotations.head
        println(s"----- Scala code: -----")
        println(numbered(scalaCode))
        println(s"----- Compile error in line ${row + 1} -----")
        println(text.mkString("\n"))
        throw JavaScriptException("Compilation error")
      }

      // The magic happens here
      // Evaluate js code to bring ScalaFiddle into js scope
      js.eval(jsCode)

      // Return js edit lambda and cast it back in to Scala land
      js.eval("ScalaFiddle.lambda").asInstanceOf[Lambda]

    } recover {
      case e: dom.ext.AjaxException if e.xhr.status == 400 =>
        val msg = s"ScalaFiddle ajax error 400: " + e.xhr.responseText
        println(msg)
        println("Please try again (refresh if testing)" +
          " - this seems to give a valid compiler.")
        //        err(msg)
        //        js.eval(s"window.alert('$msg')")
        throw e

      case e: dom.ext.AjaxException if e.xhr.status == 0 =>
        println(s"Error: " + e.xhr.responseText)
        js.eval("window.alert('Please start Scala fiddle router and server.')")
        throw e

      case e: dom.ext.AjaxException =>
        println(s"Error: ${e.xhr.responseText}")
        println(s"Error: ${e.xhr.responseType}")
        println(s"Error: ${e.xhr.status}")
        println(s"Error: ${e.xhr.statusText}")
        js.eval("window.alert('ScalaFiddle ajax error: " + e.xhr.responseText + "')")
        throw e

      case e: JavaScriptException =>
        println(e.getMessage)
        js.eval(s"window.alert('${e.getMessage} - see console for details')")
        throw e

      case e: Throwable =>
        println(e.toString)
        js.eval("window.alert('ScalaFiddle error - see console for error msgs')")
        throw e
    }
  }

  private def readCompilationResponse(jsonStr: String): CompilationResponse = {
    val r = js.JSON.parse(jsonStr).asInstanceOf[CompilationResponseJS]
    CompilationResponse(
      if (r.jsCode.isEmpty) None else Some(r.jsCode(0)),
      r.jsDeps,
      r.cssDeps,
      r.annotations.map { a =>
        EditorAnnotation(a.row, a.col, a.text, a.tpe)
      },
      r.log
    )
  }

  private def numbered(code: String): String = code.split("\n").zipWithIndex.map {
    case (c, i) if i < 9 => s" ${i + 1}  " + c
    case (c, i)          => s"${i + 1}  " + c
  }.mkString("\n")
}