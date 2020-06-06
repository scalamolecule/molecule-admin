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

  type T2 = js.Tuple2[Any, Any]
  type T3 = js.Tuple3[Any, Any, Any]
  type T4 = js.Tuple4[Any, Any, Any, Any]
  type T5 = js.Tuple5[Any, Any, Any, Any, Any]
  type T6 = js.Tuple6[Any, Any, Any, Any, Any, Any]
  type T7 = js.Tuple7[Any, Any, Any, Any, Any, Any, Any]
  type T8 = js.Tuple8[Any, Any, Any, Any, Any, Any, Any, Any]
  type T9 = js.Tuple9[Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T10 = js.Tuple10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T11 = js.Tuple11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T12 = js.Tuple12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T13 = js.Tuple13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T14 = js.Tuple14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T15 = js.Tuple15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T16 = js.Tuple16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T17 = js.Tuple17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T18 = js.Tuple18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T19 = js.Tuple19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T20 = js.Tuple20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T21 = js.Tuple21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
  type T22 = js.Tuple22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]

  type L2 = T2 => ResultPair
  type L3 = T3 => ResultPair
  type L4 = T4 => ResultPair
  type L5 = T5 => ResultPair
  type L6 = T6 => ResultPair
  type L7 = T7 => ResultPair
  type L8 = T8 => ResultPair
  type L9 = T9 => ResultPair
  type L10 = T10 => ResultPair
  type L11 = T11 => ResultPair
  type L12 = T12 => ResultPair
  type L13 = T13 => ResultPair
  type L14 = T14 => ResultPair
  type L15 = T15 => ResultPair
  type L16 = T16 => ResultPair
  type L17 = T17 => ResultPair
  type L18 = T18 => ResultPair
  type L19 = T19 => ResultPair
  type L20 = T20 => ResultPair
  type L21 = T21 => ResultPair
  type L22 = T22 => ResultPair
  type L23 = js.Tuple2[T20, T3] => ResultPair
  type L24 = js.Tuple2[T20, T4] => ResultPair
  type L25 = js.Tuple2[T20, T5] => ResultPair
  type L26 = js.Tuple2[T20, T6] => ResultPair
  type L27 = js.Tuple2[T20, T7] => ResultPair
  type L28 = js.Tuple2[T20, T8] => ResultPair
  type L29 = js.Tuple2[T20, T9] => ResultPair
  type L30 = js.Tuple2[T20, T10] => ResultPair
  type L31 = js.Tuple2[T20, T11] => ResultPair
  type L32 = js.Tuple2[T20, T12] => ResultPair
  type L33 = js.Tuple2[T20, T13] => ResultPair
  type L34 = js.Tuple2[T20, T14] => ResultPair
  type L35 = js.Tuple2[T20, T15] => ResultPair
  type L36 = js.Tuple2[T20, T16] => ResultPair
  type L37 = js.Tuple2[T20, T17] => ResultPair
  type L38 = js.Tuple2[T20, T18] => ResultPair
  type L39 = js.Tuple2[T20, T19] => ResultPair
  type L40 = js.Tuple2[T20, T20] => ResultPair
  type L41 = js.Tuple2[T20, T21] => ResultPair
  type L42 = js.Tuple2[T20, T22] => ResultPair
  type L43 = js.Tuple3[T20, T20, T3] => ResultPair
  type L44 = js.Tuple3[T20, T20, T4] => ResultPair
  type L45 = js.Tuple3[T20, T20, T5] => ResultPair
  type L46 = js.Tuple3[T20, T20, T6] => ResultPair
  type L47 = js.Tuple3[T20, T20, T7] => ResultPair
  type L48 = js.Tuple3[T20, T20, T8] => ResultPair
  type L49 = js.Tuple3[T20, T20, T9] => ResultPair
  type L50 = js.Tuple3[T20, T20, T10] => ResultPair
  type L51 = js.Tuple3[T20, T20, T11] => ResultPair
  type L52 = js.Tuple3[T20, T20, T12] => ResultPair
  type L53 = js.Tuple3[T20, T20, T13] => ResultPair
  type L54 = js.Tuple3[T20, T20, T14] => ResultPair
  type L55 = js.Tuple3[T20, T20, T15] => ResultPair
  type L56 = js.Tuple3[T20, T20, T16] => ResultPair
  type L57 = js.Tuple3[T20, T20, T17] => ResultPair
  type L58 = js.Tuple3[T20, T20, T18] => ResultPair
  type L59 = js.Tuple3[T20, T20, T19] => ResultPair
  type L60 = js.Tuple3[T20, T20, T20] => ResultPair

  def lambda2: Future[L2] = getLambda[L2]
  def lambda3: Future[L3] = getLambda[L3]
  def lambda4: Future[L4] = getLambda[L4]
  def lambda5: Future[L5] = getLambda[L5]
  def lambda6: Future[L6] = getLambda[L6]
  def lambda7: Future[L7] = getLambda[L7]
  def lambda8: Future[L8] = getLambda[L8]
  def lambda9: Future[L9] = getLambda[L9]
  def lambda10: Future[L10] = getLambda[L10]
  def lambda11: Future[L11] = getLambda[L11]
  def lambda12: Future[L12] = getLambda[L12]
  def lambda13: Future[L13] = getLambda[L13]
  def lambda14: Future[L14] = getLambda[L14]
  def lambda15: Future[L15] = getLambda[L15]
  def lambda16: Future[L16] = getLambda[L16]
  def lambda17: Future[L17] = getLambda[L17]
  def lambda18: Future[L18] = getLambda[L18]
  def lambda19: Future[L19] = getLambda[L19]
  def lambda20: Future[L20] = getLambda[L20]
  def lambda21: Future[L21] = getLambda[L21]
  def lambda22: Future[L22] = getLambda[L22]
  def lambda23: Future[L23] = getLambda[L23]
  def lambda24: Future[L24] = getLambda[L24]
  def lambda25: Future[L25] = getLambda[L25]
  def lambda26: Future[L26] = getLambda[L26]
  def lambda27: Future[L27] = getLambda[L27]
  def lambda28: Future[L28] = getLambda[L28]
  def lambda29: Future[L29] = getLambda[L29]
  def lambda30: Future[L30] = getLambda[L30]
  def lambda31: Future[L31] = getLambda[L31]
  def lambda32: Future[L32] = getLambda[L32]
  def lambda33: Future[L33] = getLambda[L33]
  def lambda34: Future[L34] = getLambda[L34]
  def lambda35: Future[L35] = getLambda[L35]
  def lambda36: Future[L36] = getLambda[L36]
  def lambda37: Future[L37] = getLambda[L37]
  def lambda38: Future[L38] = getLambda[L38]
  def lambda39: Future[L39] = getLambda[L39]
  def lambda40: Future[L40] = getLambda[L40]
  def lambda41: Future[L41] = getLambda[L41]
  def lambda42: Future[L42] = getLambda[L42]
  def lambda43: Future[L43] = getLambda[L43]
  def lambda44: Future[L44] = getLambda[L44]
  def lambda45: Future[L45] = getLambda[L45]
  def lambda46: Future[L46] = getLambda[L46]
  def lambda47: Future[L47] = getLambda[L47]
  def lambda48: Future[L48] = getLambda[L48]
  def lambda49: Future[L49] = getLambda[L49]
  def lambda50: Future[L50] = getLambda[L50]
  def lambda51: Future[L51] = getLambda[L51]
  def lambda52: Future[L52] = getLambda[L52]
  def lambda53: Future[L53] = getLambda[L53]
  def lambda54: Future[L54] = getLambda[L54]
  def lambda55: Future[L55] = getLambda[L55]
  def lambda56: Future[L56] = getLambda[L56]
  def lambda57: Future[L57] = getLambda[L57]
  def lambda58: Future[L58] = getLambda[L58]
  def lambda59: Future[L59] = getLambda[L59]
  def lambda60: Future[L60] = getLambda[L60]


  private def getLambda[Lambda]: Future[Lambda] = {

    // post actual source code and get compilation result
    val startTime = System.currentTimeMillis()
    Ajax.post(
      url = "http://localhost:8880/compile?opt=fast",
      data = scalaCode
    ).map { res =>
      val compileTime = System.currentTimeMillis() - startTime
      println(s"----- Scala code: -----")
      println(numbered(scalaCode))
      println(s"Compilation took $compileTime ms")

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

      // Return js edit lambda and cast it back into Scala land
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
    val r           = js.JSON.parse(jsonStr).asInstanceOf[CompilationResponseJS]
    val annotations = r.annotations.map(a =>
      EditorAnnotation(a.row, a.col, a.text, a.tpe)
    )
    CompilationResponse(
      if (r.jsCode.isEmpty) None else Some(r.jsCode(0)),
      r.jsDeps,
      r.cssDeps,
      annotations,
      r.log
    )
  }

  private def numbered(code: String): String = code.split("\n").zipWithIndex.map {
    case (c, i) if i < 9  => s"  ${i + 1}  " + c
    case (c, i) if i < 99 => s" ${i + 1}  " + c
    case (c, i)           => s"${i + 1}  " + c
  }.mkString("\n")
}